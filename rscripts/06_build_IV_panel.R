# MTE dissertation
# Jesus Lopez Perez
# inspect IV database for panel data and model estimation

# Setup  ---------------------------
rm(list = ls())

library(tidyverse)
library(stringr)
library(ggplot2)
library(zoo)
library(fpp3)
library(tibble)
library(naniar)
library(kableExtra)
library(rlang)
library(tidyverse)
library(ggthemes)

path <- "D:/Personal/MTE/Tesis/Propuestas_Temas/credit-markets"

RStudioView <- View
View <- function(x) {
  name  <- deparse(substitute(x))
  if ("data.frame" %in% class(x)) { 
    RStudioView(x[1:1000,], name)
  } else { 
    RStudioView(x) 
  }
}


G7 <- TRUE
if(G7 == TRUE){
  banks <- c("040002", "040012", "040014", "040021",
             "040072", "040044", "040036")
  which_db <- "G7"
}else{
  banks = NULL
  which_db <- "ALL"
}


# catalogs
read.csv(paste(path, "/Data/cat_ents.csv", sep = ""), encoding = "UTF-8",
         col.names = c("state_code","state_long", "state_short")) %>% 
  mutate(state_code = str_pad(state_code, pad = "0", width = 2, side = "left")) ->  cat_ents

cat_ents <- cat_ents %>% 
  mutate(state_code = ifelse(state_code == 999,99,state_code))

read.csv(paste(path, "/Data/cat_eeffs.csv", sep = ""), encoding = "UTF-8",
         col.names = c("institution_code", "institution")) %>% 
  mutate(institution_code = as.character(str_pad(institution_code,
                                                 pad = "0", width = 6, side = "left"))) -> cat_eeffs

read.csv(paste(path, "/Data/cat_sectores.csv", sep = ""), encoding = "UTF-8",
         col.names = c("sector_code", "Econ_Act", "scian_code", "scian_code2",
                       "GA_code", "description")) %>% 
  mutate(sector_code = as.character(str_pad(sector_code,
                                            pad = "0", width = 2, side = "left"))) -> cat_sects

read.csv(paste(path, "/Data/cat_destino.csv", sep = ""), encoding = "UTF-8",
         col.names = c("funds_use_cnbv_code", "use_description","funds_use_code")) -> cat_funds_use

read.csv(paste(path, "/Data/cat_reporte_cnbv.csv", sep = ""), encoding = "UTF-8",
         col.names = c("cnbv_report_code","report_description",
                       "large_description")) -> cat_cnbv_report

read.csv(paste(path, "/Data/cat_firm_size.csv", sep = ""), encoding = "UTF-8",
         col.names = c("firm_size_code", "firm_description")) -> cat_firm_size

read.csv(paste(path, "/Data/cat_currency.csv", sep = ""), encoding = "UTF-8",
         col.names = c("currency_code", "currency")) -> cat_currency

# Load panel data from previous script ---------------------------
panel_data_r <- read.csv(paste(path,"/Data/proc/panel_firms_",
                  which_db,
                  "_2T_16_4T_21.csv", sep = ""),
                       fileEncoding = "UTF-8")

panel_data_r <- panel_data_r %>% 
  mutate(institution_code = 
           str_pad(institution_code,width = 6, pad = "0", 
                   side = "left")) %>% 
         #state_code = str_pad(state_code,width = 2, pad = "0", side = "left")) %>% 
  mutate(period = 
           period %>% lubridate::ymd())


# read IV data ---------------------------

data_iv <- read.csv(paste(path,
                "/Data/proc/","num_cajeros","-2016-01-01_2022-01-01.csv",sep = "")) %>% 
  mutate(
    institution_code = 
      str_pad(institution_code,width = 6, pad = "0", side = "left"),
    state_code = 
      str_pad(state_code,width = 2, pad = "0", side = "left")) %>% 
  mutate(period = period %>% lubridate::ymd()) %>% 
  select(-var)

names(data_iv)[names(data_iv) == "value"] <- "num_cajeros"


# append IV data to panel data frame (one at a time) 

# create function
import_iv_data_fun <- function(data_iv_r, my_var){
  # data_iv_r <- data_iv; my_var <- "num_est_tpv"
  data_ <- read.csv(paste(path,
                          "/Data/proc/",my_var,"-2016-01-01_2022-01-01.csv",sep = "")) %>% 
    mutate(
      institution_code = 
        str_pad(institution_code,width = 6, pad = "0", side = "left"),
      state_code = 
        str_pad(state_code,width = 2, pad = "0", side = "left")) %>% 
    mutate(period = period %>% ymd) %>% 
    left_join(cat_eeffs)
  
  data <- data_iv_r %>% left_join(data_, by = c("period" = "period",
                                                "institution_code" = "institution_code",
                                                "state_code" = "state_code")) %>% 
    select(-var, -institution)
  
  names(data)[names(data) == "value"] <- my_var
  
  return(data)
}


# iterate function 
data_iv <- import_iv_data_fun(data_iv,"num_est_tpv")
data_iv <- import_iv_data_fun(data_iv,"num_sucursales")
data_iv <- import_iv_data_fun(data_iv,"pers_institucion")
data_iv <- import_iv_data_fun(data_iv,"pers_terceros")
data_iv <- import_iv_data_fun(data_iv,"transacciones_atm")
# total staff, -institution, -terceros
data_iv$pers_total <- rowSums(data_iv[,c("pers_institucion", "pers_terceros")], 
                              na.rm=TRUE)
# rename vars to english
data_iv <- data_iv %>% 
  select(-pers_institucion, -pers_terceros) %>% 
  rename("num_atm"="num_cajeros",
         "num_est_pos"="num_est_tpv",
         "num_branches"="num_sucursales", 
         "num_transactions_atm"="transacciones_atm",
         "total_staff"="pers_total")

if(G7 == TRUE){
  data_iv <- data_iv %>% filter(institution_code %in% banks)
}


# summary statistics from IV data ----------------------
iv_vars <- c("num_atm","num_est_pos", "num_branches", 
             "num_transactions_atm","total_staff")
iv_vars2 <- c("Number of ATM",
              "Number of establishements with terminals(Point-Of-Sale)",
              "Number of branches",
              "Number of transactions in ATM",
              "Total staff (internal + outsourcing)")

# funcion para NAs
check_nas <- function(df){
  df %>%
    select_if(~sum(is.na(.)) > 0) %>%
    miss_var_summary()
}

kable(check_nas(data_iv), booktabs=T, align = 'c',
      col.names = c("Variable", "number","%"), 
      digits = 2, caption = "IV data",
      format = "latex") %>%
  kable_styling(position = "center", latex_options="HOLD_position")



summary(data_iv[,iv_vars])

length(unique(data_iv$institution_code))



plot_for_loop <- function(df, y_var) {
  df %>% left_join(cat_eeffs, 
                        by = c("institution_code" = "institution_code")) %>% 
    group_by(institution, period) %>% 
    summarise(sum_var = sum(.data[[y_var]], na.rm = TRUE)) %>% 
    ggplot(aes(x = period, y = sum_var, color = institution)) + 
    geom_line(size = 1.2, alpha = 0.8) +
    #facet_wrap(~ institution) +
    labs(x = "Date", 
         subtitle = "Period: Jan-2016 to Jan-2022",
         color = "Bank") +
    theme_fivethirtyeight() + 
    theme(axis.title = element_text()) + 
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
    scale_color_brewer(palette = "Set1") 
}

plot_list <- colnames(data_iv)[-c(1:3)] %>% 
  map( ~ plot_for_loop(data_iv, .x))

plot_list[[1]] <- plot_list[[1]] + 
  labs(title = iv_vars2[1], y = iv_vars[1])
plot_list[[2]] <- plot_list[[2]] + 
  labs(title = iv_vars2[2], y = iv_vars[2])
plot_list[[3]] <- plot_list[[3]] + 
  labs(title = iv_vars2[3], y = iv_vars[3])
plot_list[[4]] <- plot_list[[4]] + 
  labs(title = iv_vars2[4], y = iv_vars[4])
plot_list[[5]] <- plot_list[[5]] + 
  labs(title = iv_vars2[5], y = iv_vars[5])

path2 <- paste("D:/Personal/MTE/Tesis/Propuestas_Temas/",
               "credit-markets/weekly report/", sep = "")
for(i in 1:5){
ggsave(filename = paste(iv_vars[i],".png", sep = ""),
       device = "png",plot = plot_list[[i]],
       path = paste(path,"/outputs/plots/iv_plots/", sep = ""),
       width = 15, height = 10, units = "cm")
}


# # join iv data with panel ---------------------------
# panel_data_r <- panel_data_r %>% 
#   left_join(data_iv, by = c("period" = "period",
#                 "institution_code" = "institution_code",
#                 "state_code" = "state_code")) 
#   


# Export  ---------------------------
# IV data
# write.csv(data_iv, paste(path,"/proc/iv_",
#                               which_db,"_2T_16_4T_21.csv", sep = ""), 
#           row.names = FALSE)
# 
# 
# # Export data
# write.csv(panel_data_r, paste(path,
#                 "/proc/panel_firms_iv_",
#                             which_db,"_2T_16_4T_21.csv", sep = ""), 
#           row.names = FALSE)
# 


