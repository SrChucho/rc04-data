# MTE Dissertation
# Jesus Lopez Perez, ITAM
# aggregate data for panel estimation

# Setup  ---------------------------
rm(list = ls())
Sys.setlocale("LC_TIME", "C")

library(tidyverse)
library(stringr)
library(ggplot2)
library(zoo)
library(fpp3)
library(tibble)
library(xtable)
library(DescTools)


# to prevent viewing large datasets
# https://stackoverflow.com/questions/48792084/view-function-showing-all-columns-makes-r-really-slow
RStudioView <- View
View <- function(x) {
  name  <- deparse(substitute(x))
  if ("data.frame" %in% class(x)) { 
    RStudioView(x[1:1000,], name)
  } else { 
    RStudioView(x) 
  }
}

path <- "D:/Personal/MTE/Tesis/Propuestas_Temas/credit-markets/Data"

G7 <- TRUE
if(G7 == TRUE){
  banks <- c("040002", "040012", "040014", "040021",
             "040072", "040044", "040036")
  which_db <- "G7"
}else{
  banks = NULL
  which_db <- "ALL"
}

subset_term_0 <- TRUE

# catalogues
read.csv(paste(path, "/cat_ents.csv", sep = ""), encoding = "UTF-8",
         col.names = c("state_code","state_long", "state_short")) %>% 
  mutate(state_code = str_pad(state_code, pad = "0", width = 2, side = "left")) ->  cat_ents

cat_ents <- cat_ents %>% 
  mutate(state_code = ifelse(state_code == 999,99,state_code))

cat_eeffs <- read.csv(paste(path, "/cat_eeffs.csv", sep = ""), encoding = "UTF-8",
         col.names = c("institution_code", "institution", "grupo")) %>% 
  mutate(institution_code = as.character(str_pad(institution_code,
                                                 pad = "0", width = 6, side = "left"))) 

cat_sects <- read.csv(paste(path, "/cat_sectores.csv", sep = ""), encoding = "UTF-8",
         col.names = c("sector_code", "Econ_Act", "scian_code", "scian_code2", "ga_code",
                       "description")) %>% 
  mutate(sector_code = as.character(str_pad(sector_code,
                                            pad = "0", width = 2, side = "left"))) 

cat_funds_use <- read.csv(paste(path, "/cat_destino.csv", sep = ""), encoding = "UTF-8",
         col.names = c("funds_use_cnbv_code", "use_description","funds_use_code")) 

cat_cnbv_report <- read.csv(paste(path, "/cat_reporte_cnbv.csv", sep = ""), encoding = "UTF-8",
         col.names = c("cnbv_report_code","report_description",
                       "large_description"))  

cat_firm_size <- read.csv(paste(path, "/cat_firm_size.csv", sep = ""), encoding = "UTF-8",
         col.names = c("firm_size_code", "firm_description")) 

cat_currency <- read.csv(paste(path, "/cat_currency.csv", sep = ""), encoding = "UTF-8",
         col.names = c("currency_code", "currency")) 


# Load data ---------------------------
data_r_all <- read_csv(paste(path, "/proc/firms_",which_db,"_2T_16_4T_21.csv", sep = ""),
                       locale = readr::locale(encoding = "latin1"))

data_r_all <- data_r_all %>% 
  mutate(institution_code = str_pad(institution_code,width = 6, pad = "0", side = "left"),
         state_code = str_pad(state_code,width = 2, pad = "0", side = "left")) %>% 
  mutate(period = period %>% sprintf("%s01", .) %>%  lubridate::ymd()) %>% 
  mutate(funds_use_code = factor(funds_use_code))

months <- unique(data_r_all$period)

data_period <- data_r_all %>% group_by(period) %>% 
  summarise(N = n()) 

# clean data ---------------------------
clean <- function(x, include = FALSE){
  x[is.na(x)] <- include
  x
}

# cleaning interest rates
N <- nrow(data_r_all)
n_orig <- nrow(data_r_all)
data_r_all$log_int_rate_d <- log(data_r_all$interest_rate_disposition)
data_r_all$log_int_rate_a <- log(data_r_all$interest_rate_all)
data_r_all <- with(data_r_all, data_r_all[clean(log_int_rate_d >=0, include = TRUE),])
data_r_all <- with(data_r_all, data_r_all[clean(log_int_rate_a >=0, include = TRUE),])
data_r_all <- with(data_r_all, data_r_all[clean( !is.infinite(interest_rate_all), include = TRUE),])
data_r_all <- with(data_r_all, data_r_all[clean( !is.infinite(interest_rate_disposition), include = TRUE),])
n_clean1 <- nrow(data_r_all)
n_orig - n_clean1; 100* (n_orig - n_clean1) / n_orig 
n_removed_int <- n_orig - n_clean1

# cleaning term
data_r_all <- with(data_r_all, 
                   data_r_all[clean(term_disposition >=0, 
                                    include = TRUE),])
data_r_all <- with(data_r_all, 
                   data_r_all[clean(term_all >=0, 
                                    include = TRUE),])
data_r_all <- with(data_r_all, 
                   data_r_all[clean( !is.infinite(term_all), 
                                     include = TRUE),])

# 98.9% of the database has a maturity of at most 36 months (3years)
# we keep only less than 3y
data_r_all %>% 
  mutate(term_y = 
           case_when(term_disposition <= 12 ~ "1",
                     term_disposition > 12 & term_disposition <= 24 ~ "2",
                     term_disposition > 24 & term_disposition <= 36 ~ "3",
                     term_disposition > 36 ~ "3+")) %>% 
  group_by(term_y) %>% 
  summarise(N = n()) %>% 
  mutate(pct = 100* N /sum(N))
data_r_all <- with(data_r_all, 
                   data_r_all[clean( !(term_disposition > 36), 
                                     include = TRUE),])


data_r_all %>% 
  mutate(term_y = 
           case_when(term_disposition <= 1 ~ "1",
                     term_disposition > 1 & term_disposition <= 3 ~ "2",
                     term_disposition > 3 & term_disposition <= 6 ~ "3",
                     term_disposition > 6 & term_disposition <= 12 ~ "4",
                     term_disposition > 12 & term_disposition <= 24 ~ "5",
                     term_disposition > 36 ~ "6")) %>% 
  group_by(term_y) %>% 
  summarise(N = n()) %>% 
  mutate(pct = 100* N /sum(N)) 



n_clean2 <- nrow(data_r_all)
n_orig  - (n_clean2); 100* (n_orig - (n_clean2)) / n_orig 
n_clean2


# cleaning NA in past_due_pct, set = 0 if past_due_portfolio = 0
data_r_all[is.na(data_r_all$past_due_pct),"past_due_pct"] <- 0

# cleaning total_portfolio = 0, set = loan_disposition_month
data_r_all[data_r_all$total_portfolio == 0,"total_portfolio"] <- 
  data_r_all[data_r_all$total_portfolio == 0,"loan_disposition_month"]

# check
nrow(data_r_all)


# summary statistics ---------------------------

table_id_count <- 
  data_r_all %>% 
  group_by(firm_id) %>% 
  summarise(N = n(),
            sum_disposition = sum(loan_disposition_month)) %>% 
  mutate(N_rep = case_when((N > 0 & N <= 10) ~ "1 to 10 obs",
                           (N >10 & N <= 20) ~ "11 to 20 obs",
                           (N >20 & N <= 30) ~ "21 to 30 obs",
                           (N >30 & N <= 40) ~ "31 to 40 obs",
                           (N >40 & N <= 50) ~ "41 to 50 obs",
                           (N >50 & N <= 60) ~ "51 to 60 obs",
                           (N >60 & N <= 68) ~ "61 to 68 obs",
                           N > 68 ~ "More than 68 obs")) %>% 
  mutate(N_rep = fct_relevel(N_rep,"1 to 10 obs",
                             "11 to 20 obs","21 to 30 obs",
                             "31 to 40 obs","41 to 50 obs",
                             "51 to 60 obs","61 to 68 obs",
                             "More than 68 obs", "Total")) %>% 
  group_by(N_rep) %>% 
  summarise(firms = n(),
            count = sum(N), 
            sum_disposition = sum(sum_disposition) / 1000000) %>% 
  mutate('% firms' = round(100 * firms / sum(firms),2),
         '% count' = round(100* count / sum(count),2),
         '% amount' = round(100*sum_disposition / sum(sum_disposition),2)) %>% 
  select(N_rep, firms, `% firms`,
         count, `% count`,
         sum_disposition, `% amount`)

# sense check
nrow(data_r_all) - sum(table_id_count$count)
sum(table_id_count$`% amount`)

total <- c("", 
           sum(table_id_count$firms), 100,
           sum(table_id_count$count), 100,
           sum(table_id_count$sum_disposition),100)

table_id_count <- rbind(table_id_count,total)
table_id_count$N_rep <- as.character(table_id_count$N_rep)
table_id_count[9,"N_rep"] <- "Total"
length(unique(data_r_all$firm_id))

# format for thousand separator
table_id_count$firms  <-
  format(as.numeric(table_id_count$firms),big.mark=",")

table_id_count$count  <-
  format(as.numeric(table_id_count$count),big.mark=",")

table_id_count$sum_disposition <-
  format(as.numeric(table_id_count$sum_disposition),big.mark=",")

# export to LaTeX
table_id_count_x <- xtable(table_id_count, align = "lrrrrrrr",
                 latex.environments="center", 
                 format.args = list(digits = 2))

print(xtable(table_id_count_x), include.rownames=FALSE)


#print(table_id_count_x,
#      type = "latex", include.rownames=FALSE, file = "DF.TAB.html")

# IV data ---------------------
import_iv_data_fun <- function(data_iv_r, my_var){
  # data_iv_r <- data_iv; my_var <- "num_est_tpv"
  data_ <- read.csv(paste(path,
                          "/proc/",my_var,"-2016-01-01_2022-01-01.csv",sep = "")) %>% 
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

data_iv <- read.csv(paste(path,
                          "/proc/","num_cajeros","-2016-01-01_2022-01-01.csv",sep = "")) %>% 
  mutate(
    institution_code = 
      str_pad(institution_code,width = 6, pad = "0", side = "left"),
    state_code = 
      str_pad(state_code,width = 2, pad = "0", side = "left")) %>% 
  mutate(period = period %>% lubridate::ymd()) %>% 
  select(-var)

names(data_iv)[names(data_iv) == "value"] <- "num_cajeros"

# iterate IV function 
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


# join iv data with panel ---------------------------
data_r_all <- data_r_all %>% 
  left_join(data_iv, by = c("period" = "period",
                            "institution_code" = "institution_code",
                            "state_code" = "state_code")) 

# subset term = 0 ----------------------

# subset data
if(subset_term_0 == TRUE){
  data_r_all <- data_r_all %>% filter(term_disposition <1 ) 
  which_term <- "term_u_1"
}else{
  data_r_all <- data_r_all %>% filter(term_disposition >= 1 ) 
  which_term <- "term_geq_1"
}


# aggregate data for panel ---------------------------


data_panel <- data_r_all %>% # data set
  group_by(period, institution_code, # grouping vars
           sector_code) %>% 
  summarise(N = n(), # aggregated data
              sum_loans = sum(number_of_loans),
              sum_disposition = sum(loan_disposition_month),
              mean_disposition = mean(loan_disposition_month, na.rm = TRUE),
              sum_past_due = sum(past_due_portfolio),
              sum_total = sum(total_portfolio),
              wa_interest_rate_d = sum(interest_rate_disposition * loan_disposition_month, na.rm = TRUE) / sum(loan_disposition_month, na.rm = TRUE),
              wa_term_d = sum(term_disposition  * loan_disposition_month, na.rm = TRUE) / sum(loan_disposition_month,na.rm = TRUE),
              wa_past_due_pct = 100*sum(sum_past_due) / sum(sum_total),
              wa_atm = sum(num_atm * loan_disposition_month, na.rm = TRUE) / sum(loan_disposition_month,na.rm = TRUE),
            wa_est_pos = sum(num_est_pos * loan_disposition_month, na.rm = TRUE) / sum(loan_disposition_month,na.rm = TRUE),
            wa_branches = sum(num_branches * loan_disposition_month, na.rm = TRUE) / sum(loan_disposition_month,na.rm = TRUE),
            wa_transactions_atm = sum(num_transactions_atm * loan_disposition_month, na.rm = TRUE) / sum(loan_disposition_month,na.rm = TRUE),
            wa_total_staff = sum(total_staff * loan_disposition_month, na.rm = TRUE) / sum(loan_disposition_month,na.rm = TRUE)
            ) %>% 
  select(sector_code, period, everything()) %>% # order columns
  data.frame() # convert to data frame


temp <- sapply(1:ncol(data_panel),
               function(x) is.na(data_panel[,x]))
n_nas <- colSums(temp)
names(n_nas) <- colnames(data_panel); n_nas

data_period$N_panel <- data_panel %>% group_by(period) %>% 
    summarise(N = n()) %>% pull(N)
  


# creating dummies --------------------
# create dummies function
clase_mat <- function(X, clase_var){
  # X <- panel_data_r; clase_var <- "institution_code"
  X <- data.frame(X)
  X_c <- X[,clase_var]
  
  X_d <- matrix(0, length(X_c), length(unique(X_c)))
  colnames(X_d) <- unique(X_c)
  for(class_i in unique(X_c))
    X_d[which(X_c == class_i), class_i] <- 1
  
  return(X_d)
}

# create dummies for institution
dum_inst <- clase_mat(data_panel,"institution_code")
inst <- cat_eeffs[cat_eeffs[,"institution_code"] %in% colnames(dum_inst),"institution"]
colnames(dum_inst) <- inst
data_panel <- cbind(data_panel, dum_inst)

# create dummies for sector_code
dum_sect <- clase_mat(data_panel,"sector_code")
colnames(dum_sect) <- paste("S",colnames(dum_sect), sep = "")
data_panel <- cbind(data_panel, dum_sect)
sect_names <- colnames(dum_sect)[order(colnames(dum_sect))]


# Export panel  ---------------------------

write.csv(data_panel, paste(path,"/proc/panel_firms_",
                            which_db,"_",which_term,"_2T_16_4T_21.csv", sep = ""), 
          row.names = FALSE)


# esto ya no es relevante # 20220714
# # Export only unique firms (not pseudo-panel)
# 
# table(data_panel$N)
# 
# data_panel_res <- data_panel %>% 
#   filter(N == 1) 
# 
# write.csv(data_panel_res, paste(path,"/proc/panel_firms_res_",
#                             which_db,"_2T_16_4T_21.csv", sep = ""), 
#           row.names = FALSE)


# not run  ---------------------------


data_r_all %>% group_by(period) %>% 
  summarise(sum_loans_disposition = log(sum(loan_disposition_month))) %>% 
  ggplot(aes(x = factor(period), y = sum_loans_disposition)) + 
  geom_point(stat = "identity") + 
  ylim(c(20,30)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))


# reporte mirando a 
# https://picomponentebi.cnbv.gob.mx/ReportViwer/ReportService?sector=40&tema=2&subTema=3&tipoInformacion=0&subTipoInformacion=0&idReporte=040_11a_BIPER0&idPortafolio=0&idTipoReporteBI=1
  
data_r_all %>% filter(period == "202111") %>% 
  group_by(institution) %>% 
  summarise(N = n(),
            N_number_of_rfc  = sum(number_of_rfc),
            N_number_of_loans  = sum(number_of_loans),
            sum_loan_amount  = sum(loan_amount)/1000000,
            avg_interest_rate = sum(interest_rate * loan_amount, na.rm = T) / sum(loan_amount),
            avg_arrears = sum(arrears_pct*loan_amount) / sum(loan_amount)) %>% 
  mutate(pct_rfc = 100*N_number_of_rfc / sum(N_number_of_rfc),
         pct_num_of_loans = 100*N_number_of_loans /sum(N_number_of_loans),
        pct_amount = 100 * sum_loan_amount / sum(sum_loan_amount)) %>%  
  arrange(desc(pct_amount)) -> table_1 # %>%  pull(N) %>% sum()






#### not used table
# summary statistics ---------------------------
panel_id_count <- data_panel %>% group_by(sector_code) %>% 
  summarise(N = n(),
            sum_disposition = sum(sum_disposition )) %>% 
  mutate(N_rep = case_when((N > 0 & N <= 10) ~ "1 to 10 obs",
                           (N >10 & N <= 20) ~ "11 to 20 obs",
                           (N >20 & N <= 30) ~ "21 to 30 obs",
                           (N >30 & N <= 40) ~ "31 to 40 obs",
                           (N >40 & N <= 50) ~ "41 to 50 obs",
                           (N >50 & N <= 60) ~ "51 to 60 obs",
                           (N >60 & N <= 68) ~ "61 to 68 obs",
                           N > 68 ~ "More than 68 obs")) %>% 
  mutate(N_rep = fct_relevel(N_rep,"1 to 10 obs",
                             "11 to 20 obs","21 to 30 obs",
                             "31 to 40 obs","41 to 50 obs",
                             "51 to 60 obs","61 to 68 obs",
                             "More than 68 obs", "Total") )%>% 
  group_by(N_rep) %>% 
  summarise(firms_group = n(),
            count = sum(N), 
            sum_disposition = sum(sum_disposition) / 1000000) %>% 
  mutate('% firms_group' = round(100 * firms_group / sum(firms_group),2),
         '% count' = round(100* count / sum(count),2),
         '% amount' = round(100*sum_disposition / sum(sum_disposition),2) ) %>% 
  select(N_rep, firms_group, `% firms_group`,
         count, `% count`,
         sum_disposition, `% amount`)

# sense check
nrow(data_panel) - sum(panel_id_count$count)
sum(panel_id_count$`% amount`)

total <- c("", 
           sum(panel_id_count$firms_group), 100,
           sum(panel_id_count$count), 100,
           sum(panel_id_count$sum_disposition),100)

panel_id_count <- rbind(panel_id_count,total)
panel_id_count$N_rep <- as.character(panel_id_count$N_rep)
panel_id_count[8,"N_rep"] <- "Total"
length(unique(data_panel$ID))

# format for thousand separator
panel_id_count$firms_group  <-
  format(as.numeric(panel_id_count$firms_group),big.mark=",")

panel_id_count$count  <-
  format(as.numeric(panel_id_count$count),big.mark=",")

panel_id_count$sum_disposition <-
  format(as.numeric(panel_id_count$sum_disposition),big.mark=",")

# export to LaTeX
panel_id_count_x <- xtable(panel_id_count, align = "lrrrrrrr",
                           latex.environments="center", 
                           format.args = list(digits = 2))

print(xtable(panel_id_count_x), include.rownames=FALSE)



data_period %>% pivot_longer(cols = -period)  %>% 
  ggplot(aes(x = factor(period), y = value, fill = name)) +
  geom_bar(stat = "identity", position = "dodge") + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))

head(data_panel)

