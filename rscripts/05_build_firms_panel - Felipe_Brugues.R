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
library(forecast)
library(dplyr)
library(imputeTS)
library(plm)

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

read.csv(paste(path, "/cat_eeffs.csv", sep = ""), encoding = "UTF-8",
         col.names = c("institution_code", "institution", "grupo")) %>% 
  mutate(institution_code = as.character(str_pad(institution_code,
                                                 pad = "0", width = 6, side = "left"))) -> cat_eeffs

read.csv(paste(path, "/cat_sectores.csv", sep = ""), encoding = "UTF-8",
         col.names = c("sector_code", "Econ_Act", "scian_code", "scian_code2",
                       "ga_code","description")) %>% 
  mutate(sector_code = as.character(str_pad(sector_code,
                                            pad = "0", width = 2, side = "left"))) -> cat_sects

read.csv(paste(path, "/cat_destino.csv", sep = ""), encoding = "UTF-8",
         col.names = c("funds_use_cnbv_code", "use_description",
                       "funds_use_code", "funds_use_code2")) -> cat_funds_use

read.csv(paste(path, "/cat_reporte_cnbv.csv", sep = ""), encoding = "UTF-8",
         col.names = c("cnbv_report_code","report_description",
                       "large_description")) -> cat_cnbv_report

read.csv(paste(path, "/cat_firm_size.csv", sep = ""), encoding = "UTF-8",
         col.names = c("firm_size_code", "firm_description")) -> cat_firm_size

read.csv(paste(path, "/cat_currency.csv", sep = ""), encoding = "UTF-8",
         col.names = c("currency_code", "currency")) -> cat_currency


# Load data ---------------------------
data_r_all <- read_csv(paste(path, "/proc/firms_",which_db,"_2T_16_4T_21.csv", sep = ""),
                       locale = readr::locale(encoding = "UTF-8"))

data_r_all <- data_r_all %>% 
  mutate(institution_code = str_pad(institution_code,width = 6, pad = "0", side = "left"),
         state_code = str_pad(state_code,width = 2, pad = "0", side = "left")) %>% 
  mutate(period = period %>% sprintf("%s01", .) %>%  lubridate::ymd()) %>% 
  mutate(funds_use_code = factor(funds_use_code))

months <- unique(data_r_all$period)

data_period <- data_r_all %>% group_by(period) %>% 
  summarise(N = n()) 

# load INPP
inpp <- read.csv(paste(path, "/inpp/INPP_ene2003_ago2022.csv",
                       sep = ""))
inpp <- inpp %>% 
  mutate(Fecha = Fecha %>% lubridate::ymd() )

# load INPC
inpc <- read.csv(paste(path, "/inpc/inpc_2015_2022.csv",
                       sep = ""))
inpc <- inpc %>% 
  mutate(Fecha = Fecha %>% lubridate::ymd() )


cpm <- function(x, q = 1){
  index <- matrix(0, length(x) - q, 2)
  index[,1] <- 1:nrow(index)
  index[,2] <- (q+1):(nrow(index)+q)
  
  xm <- rep(0, nrow(index))
  for(i in 1 : length(xm))
    xm[i] <- x[index[i,2]]/x[index[i,1]]*100-100
  
  return(xm)
}

inpp_inflacion <- data.frame(apply(inpp[,-1], 2, function(x) cpm(x,12)))
inpp_inflacion$Fecha <- NA
inpp_inflacion$Fecha <- inpp$Fecha[-c(1:12)]


inpc_inflacion <- data.frame(cpm(inpc[,-1], 12))
inpc_inflacion$Fecha <- NA
inpc_inflacion$Fecha <- inpc$Fecha[-c(1:12)]
colnames(inpc_inflacion) <- c("inflation","Fecha")

inpc <- inpc %>% 
  left_join(inpc_inflacion)

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
n_removed_int <- n_orig - n_clean1; n_removed_int
100* (n_orig - n_clean1) / n_orig 


# cleaning term
n_orig_2 <- nrow(data_r_all)
data_r_all <- with(data_r_all, 
                   data_r_all[clean(term_disposition >0, 
                                    include = TRUE),])
# data_r_all <- with(data_r_all, 
#                    data_r_all[clean(term_all >0, 
#                                     include = TRUE),])
data_r_all <- with(data_r_all, 
                   data_r_all[clean( !is.infinite(term_all), 
                                     include = TRUE),])
n_clean2 <- nrow(data_r_all)
n_removed_int2<- n_orig_2 - n_clean2; n_removed_int2
100* (n_orig_2 - n_clean2) / n_orig 


# 98.9% of the database has a maturity of at most 36 months (3years)
# we keep only less than 3y
n_orig_3 <- nrow(data_r_all)
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



n_clean3 <- nrow(data_r_all)
n_removed_int3 <- n_orig_3 - n_clean3; n_removed_int3
100* (n_orig_3 - n_clean3) / n_orig 


# cleaning NA in past_due_pct, set = 0 if past_due_portfolio = 0
data_r_all[is.na(data_r_all$past_due_pct),"past_due_pct"] <- 0

# cleaning total_portfolio = 0, set = loan_disposition_month
data_r_all[data_r_all$total_portfolio == 0,"total_portfolio"] <- 
  data_r_all[data_r_all$total_portfolio == 0,"loan_disposition_month"]

# check
nrow(data_r_all)


# cleaning fideicomisos / keep only SME
n_orig_4 <- nrow(data_r_all)
data_r_all <- with(data_r_all, 
                   data_r_all[clean(!cnbv_report_code %in% c("R469","R484"), 
                                    include = TRUE),])
n_clean3 <- nrow(data_r_all)
n_removed_int3<- n_orig_4 - n_clean3; n_removed_int3

100 *( n_removed_int3 / n_orig)

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
    dplyr::select(-var, -institution)
  
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
  dplyr::select(-var)

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
  dplyr::select(-pers_institucion, -pers_terceros) %>% 
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

data_iv_inst <- data_iv %>% 
  group_by(period, institution_code) %>% 
  summarise(num_atm = sum(num_atm, na.rm = TRUE), 
            num_est_pos = sum(num_est_pos, na.rm = TRUE),
            num_branches = sum(num_branches, na.rm = TRUE),
            num_transactions_atm = sum(num_transactions_atm, na.rm = TRUE),
            total_staff = sum(total_staff, na.rm = TRUE)) %>% 
  ungroup()

# subset term = 0 ----------------------
# not necessary due to aggregation 20220815
# # subset data
# if(subset_term_0 == TRUE){
#   data_r_all <- data_r_all %>% filter(term_disposition <1 ) 
#   which_term <- "term_u_1"
# }else{
#   data_r_all <- data_r_all %>% filter(term_disposition >= 1 ) 
#   which_term <- "term_geq_1"
# }


# aggregate data for panel ---------------------------


data_panel_orig <- data_r_all %>% # data set
  group_by(period, institution_code, # grouping vars
           scian_code) %>% 
  summarise(N = n(), # aggregated data
            sum_loans = sum(number_of_loans),
            sum_disposition = sum(loan_disposition_month),
            mean_disposition = mean(loan_disposition_month, na.rm = TRUE),
            sum_past_due = sum(past_due_portfolio),
            sum_total = sum(total_portfolio),
            wa_interest_rate_d = sum(interest_rate_disposition * loan_disposition_month, na.rm = TRUE) / sum(loan_disposition_month, na.rm = TRUE),
            wa_term_d = sum(term_disposition  * loan_disposition_month, na.rm = TRUE) / sum(loan_disposition_month,na.rm = TRUE),
            wa_past_due_pct = 100*sum(sum_past_due) / sum(sum_total)
            #mean_atm = mean(num_atm, na.rm = TRUE),
            #mean_est_pos = mean(num_est_pos, na.rm = TRUE),
            #mean_branches = mean(num_branches, na.rm = TRUE),
            #mean_transactions_atm = mean(num_transactions_atm, na.rm = TRUE),
            #mean_total_staff = mean(total_staff, na.rm = TRUE),
            #wa_atm = sum(num_atm * loan_disposition_month, na.rm = TRUE) / sum(loan_disposition_month,na.rm = TRUE),
            #wa_est_pos = sum(num_est_pos * loan_disposition_month, na.rm = TRUE) / sum(loan_disposition_month,na.rm = TRUE),
            #wa_branches = sum(num_branches * loan_disposition_month, na.rm = TRUE) / sum(loan_disposition_month,na.rm = TRUE),
            #wa_transactions_atm = sum(num_transactions_atm * loan_disposition_month, na.rm = TRUE) / sum(loan_disposition_month,na.rm = TRUE),
            #wa_total_staff = sum(total_staff * loan_disposition_month, na.rm = TRUE) / sum(loan_disposition_month,na.rm = TRUE)
  ) %>% 
  dplyr::select(scian_code, period, everything()) %>% # order columns
  data.frame() # convert to data frame




data_panel <- data_r_all %>% # data set
  group_by(period, institution_code, # grouping vars
           scian_code, state_code) %>% 
  summarise(N = n(), # aggregated data
              sum_loans = sum(number_of_loans),
              sum_disposition = sum(loan_disposition_month),
              mean_disposition = mean(loan_disposition_month, na.rm = TRUE),
              sum_past_due = sum(past_due_portfolio),
              sum_total = sum(total_portfolio),
              wa_interest_rate_d = sum(interest_rate_disposition * loan_disposition_month, na.rm = TRUE) / sum(loan_disposition_month, na.rm = TRUE),
              wa_term_d = sum(term_disposition  * loan_disposition_month, na.rm = TRUE) / sum(loan_disposition_month,na.rm = TRUE),
              wa_past_due_pct = 100*sum(sum_past_due) / sum(sum_total)
            # mean_atm = mean(num_atm, na.rm = TRUE),
            # mean_est_pos = mean(num_est_pos, na.rm = TRUE),
            # mean_branches = mean(num_branches, na.rm = TRUE),
            # mean_transactions_atm = mean(num_transactions_atm, na.rm = TRUE),
            # mean_total_staff = mean(total_staff, na.rm = TRUE),
            # wa_atm = sum(num_atm * loan_disposition_month, na.rm = TRUE) / sum(loan_disposition_month,na.rm = TRUE),
            # wa_est_pos = sum(num_est_pos * loan_disposition_month, na.rm = TRUE) / sum(loan_disposition_month,na.rm = TRUE),
            # wa_branches = sum(num_branches * loan_disposition_month, na.rm = TRUE) / sum(loan_disposition_month,na.rm = TRUE),
            # wa_transactions_atm = sum(num_transactions_atm * loan_disposition_month, na.rm = TRUE) / sum(loan_disposition_month,na.rm = TRUE),
            # wa_total_staff = sum(total_staff * loan_disposition_month, na.rm = TRUE) / sum(loan_disposition_month,na.rm = TRUE)
            ) %>% 
  dplyr::select(scian_code, period, everything()) %>% # order columns
  data.frame() # convert to data frame

# we add sum of IV (vs wa or mean)
data_panel <- data_panel %>% 
  left_join(data_iv_inst,
            by = c("period" = "period",
          "institution_code" = "institution_code")) 


# looking at the data
# data_panel %>% 
#   ggplot(aes(x = (mean_disposition), group = institution_code)) + 
#   geom_histogram() + 
#   facet_wrap(~institution_code)
# 

temp <- sapply(1:ncol(data_panel),
               function(x) is.na(data_panel[,x]))
n_nas <- colSums(temp)
names(n_nas) <- colnames(data_panel); n_nas

data_period$N_panel <- data_panel %>% group_by(period) %>% 
    summarise(N = n()) %>% pull(N)
  


######### creating dummies --------------------
# create dummies function
clase_mat <- function(X, clase_var){
  # X <- data_panel ; clase_var <- "year"
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
dum_sect <- clase_mat(data_panel,"scian_code")
colnames(dum_sect) <- paste("S",colnames(dum_sect), sep = "")
data_panel <- cbind(data_panel, dum_sect)
sect_names <- colnames(dum_sect)[order(colnames(dum_sect))]

# create dummies for year

clase_mat <- function(X, clase_var){
  # X <- data_panel ; clase_var <- "year"
  X <- data.frame(X)
  X_c <- X[,clase_var]
  
  X_d <- matrix(0, length(X_c), length(unique(X_c)))
  colnames(X_d) <- unique(X_c)
  for(class_i in unique(X_c))
    X_d[which(X_c == class_i), as.character(class_i)] <- 1
  
  return(X_d)
}

data_panel$year <- year(data_panel$period)
dum_anio <- clase_mat(data_panel,"year")
colnames(dum_anio) <- paste("A",colnames(dum_anio), sep = "")
data_panel <- cbind(data_panel, dum_anio)
anios_names <- colnames(dum_anio)[order(colnames(dum_anio))]

# convert to real terms ------

data_panel2 <- data_panel %>% left_join(inpp, by = c("period" = "Fecha")) %>% 
  mutate(mean_disposition_r = 100 * mean_disposition / INPP_SP) 

# real balances by industry  
deflactor_adhoc <- data_panel2 %>% 
  mutate(inpp_adhoc = case_when(scian_code == 11 ~ S11.y,
                                scian_code == 21 ~ S21.y,
                                scian_code == 23 ~ S23.y,
                                scian_code == "31-33" ~ S31.S33,
                                scian_code == "43-46" ~ GA3, # comodin
                                scian_code == "48-49" ~ S48.S49,
                                scian_code == 51 ~ S51.y,
                                scian_code == 52 ~ GA3, # comodin
                                scian_code == 54 ~ S54.y,
                                scian_code == 61 ~ S61.y,
                                scian_code == 62 ~ S62.y,
                                scian_code == 71 ~ S71.y,
                                scian_code == 72 ~ S72.y,
                                scian_code == 93 ~ GA3 # comodin
                                ) 
         ) %>% 
  pull(inpp_adhoc)

# real balances by total producer's price  
deflactor_inpp <- data_panel2 %>% 
  pull(INPP_SP) 


##  real interest rates
data_panel2 <- data_panel %>% 
  left_join(inpp_inflacion, by = c("period" = "Fecha")) 

# real interest rates by sector inflation
inflacion_adhoc <- data_panel2 %>% 
  mutate(inflation_adhoc = case_when(scian_code == 11 ~ S11.y,
                                     scian_code == 21 ~ S21.y,
                                     scian_code == 23 ~ S23.y,
                                     scian_code == "31-33" ~ S31.S33,
                                     scian_code == "43-46" ~ GA3, # comodin
                                     scian_code == "48-49" ~ S48.S49,
                                     scian_code == 51 ~ S51.y,
                                     scian_code == 52 ~ GA3, # comodin
                                     scian_code == 54 ~ S54.y,
                                     scian_code == 61 ~ S61.y,
                                     scian_code == 62 ~ S62.y,
                                     scian_code == 71 ~ S71.y,
                                     scian_code == 72 ~ S72.y,
                                     scian_code == 93 ~ GA3 # comodin
  ) 
  ) %>% 
  pull(inflation_adhoc)

# real interest rates by overall producer's inflation
inflacion_inpp <- data_panel2 %>% 
  pull(INPP_SP) 

# add the variables to the panel
data_panel$deflactor_adhoc <- deflactor_adhoc
data_panel$deflactor_inpp <- deflactor_inpp

data_panel$inflacion_adhoc <- inflacion_adhoc
data_panel$inflacion_inpp <- inflacion_inpp


# panel data with adhoc data
data_panel_real_adhoc <- data_panel %>% 
  mutate(
    sum_disposition_r = 100*sum_disposition/deflactor_adhoc,
    mean_disposition_r = 100*mean_disposition/deflactor_adhoc,
    sum_past_due_r = 100*sum_past_due/deflactor_adhoc,
    sum_total_r = 100*sum_total/deflactor_adhoc,
    wa_interest_rate_d_r = wa_interest_rate_d - inflacion_adhoc,
    wa_interest_rate_d_r_fisher = 100*((1+wa_interest_rate_d/100)/(1+inflacion_adhoc/100)-1),
    log_wa_interest_rate_d_r = log(wa_interest_rate_d_r/100),
    log1p_wa_interest_rate_d_r = log1p(wa_interest_rate_d_r/100),
    asinh_wa_int_rate_d_r = asinh(wa_interest_rate_d_r),
    log_wa_interest_rate_d_r_f = log(wa_interest_rate_d_r_fisher/100),
    log1p_wa_interest_rate_d_r_f = log1p(wa_interest_rate_d_r_fisher/100),
    asinh_wa_int_rate_d_r_f = asinh(wa_interest_rate_d_r_fisher),
    )

# 13% negatives
sum(data_panel_real_adhoc$wa_interest_rate_d_r_fisher<=0)/length(data_panel_real_adhoc$wa_interest_rate_d_r)
sum(data_panel_real_adhoc$wa_interest_rate_d_r<=0)/length(data_panel_real_adhoc$wa_interest_rate_d_r)

# panel data with overall inpp
data_panel_real_inpp <- data_panel %>% 
  mutate(
    sum_disposition_r = 100*sum_disposition/deflactor_inpp,
    mean_disposition_r = 100*mean_disposition/deflactor_inpp,
    sum_past_due_r = 100*sum_past_due/deflactor_inpp,
    sum_total_r = 100*sum_total/deflactor_inpp,
    wa_interest_rate_d_r = wa_interest_rate_d - inflacion_inpp,
    wa_interest_rate_d_r_fisher = 100*((1+wa_interest_rate_d/100)/(1+inflacion_inpp/100)-1),
    log_wa_interest_rate_d_r = log(wa_interest_rate_d_r/100),
    log1p_wa_interest_rate_d_r = log1p(wa_interest_rate_d_r/100),
    asinh_wa_int_rate_d_r = asinh(wa_interest_rate_d_r),
    log_wa_interest_rate_d_r_f = log(wa_interest_rate_d_r_fisher/100),
    log1p_wa_interest_rate_d_r_f = log1p(wa_interest_rate_d_r_fisher/100),
    asinh_wa_int_rate_d_r_f = asinh(wa_interest_rate_d_r_fisher)
    )

# 11% negatives
sum(data_panel_real_inpp$wa_interest_rate_d_r_fisher<=0)/length(data_panel_real_adhoc$wa_interest_rate_d_r)
sum(data_panel_real_inpp$wa_interest_rate_d_r<=0)/length(data_panel_real_adhoc$wa_interest_rate_d_r)



# plot
x11()
opp <- par(mfrow = c(4,2))
# sector specific 
plot(density(data_panel_real_adhoc$wa_interest_rate_d_r_fisher),
     main = "Real i with sector specific INPP",xlab = "", ylab = "density",
     col = 4)
lines(density(data_panel_real_adhoc$wa_interest_rate_d_r), col = 2)
legend("right",legend = c("Fisher","Normal"), col = c(4,2),
       horiz = FALSE,bty = "n", lty = c(1,1))
abline(v = 0,lty = 2)
# overall inpp
plot(density(data_panel_real_inpp$wa_interest_rate_d_r_fisher),
     main = "Real i with overall INPP",xlab = "", ylab = "density",
     col = 4)
lines(density(data_panel_real_inpp$wa_interest_rate_d_r), col = 2)
legend("right",legend = c("Fisher","Normal"), col = c(4,2),
       horiz = FALSE,bty = "n", lty = c(1,1))
abline(v = 0,lty = 2)
# transformations
#log sector specific
plot(density(
  na_remove(data_panel_real_adhoc$log_wa_interest_rate_d_r_f)),
     main = "log Real i with sector specific INPP",xlab = "", ylab = "density",
     col = 4)
lines(density(
  na_remove(data_panel_real_adhoc$log_wa_interest_rate_d_r)),
  col = 2)
legend("right",legend = c("Fisher","Normal"), col = c(4,2),
       horiz = FALSE,bty = "n", lty = c(1,1))
abline(v = 0,lty = 2)
# log overall inpp
plot(density(
  na_remove(data_panel_real_inpp$log_wa_interest_rate_d_r_f)),
  main = "log Real i with overall INPP",xlab = "", ylab = "density",
  col = 4)
lines(density(
  na_remove(data_panel_real_inpp$log_wa_interest_rate_d_r)),
  col = 2)
legend("right",legend = c("Fisher","Normal"), col = c(4,2),
       horiz = FALSE,bty = "n", lty = c(1,1))
abline(v = 0,lty = 2)
# asinh sector specific
plot(density(data_panel_real_adhoc$asinh_wa_int_rate_d_r_f),
     main = "asinh real i with sector specific INPP",xlab = "", ylab = "density",
     col = 4)
lines(density(data_panel_real_adhoc$asinh_wa_int_rate_d_r), col = 2)
legend("right",legend = c("Fisher","Normal"), col = c(4,2),
       horiz = FALSE,bty = "n", lty = c(1,1))
abline(v = 0,lty = 2)
# asinh overall inpp
plot(density(data_panel_real_inpp$asinh_wa_int_rate_d_r_f),
     main = "asinh real i with overall INPP",xlab = "", ylab = "density",
     col = 4)
lines(density(data_panel_real_inpp$asinh_wa_int_rate_d_r), col = 2)
legend("right",legend = c("Fisher","Normal"), col = c(4,2),
       horiz = FALSE,bty = "n", lty = c(1,1))
abline(v = 0,lty = 2)
#log1p sector specific
plot(density(
  na_remove(data_panel_real_adhoc$log1p_wa_interest_rate_d_r_f)),
  main = "log1p Real i with sector specific INPP",xlab = "", ylab = "density",
  col = 4)
lines(density(
  na_remove(data_panel_real_adhoc$log1p_wa_interest_rate_d_r)),
  col = 2)
legend("right",legend = c("Fisher","Normal"), col = c(4,2),
       horiz = FALSE,bty = "n", lty = c(1,1))
abline(v = 0,lty = 2)
#log1p overall inpp
plot(density(
  na_remove(data_panel_real_inpp$log1p_wa_interest_rate_d_r_f)),
  main = "log1p Real i with sector specific INPP",xlab = "", ylab = "density",
  col = 4)
lines(density(
  na_remove(data_panel_real_inpp$log1p_wa_interest_rate_d_r)),
  col = 2)
legend("right",legend = c("Fisher","Normal"), col = c(4,2),
       horiz = FALSE,bty = "n", lty = c(1,1))
abline(v = 0,lty = 2)
par(opp)



###### "out of context" INPP graphs  -------
tabla_inflacion_sector <- inpp_inflacion %>% 
  mutate(anio = year(Fecha)) %>% 
  group_by(anio) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  filter(anio > 2015) %>% 
  dplyr::select(anio, INPP_SP, INPP, S11,S21,S23,`S31.S33`, 
         S48.S49,S51,S54, S61, S62, S71, S72 )
# plot 
inpp_inflacion %>% 
  mutate(anio = year(Fecha)) %>% 
  filter(anio > 2015 & anio < 2022) %>% 
  dplyr::select(INPP_SP, INPP, S11,S21,S23,`S31.S33`, 
         S48.S49,S51,S54, S61, S62, S71, S72) %>% 
  pivot_longer(everything(), 
               names_to = c("sector"), 
               values_to = "value" ) %>% 
  ggplot(aes(x = value, y = sector)) +
  geom_boxplot(aes(group = sector)) +
  coord_flip() +
  labs(title = "Producer's Price Index by sector \n", 
       subtitle = "Average anual inflation rate for 2016-2021,
       \n INPP total, INPP sin petroleo y sectores",
       y = "",
       x = "inflación (%)") +
  theme_minimal()


# data_panel <- data_panel %>% 
#   mutate(
#     log_wa_interest_rate_d = log(wa_interest_rate_d),
#     log_mean_disposition = log(mean_disposition/1000)
#   )

data_panel <- data_panel %>% 
  left_join(inpc, by = c("period" = "Fecha")) 

# Summary statistics --------------

my_summary <- function(data, var){
  summarise(data, 
            N = sum(!is.na(.data[[var]])),
            Mean = mean(.data[[var]], na.rm = TRUE),
            "Std Dev" = sd(.data[[var]],na.rm = TRUE),
            "P10" = quantile(.data[[var]], .1, na.rm = TRUE),
            "P25" = quantile(.data[[var]], .25, na.rm = TRUE),
            "P50" = quantile(.data[[var]], .5, na.rm = TRUE),
            "P75" = quantile(.data[[var]], .75, na.rm = TRUE),
            "P90" = quantile(.data[[var]], .9, na.rm = TRUE))
}

data_summary <- data_panel %>% 
  mutate(mean_disposition_th = mean_disposition/1000)

vars_ <- c("mean_disposition_th", 
         "wa_interest_rate_d",
         "wa_term_d", 
         "inflation") 

vars_public <- c("Loan amount", "Interest rate", 
                 "Term", "Inflation rate")

table1_A <- map(vars_, 
                function(x) my_summary(data_summary,x)) %>% 
  do.call(rbind, .) %>% 
  mutate("Variable" = vars_public) %>% 
  select(Variable, everything()) 

table1_A[1,-1] <- round(table1_A[1,-1] ,0)

knitr::kable(
  table1_A, 
  digits = 1,
  format.args = list(big.mark = ",",   scientific = FALSE),
  format = "latex")

pdata_orig <- pdata.frame(data_panel_orig,
                     index = c("scian_code","period") )
print(pdim(pdata_orig))
max(table(index(pdata_orig), useNA = "ifany"))

pdata <- pdata.frame(data_panel,
                     index = c("scian_code","period","state_code") )
print(pdim(pdata))
mean(table(index(pdata), useNA = "ifany"))

# to deal with: "weight must be constant within sect_code" in Stata
data_panel <- data_panel %>% 
  group_by(scian_code) %>% 
  arrange(scian_code) %>% 
  mutate(N_sect = sum(N)) %>% 
  ungroup()



# Export panel  ---------------------------

# export panel 
write.csv(data_panel, paste(path,"/proc/panel_firms_",
                                       which_db,"SME.csv", sep = ""), 
          row.names = FALSE)

# export panel with sector specific inpp
write.csv(data_panel_real_adhoc, paste(path,"/proc/panel_firms_",
                            which_db,"_real_adhoc.csv", sep = ""), 
          row.names = FALSE)

# export panel with overall inpp
write.csv(data_panel_real_inpp, paste(path,"/proc/panel_firms_",
                                       which_db,"_real_inpp.csv", sep = ""), 
          row.names = FALSE)


my_summary <- function(data, var, log = TRUE) {
  if(log == TRUE){
  data %>% 
    dplyr::select({{ var }}) %>% 
    summarise(N = n(),
              Mean = mean(log1p({{ var }}), na.rm = TRUE),
              "Std Dev" = sd(log1p({{ var }}), na.rm = TRUE),
              "P10" = quantile(log1p({{ var }}),.1, na.rm = TRUE),
              "P25" = quantile(log1p({{ var }}),.25, na.rm = TRUE),
              "p50" = median(log1p({{ var }}), na.rm = TRUE),
              "P75" = quantile(log1p({{ var }}),.75, na.rm = TRUE),
              "P90" = quantile(log1p({{ var }}),.90, na.rm = TRUE)
    ) 
  }else{
    data %>% 
      dplyr::select({{ var }}) %>% 
      summarise(N = n(),
                Mean = mean(({{ var }}), na.rm = TRUE),
                "Std Dev" = sd(({{ var }}), na.rm = TRUE),
                "P10" = quantile(({{ var }}),.1, na.rm = TRUE),
                "P25" = quantile(({{ var }}),.25, na.rm = TRUE),
                "p50" = median(({{ var }}), na.rm = TRUE),
                "P75" = quantile(({{ var }}),.75, na.rm = TRUE),
                "P90" = quantile(({{ var }}),.90, na.rm = TRUE)
      ) 
  }
}



table_summary <- data.frame(rbind(
  my_summary(data_panel, mean_disposition),
  my_summary(data_panel, wa_interest_rate_d),
  my_summary(data_panel, wa_term_d),
  my_summary(data_iv_inst, num_atm, FALSE),
  my_summary(data_iv_inst, num_est_pos,TRUE),
  my_summary(data_iv_inst, num_branches, FALSE),
  my_summary(data_iv_inst, num_transactions_atm, TRUE),
  my_summary(data_iv_inst, total_staff, FALSE)
))
row.names(table_summary) <- c("Loans ($, log)",
                              "Interest rate (%, WA)",
                              "Term (months, WA)",
                              "ATM",
                              "TPV (log)",
                              "Branches",
                              "ATM transactions (log)",
                              "Staff")

knitr::kable(table_summary, digits = 2, format = "latex",booktabs=T)

View(data_panel)

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
  dplyr::select(N_rep, firms_group, `% firms_group`,
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

