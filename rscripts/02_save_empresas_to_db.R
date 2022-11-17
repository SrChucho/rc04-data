# Function to process "Reportes  Info_CarteraEmpresas" downloaded from CNBV
# This script generates de main DB 
# Jesus Lopez-Perez
# April - 2022

# setup ---------------------------
library(zoo)
library(readxl)
library(lubridate)
library(tidyverse)
library(stringr)

rm(list = ls())

filter <- dplyr::filter
melt <- reshape2::melt

path <- "D:/Personal/MTE/Tesis/Propuestas_Temas/credit-markets/Data"

# Reduced DB (TRUE - only G7, FALSE - ALL banks)
g7 <- TRUE

if(g7 == TRUE){
  banks <- c("040002", "040012", "040014", "040021",
             "040072", "040044", "040036")
  which_db <- "G7"
}else{
  banks = NULL
  which_db <- "ALL"
}


# data up to this date
last_month <- as.Date("2021-12-31")

# define quarters 
tags_meses <- seq(as.Date("2016-04-01"), last_month, by = "quarter") %>% 
  format("%Y%m") %>% tibble() %>% 
  mutate(qtr = substr(.,5,6)) %>% 
  mutate(qtr2 = case_when(qtr == "01" ~ "1T",
                          qtr == "04" ~ "2T",
                          qtr == "07" ~ "3T",
                          TRUE ~ "4T")) %>% 
  mutate(yr = substr(., 3,4)) %>% 
  mutate(qtr_yr = paste(qtr2,yr, sep = "_")) %>% 
  pull(qtr_yr)

# define which sheet will read
pestagna_id <- "Empresas"

# import catalogs  ---------------------------
read.csv(paste(path, "/cat_ents.csv", sep = ""), encoding = "UTF-8",
         col.names = c("state_code","state_long", "state_short")) %>% 
  mutate(state_code = str_pad(state_code, pad = "0", width = 2, side = "left")) ->  cat_ents

cat_ents <- cat_ents %>% 
  mutate(state_code = ifelse(state_code == 999,99,state_code))

read.csv(paste(path, "/cat_eeffs.csv", sep = ""), encoding = "UTF-8",
         col.names = c("institution_code", "institution")) %>% 
  mutate(institution_code = as.character(str_pad(institution_code,
                  pad = "0", width = 6, side = "left"))) -> cat_eeffs

read.csv(paste(path, "/cat_sectores.csv", sep = ""), encoding = "UTF-8",
         col.names = c("sector_code", "Econ_Act", "scian_code", "scian_code2",
                       "GA_code", "description")) %>% 
  mutate(sector_code = as.character(str_pad(sector_code,
                  pad = "0", width = 2, side = "left"))) -> cat_sects

read.csv(paste(path, "/cat_destino.csv", sep = ""), encoding = "UTF-8",
         col.names = c("funds_use_cnbv_code", "use_description","funds_use_code")) -> cat_funds_use
  
read.csv(paste(path, "/cat_reporte_cnbv.csv", sep = ""), encoding = "UTF-8",
           col.names = c("cnbv_report_code","report_description",
                         "large_description")) -> cat_cnbv_report
  
read.csv(paste(path, "/cat_firm_size.csv", sep = ""), encoding = "UTF-8",
         col.names = c("firm_size_code", "firm_description")) -> cat_firm_size

read.csv(paste(path, "/cat_currency.csv", sep = ""), encoding = "UTF-8",
         col.names = c("currency_code", "currency")) -> cat_currency



# creates function ---------------------------
# function to read file and give format
# archivo, pestania, output, bancos = NULL
read_firms <- function (pestagna_id, period, 
                             output = FALSE, banks = NULL) {
  # 1. reading file Info CarteraEmpresas
  # 2. recode vars with catalogues
  # 3. creates new variables (rate, arrears, etc)
  # 4. select relevant variables
  # 5. filters for G7 if necessary
  # 6. filters for dispositions > 0 
  # for debugging: period <- tags_meses[1]; output <- TRUE; banks = NULL
  
  name_ <- pestagna_id
  if (output) cat(sprintf("%-32s %s\n", name_, period))
  
  archivo_ <- file.path(path, "raw", "Info_CarteraEmpresas_%s.xlsx") %>% 
    sprintf(period)
  
  if (file.exists(archivo_)) {
  
    firms_ <- read_excel(archivo_, sheet = name_) %>%
      {as_data_frame(.)} %>%  
      filter(Monto_dispuesto > 0) %>% # we keep only new loans
      mutate(
        institution_code = as.character(cve_institucion),
        state_code =  ifelse(cve_estado == 999, 99,
                             str_pad(cve_estado, pad = "0", width = 2, side = "left")),
        sector_code = str_pad(cve_sector_actividad_economica, pad = "0", width = 2, side = "left"),
        interest_rate_disposition = ifelse(Monto_dispuesto ==0,"", Tasa_x_monto_dispuesto / Monto_dispuesto),
        interest_rate_all = Tasa_x_resp_total / Cartera_total,
        term_disposition = ifelse(Monto_dispuesto ==0,"", Plazo_x_monto_dispuesto / Monto_dispuesto),
        term_all = Plazo_x_resp_total / Cartera_total,
        average_loan = Cartera_total/Num_creditos,
        past_due_pct = 100 * `Cartera Vencida` / (`Cartera Vigente` + `Cartera Vencida`)
          ) %>% 
      mutate(Destino_credito = 
               ifelse(Destino_credito == "Proyectos de infraestructura",
                      "Proyectos de Infraestructura",
                      Destino_credito)) %>% 
      mutate(cnbv_report_code = paste("R",str_sub(Subreporte,7,9), sep = "") ) %>% 
      left_join(y = cat_eeffs, by= ("institution_code")) %>%  
      left_join(y = cat_sects, by = c("sector_code" ="sector_code")) %>%
      left_join(y = cat_ents, by = c("state_code" = "state_code") ) %>%
      left_join(y = cat_funds_use, by = c("Destino_credito" = "use_description") ) %>% 
      left_join(y = cat_currency, by = c("Moneda" = "currency") ) %>%
      left_join(y = cat_firm_size, by = c("Tamanio_empresa" = "firm_description") ) %>% 
      #filter(Cartera_total != 0 ) %>% 
      mutate(firm_id = paste0(institution_code,# 6 digits
                              cnbv_report_code, # 3 digits
                              scian_code2,# 2 digits
                              state_code,# 2 digits
                              funds_use_code,# 3 digits
                              currency_code, # 2 letters
                              firm_size_code)) %>% # 1 letter
      select(firm_id,
             period = Periodo, 
             institution_code, institution,  
             cnbv_report_code,         
             scian_code,
             GA_code,
             state_code, 
             funds_use_code, 
             currency_code, 
             firm_size_code, # 1 letter
             number_of_loans = Num_creditos,
             number_of_debtors_by_rfc = Num_acreditados_x_RFC,
             loan_disposition_month = Monto_dispuesto,
             int_rate_x_total_resp = Tasa_x_resp_total,
             int_rate_x_loan_amount = Tasa_x_monto_dispuesto,
             term_x_total_resp = Plazo_x_resp_total,
             term_x_loan_amount = Plazo_x_monto_dispuesto,
             total_portfolio = Cartera_total,
             current_portfolio = `Cartera Vigente`,
             past_due_portfolio = `Cartera Vencida`,
             sector_code:past_due_pct)
      
    
    if(!is.null(banks)){
      firms <-  firms_ %>% filter(institution_code %in% banks)
    }else{
      firms <- firms_ 
    }
    
  }
  return(firms)
}


# iterates function ---------------------------
# Process files and generates database 
firms_frame_ <- expand.grid(pestagna_id, tags_meses, 
                            stringsAsFactors=FALSE) %>% 
  apply(1, . %>% {read_firms(.[1], .[2], output=TRUE, 
                             banks = banks )}) 

firms_frame <- firms_frame_ %>% 
  bind_rows 

# export results ---------------------------
write.csv(firms_frame,
          paste(path,"/proc/",
                "firms_",which_db,"_",tags_meses[1],"_",tags_meses[length(tags_meses)],
                ".csv", sep = ""),
          fileEncoding = "UTF-8")

