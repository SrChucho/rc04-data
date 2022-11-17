# Setup  ---------------------------

library(tidyverse)
library(stringr)
library(ggplot2)
library(zoo)
library(fpp3)
library(tibble)

path <- "D:/Personal/MTE/Tesis/Propuestas_Temas/credit-markets/Data"

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


# catalogues
read.csv(paste(path, "/cat_ents.csv", sep = ""), encoding = "UTF-8",
         col.names = c("state_code","state_long", "state_short")) %>% 
  mutate(state_code = str_pad(state_code, pad = "0", width = 2, side = "left")) ->  cat_ents

cat_ents <- cat_ents %>% 
  mutate(state_code = ifelse(state_code == 999,99,state_code))

read.csv(paste(path, "/cat_eeffs.csv", sep = ""), encoding = "UTF-8",
         col.names = c("institution_code", "institution", "group")) %>% 
  mutate(institution_code = as.character(str_pad(institution_code,
                                                 pad = "0", width = 6, side = "left"))) -> cat_eeffs

read.csv(paste(path, "/cat_sectores.csv", sep = ""), encoding = "UTF-8",
         col.names = c("sector_code", "Econ_Act", "scian_code", "scian_code2",
                       "GA_code", "description")) %>% 
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
