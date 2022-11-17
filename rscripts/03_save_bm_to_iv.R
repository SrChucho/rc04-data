# MTE thesis Jesus Lopez
# Code for downloading IV for the interest rate
# 2022-05-02

rm(list = ls())

# setup ---------------------------
library(zoo)
library(readxl)
library(lubridate)
library(tidyverse)
filter <- dplyr::filter
melt <- reshape2::melt

# modify this parameters to change working variable
alias <- "transacciones_atm"

dir_cnbv <- paste("D:/Personal/MTE/Tesis/Propuestas_Temas/",
                  "credit-markets/Data", sep = "")

# import catalogs ---------------------

cat_eeffs <- read.csv(paste(dir_cnbv, "/cat_eeffs.csv", sep = ""), encoding = "UTF-8",
         col.names = c("institution_code", "institution")) %>% 
  mutate(institution_code = as.character(str_pad(institution_code,
                                                 pad = "0", width = 6, side = "left"))) 

cat_ents <- read.csv(paste(dir_cnbv, "/cat_ents.csv", sep = ""), encoding = "UTF-8",
         col.names = c("state_code","state_long", "state_short")) %>% 
  mutate(state_code = str_pad(state_code, pad = "0", width = 2, side = "left")) 


var_names_table <- 
  read.csv(paste(dir_cnbv, "/raw-bm/var_names_table.csv", sep = ""), 
  encoding = "UTF-8", col.names = c("working_var",
        "my_sheets_rnm_from", "my_sheets_rnm_to")) 

my_sheets_rnm <- 
  var_names_table[var_names_table[,"working_var"] == alias,3]
names(my_sheets_rnm) <- 
  var_names_table[var_names_table[,"working_var"] == alias,2]




sheets_df <- read.csv(paste(dir_cnbv, "/cat_sheets_cnbv.csv", sep = ""), 
                      encoding = "UTF-8",
                      col.names = c("Nombre","Alias","rnm")) %>% 
  filter(Alias == alias)

# dates
starts <- "2016-01-01" %>% as.Date()
ends <- "2022-01-01" %>% as.Date()
periods <- seq(starts, ends, by = "1 month") %>% 
  format("%Y%m")


# creates function ---------------------------

# function to read files and convert to state_level
# banks <- c("Banamex", "BBVA Bancomer")
# archivo, pestania, output, bancos = NULL
read_multibanca <- function (sheet_id, period, 
                             output = FALSE, sheets_rnm) {
  # sheet_id <- "Num de Transac en Cajeros Aut"; period <- 201601; output <- TRUE
  # sheets_rnm <- c("NÃºmero de Transacciones en Cajeros AutomÃ¡ticos" = "Num de Transac en Cajeros Aut")
  # Lee archivo de Banca Multiple
  name_ <- sheet_id
  if (output) cat(sprintf("%-32s %s\n", name_, period))
  
  file_ <- file.path(dir_cnbv, "raw-bm", "BM_Operativa_%s.xls") %>% 
    sprintf(period)
  if (file.exists(file_)) {
    
    mb_ <- read_excel(file_, sheet = name_, skip = 1) %>%
      { names(.)[1:4] <- c("State", "cvegeo", "colonia", "total")
      as_data_frame(.)} %>%  
      filter(!is.na(colonia)) %>%  # drop state totals
      slice(-1) %>%  # drop total first row 
      mutate(
        state_code  = zoo::na.locf(cvegeo) %>% str_sub(4, 5),
        period   = period %>% sprintf("%s01", .) %>% ymd,
        var    = name_) %>% 
      select(-total, -colonia, -State, -cvegeo) %>% 
      select(period, var, state_code, everything())
    
    mb <- mb_ %>%  pivot_longer(cols = -c(period, var, state_code), 
                                names_to = "bank") %>% 
      select(period, var, state_code, 
             bank, value) %>% 
      group_by(period, var, state_code, bank) %>% 
      summarize_at("value", . %>% sum(na.rm = TRUE))
      data.frame()
    # mb <- select(mb_, c(banks, 
    #                     "type","cvegeo","period","value"))
  } else {
    # sheet_id <- "Num de Transac en Cajeros Aut"; period <- "201603"; 
    # output <- TRUE
    file_ <- file.path(dir_cnbv, "raw-bm", "BM_Operativa_%s.xlsx") %>% 
      sprintf(period)
    
    if ("Hoja1" %in% excel_sheets(file_)) {
      mb <- read_excel(file_, sheet = "Hoja1", col_types = 
                           c("text", "text", "text", "text", 
                             "text", "text", "skip","text", 
                             "numeric", "skip", "skip"),
                         range = cell_cols("A:K")) %>% 
        mutate(period = period %>% str_c("01") %>% as.Date("%Y%m%d"),
               state_code = cve_inegi %>% str_sub(4, 5), 
               var = dl_producto_financiero %>% str_replace_all(sheets_rnm)) %>% 
        select(period = period, 
               var, 
               state_code,
               bank = nombre_publicacion,
               value = dat_num_total) %>% 
        filter(var == sheet_id) %>% 
        select(period, var, state_code, 
               bank, value) %>% 
        group_by(period, var, state_code, bank) %>% 
        summarize_at("value", . %>% sum(na.rm = TRUE))
      } else {
      mb <- read_excel(file_, sheet = "Datos", col_types = 
                           c("text", "text", "text", "text", "text", "text", "skip",
                             "text", "numeric", "skip", "skip", "skip"),range = cell_cols("A:L")) %>% 
        mutate(period = period %>% str_c("01") %>% as.Date("%Y%m%d"),
               state_code = cvegeo %>% str_sub(4, 5), 
               var = var %>% str_replace_all(sheets_rnm)) %>% 
        select(period = `Periodo (clave)`, 
               var = `Producto Financiero`, 
               state_code,
               bank = `InstituciÃ³n`,
               value = Total) %>% 
        filter(var == sheet_id) %>% 
        select(period, var, state_code, 
               bank, value) %>% 
        group_by(cvegeo, state_code) %>% 
        summarize_at("value", . %>% sum(na.rm = TRUE))
    }
    
    # mb_1 <- mb_0 %>% 
    #   group_by(type, cvegeo, period, bank) %>% 
    #   summarize_at("value", . %>% sum(na.rm = TRUE))
    # 
    # mb_2 <- mb_0 %>% 
    #   group_by(type, cvegeo, period, bank) %>% 
    #   summarize_at("value", . %>% sum(na.rm = TRUE))
    # 
    # mb <- bind_rows(mb_1, mb_2) %>% 
    #   pivot_longer(cols = -type,
    #                names_to = c("bank","value"),
    #                names_sep =" [.]")
  }
  return (mb)
}

# iterates function ---------------------------
MB_frame_ <- expand.grid(sheets_df$Nombre, periods, 
                         stringsAsFactors=FALSE) %>%
  apply(1, . %>% {read_multibanca(.[1], .[2], 
                                  output=TRUE, sheets_rnm = my_sheets_rnm)}) 

MB_frame_r <- MB_frame_ %>% 
  bind_rows %>% data.frame()

# homologate names of banks
MB_frame_ <- MB_frame_r %>% 
  mutate(bank = recode(bank, `Banamex` = "Citibanamex",
                     `Banco Bancrea` = "Bancrea",
                     `Banco Credit Suisse` = "Credit Suisse",
                     `Banco Sabadell` = "Sabadell",
                     `Bank of Tokyo-Mitsubishi UFJ` = "MUFG Bank",
                     `Banorte/Ixe` = "Banorte", ## recoded 20220720
                     #`Barclays` = "NA", # we remove this institution
                     `BBVA Bancomer` = "BBVA México",
                     `Forjadores` = "Banco Forjadores",
                     #`Investa Bank` = "NA", # we remove this institution
                     # `UBS` = "NA", # we remove this institution
                     #`Volkswagen Bank` = "NA", # we remove this institution
                     #`BIAfirme` = "NA", # we remove this institution
                     #`Pagatodo` = "NA", # we remove this institution
                     #`Banco S3` = "NA", #we remove this institution
                     `KEB Hana Bank` = "KEB Hana México"))

# check
MB_frame_ %>% 
  left_join(cat_eeffs, by = c("bank" = "institution")) %>% 
  filter(is.na(institution_code))


# we filter out institutions with no credits
MB_frame <- MB_frame_ %>% 
  left_join(cat_eeffs, by = c("bank" = "institution")) %>% 
  filter(!is.na(institution_code)) %>% 
  select(period, var, state_code,
         institution_code, value)

# export data ---------------------------

write.csv(MB_frame, paste(dir_cnbv,"/proc/",alias,"-", 
                          starts,"_",ends,".csv", sep = ""),row.names = FALSE,
          fileEncoding = "UTF-8")

