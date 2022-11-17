# MTE thesis Jesus Lopez
# Code for downloading Boletin Estadistico data from CNBV
# 2022-07-25
# adapted from Diego Villamil, OPI, IMCO

# setup ---------------------------
library(tidyverse)

# verify which is the last month available at 
# http://portafolioinfo.cnbv.gob.mx/PUBLICACIONES/IO/Paginas/bm.aspx
last_month <- as.Date("2021-12-01")

tags_months <- seq(as.Date("2016-01-01"), last_month, by = "1 month") %>% 
  format("%Y%m")

path_raw <- "D:/Personal/MTE/Tesis/Propuestas_Temas/credit-markets/Data/raw-be/"

# creates function  ---------------------------
download_bm <- function (fecha_tag, folder) {
  # fecha_tag <- "201601"; folder <- path_raw
  print(paste("Downloading:",fecha_tag, sep = ""))
  url_0 <- "https://portafolioinfo.cnbv.gob.mx/PortafolioInformacion/BE_BM_%s.xls" %>% sprintf(fecha_tag) 
  download.file(url_0, method="curl", 
                destfile = file.path(folder, basename(url_0)))
  
  # files randomly change between  xls, xlsx and xlsm
  # we identify the file size and correct for it
  if (file.size(file.path(folder, basename(url_0))) < 1000) {
    url_1 <- str_replace(url_0, ".xls$", ".xlsx")
    download.file(url_1, method="curl", 
                  destfile = file.path(folder, basename(url_1)))
    file.remove(file.path(folder, basename(url_0)))
  
    if (file.size(file.path(folder, basename(url_1))) < 1000) {
      url_2 <- str_replace(url_0, ".xls$", ".xlsm")
      download.file(url_2, method="curl", 
                    destfile = file.path(folder, basename(url_2)))
      file.remove(file.path(folder, basename(url_1)))
    }
  }
}

# iterates function ---------------------------
tmp_var <- lapply(tags_months, download_bm, path_raw)

