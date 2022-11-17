# MTE thesis Jesus Lopez
# Code for downloading IV data for the interest rate from CNBV
# 2022-05-02
# adapted from Diego Villamil, OPI, IMCO

# setup ---------------------------
library(tidyverse)

# verify which is the last month available at 
# http://portafolioinfo.cnbv.gob.mx/PUBLICACIONES/IO/Paginas/bm.aspx
last_month <- as.Date("2021-12-01")

tags_months <- seq(as.Date("2021-03-01"), last_month, by = "1 month") %>% 
  format("%Y%m")

path_raw <- "D:/Personal/MTE/Tesis/Propuestas_Temas/credit-markets/Data/raw-bm/"

# creates function  ---------------------------
download_bm <- function (fecha_tag, folder) {
  # fecha_tag <- "201609"; folder <- path_raw
  url_0 <- "https://portafolioinfo.cnbv.gob.mx/PortafolioInformacion/BM_Operativa_%s.xls" %>% sprintf(fecha_tag) 
  download.file(url_0, method="curl", 
                destfile = file.path(folder, basename(url_0)))
  
  # files randomly change between  xls and xlsx
  # we identify the file size and correct for it
  if (file.size(file.path(folder, basename(url_0))) < 1000) {
    url_1 <- str_replace(url_0, ".xls$", ".xlsx")
    download.file(url_1, method="curl", 
                  destfile = file.path(folder, basename(url_1)))
    file.remove(file.path(folder, basename(url_0)))
  }
}

# iterates function ---------------------------
tmp_var <- lapply(tags_months, download_bm, path_raw)

