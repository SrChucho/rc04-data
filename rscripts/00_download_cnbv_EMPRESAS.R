# # Function to download files  Info_CarteraEmpresas from CNBV
# Jesus Lopez-Perez
# Abril - 2022

# setup ---------------------------
# https://portafolioinfo.cnbv.gob.mx/Paginas/Contenidos.aspx?ID=40&Titulo=Banca%20M%C3%BAltiple

path_raw <- "D:/Personal/MTE/Tesis/Propuestas_Temas/credit-markets/Data/raw"

url_0 <- "https://portafolioinfo.cnbv.gob.mx/_layouts/15/download.aspx?SourceUrl=https://portafolioinfo.cnbv.gob.mx/PortafolioInformacion/Info_CarteraEmpresas_4T_21.xlsx"

#url_0 <- "https://portafolioinfo.cnbv.gob.mx/PortafolioInformacion/BM_Operativa_%s.xls" %>% sprintf(fecha_tag) 

ultimo_mes <- as.Date("2022-03-31")

tags_meses <- seq(as.Date("2016-04-01"), ultimo_mes, by = "quarter") %>% 
  format("%Y%m") %>% tibble() %>% 
  mutate(qtr = substr(.,5,6)) %>% 
  mutate(qtr2 = case_when(qtr == "01" ~ "1T",
                         qtr == "04" ~ "2T",
                         qtr == "07" ~ "3T",
                         TRUE ~ "4T")) %>% 
  mutate(yr = substr(., 3,4)) %>% 
  mutate(qtr_yr = paste(qtr2,yr, sep = "_")) %>% 
  pull(qtr_yr)

# creates function ---------------------------
descarga_cartera <- function (fecha_tag, folder) {
  # fecha_tag = tags_meses[1]; folder = path_raw
  url_0 <- "https://portafolioinfo.cnbv.gob.mx/_layouts/15/download.aspx?SourceUrl=https://portafolioinfo.cnbv.gob.mx/PortafolioInformacion/Info_CarteraEmpresas_%s.xlsx" %>% sprintf(fecha_tag) 
  download.file(url_0, method="curl", 
                destfile = file.path(folder, basename(url_0)))
}

# iterates function ---------------------------
tmp_var <- lapply(tags_meses, descarga_cartera, path_raw)


