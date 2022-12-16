library(dplyr)
library(magrittr)
library(openxlsx)
library(readxl)

bydsa <- read_excel("C:/Users/Eduardo.Armenta/Desktop/Xignux/Trabajos Miscelaneos/Categorias SAP/BYDSA_Categorizacion_SAP.xlsx",
                  sheet = "BASE_SAP_V3")
muestra <- bydsa %>%
  filter(is.na(DESC_SEGMENTO_ES)) %>%
  sample_n(size=83500)

write.xlsx(muestra, file="Muestra_Bydsa_100K_V3.xlsx", sheetName="Muestra100K")
  