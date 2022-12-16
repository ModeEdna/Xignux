library(dplyr)
library(magrittr)
filtrados <- BD_Proveedores %>%
  unique() %>%
  select(REF_ERP, UUID) %>%
  filter(!is.na(UUID)) %>%
  filter(!is.na(REF_ERP))
library(openxlsx)
write.xlsx(filtrados, file="Tabla_RFC_UUID.xlsx", sheetName="RFC_UUID")
