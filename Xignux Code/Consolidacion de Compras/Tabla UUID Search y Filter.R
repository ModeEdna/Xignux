library(dplyr)
library(magrittr)
BD_Proveedores_Filtrado <- BD_Proveedores %>%
  filter(!is.na(UUID))
# esto filtra el DB por UUID no vacios
library(openxlsx)
write.xlsx(BD_Proveedores_Filtrado, file = "BD_Proveedores.xlsx", sheetName = "BD_Proveedores_Filtrado.xlsx")
