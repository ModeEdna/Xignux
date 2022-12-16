library(readxl)
library(dlookr)

RUV <- read_excel("C:/Users/Eduardo.Armenta/Desktop/Xignux/Proyectos/TAM/Datos/EDA_RUV.xlsx")

eda_report(RUV, output_format = "html")
