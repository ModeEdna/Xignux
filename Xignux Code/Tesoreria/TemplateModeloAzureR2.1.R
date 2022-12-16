# R version: 3.5.1
# The script MUST contain a function named azureml_main
# which is the entry point for this module.

# Please note that functions dependant on X11 library
# such as "View" are not supported because X11 library
# is not pre-installed.

# The entry point function MUST have two input arguments.
# If the input port is not connected, the corresponding
# dataframe argument will be null.
#   Param<dataframe1>: a R DataFrame
#   Param<dataframe2>: a R DataFrame
azureml_main <- function(dataframe1){
  print("R script run.")
  
  if(!require(astsa)) install.packages("astsa",repos = "https://cloud.r-project.org")
  if(!require(tibble)) install.packages("tibble",repos = "https://cloud.r-project.org")
  if(!require(dplyr)) install.packages("dplyr",repos = "https://cloud.r-project.org")
  if(!require(zoo)) install.packages("zoo",repos = "https://cloud.r-project.org")
  if(!require(lubridate)) install.packages("lubridate",repos = "https://cloud.r-project.org")
  if(!require(tidyr)) install.packages("tidyr",repos = "https://cloud.r-project.org")
  
  library(tibble)
  library(astsa)
  library(dplyr)
  library(zoo)
  library(lubridate)
  library(tidyr)
  
  D <- data.frame(month(Sys.Date()), year(Sys.Date()))
  D <- rename(D, Month = month.Sys.Date..., Year = year.Sys.Date...)
  D <- as.yearmon(paste0(D$Year, "-", D$Month)) + 0:11/12
  D1 <- data.frame(D)
  D2 <- separate(D1, col = "D", into = c("Mes", "Año"), sep = " ")
  D2[D2 == "Aug"] <- ("08")
  D2[D2 == "Sep"] <- ("09")
  D2[D2 == "Oct"] <- ("10")
  D2[D2 == "Nov"] <- ("11")
  D2[D2 == "Dec"] <- ("12")
  D2[D2 == "Jan"] <- ("01")
  D2[D2 == "Feb"] <- ("02")
  D2[D2 == "Mar"] <- ("03")
  D2[D2 == "Apr"] <- ("04")
  D2[D2 == "May"] <- ("05")
  D2[D2 == "Jun"] <- ("06")
  D2[D2 == "Jul"] <- ("07")
  
  totales <- dataframe1 %>% select(Importe)
  totalests <- ts(totales, start = 0, end = nrow(totales)-1, frequency = 1)
  totalesdiff <- diff(totalests)
  results <- sarima.for(totalesdiff, n.ahead = 12, p = 1, d = 1, q = 1, P = 1, D = 0, Q = 1, S = 12)
  results1 <- totalests[nrow(totales)] + cumsum(results$pred)
  dataframeResults <- data.frame(D2$Mes, D2$Año, results1, results$se, results$se*2, results$se*-1, results$se*-2)
  dataframeResults <- dataframeResults %>% rename(Mes = D2.Mes, Año = D2.Año, Prediccion = results1, StandardError = results.se, StandardError2 = results.se...2, StandardErrorN = results.se....1, StandardError2N = results.se....2)
  
  return(list(dataset1=dataframeResults))
}