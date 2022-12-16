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
  
  library(tibble)
  library(astsa)
  library(dplyr)
  
  totales <- dataframe1 %>% select(Amount)
  totalests <- ts(totales, start = 0, end = 65, frequency = 1)
  totalesdiff <- diff(totalests)
  results <- sarima.for(totalesdiff, n.ahead = 12, 1, 1, 1, 1, 0, 1, 12)
  results1 <- totalests[65] + cumsum(results$pred)
  dataframeResults <- data.frame(c('Jul', 'Ago', 'Sep', 'Oct', 'Nov', 'Dic', 'Ene', 'Feb', 'Mar', 'Abr', 'May', 'Jun'), c('2021', '2021', '2021', '2021', '2021', '2021', '2022', '2022', '2022', '2022', '2022', '2022'), results1, results$se, results$se*2, results$se*-1, results$se*-2)
  dataframeResults <- dataframeResults %>% rename(Mes = c..07....08....09....10....11....12....01....02....03....04..., Año = c..2021....2021....2021....2021....2021....2021....2022....2022..., Prediction = results1, StandardError = results.se, StandardError2 = results.se...2, StandardErrorN = results.se....1, StandardError2N = results.se....2)  
  
  return(list(dataset1=dataframeResults))
}