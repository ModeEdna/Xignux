# load packages to use
library(astsa)
library(readxl)
library(tidyverse)
library(tseries) # for adf test

# load excel from desktop, select the Amount column
totales <- read_excel("C:/Users/Eduardo.Armenta/Desktop/Xignux/Proyectos/Tesoreria/Data/QualtiaTesoreriaPronostico.xlsx")
totales1 <- totales %>% select(Amount)
nrow(totales1)

# turn data into a time series and check to see if it changed format
totalests <- ts(totales1, start = 0, end = nrow(totales1)-1, frequency = 1)
print(totalests)

# look at the differentiated time series
totalesdiff <- diff(totalests)
adf.test(totalesdiff)

# fit the models and make sure data is stationary
acf2(totalesdiff)
sarima(totalesdiff, 1, 1, 1, 1, 0, 1, 12)

# print undifferenced forecast
results <- sarima.for(totalesdiff, n.ahead = 12, p = 1, d = 1, q = 1, P = 1, D = 0, Q = 1, S = 12)
results1 <- totalests[nrow(totales)] + cumsum(results$pred)
dataframeResults <- data.frame(c('07', '08', '09', '10', '11', '12', '01', '02', '03', '04', '05', '06'), c('2021', '2021', '2021', '2021', '2021', '2021', '2022', '2022', '2022', '2022', '2022', '2022'), results1, results$se, results$se*2, results$se*-1, results$se*-2)
dataframeResults <- dataframeResults %>% rename(Mes = c..07....08....09....10....11....12....01....02....03....04..., Año = c..2021....2021....2021....2021....2021....2021....2022....2022..., Prediction = results1, StandardError = results.se,
                                                StandardError2 = results.se...2, StandardErrorN = results.se....1,
                                                StandardError2N = results.se....2)
print(dataframeResults)

# Get historical and prediction data together
undiffData <- data.frame(results1)
undiffDatats <- ts(undiffData, start = 0, end = 11, frequency = 1)
allTogether <- ts(c(totalests, undiffDatats), start = start(totalests), frequency = frequency(totalests))
tsplot(allTogether, ylim = c(0, 20000000))
abline(v=nrow(totalests)-1, col = "red")
print(allTogether)
