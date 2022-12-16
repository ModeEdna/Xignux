# load packages to use
library(astsa)
library(readxl)
library(tidyverse)
library(tseries)
library(forecast)

# load excel from desktop, select the Amount column
XOSE <- read_excel("C:/Users/Eduardo.Armenta/Desktop/Xignux/Proyectos/Tesoreria/Data/XOSEEFactorTesoreriaPronostico.xlsx")
XOSE1 <- XOSE %>% select(Amount)
print(XOSE1)

# turn data into a time series and check to see if it changed format
XOSEts <- ts(XOSE1, start = 0, end = 65, frequency = 1)
is.ts(XOSEts)
print(XOSEts)

# plot the data with vertical lines at 12-month interval
tsplot(x = XOSEts, ylim = c(0, 30000000), 
       main = "Dolares por mes por año", xlab = "Numero del mes", ylab = "Dolares", xaxt = "n", yaxt = "n")
axis(1, at=seq(0, 66, by=12))
marks <- c(0, 5000000, 10000000, 15000000, 20000000, 25000000, 30000000)
axis(2, at=marks, labels=format(marks, scientific=FALSE))
abline(v=c(11, 23, 35, 47, 59, 71), col = "red")

# look at the differentiated and log versions of the time series
XOSEDiff <- diff(XOSEts)
plot(XOSEDiff)
adf.test(XOSEDiff) #smaller than 0.05 means stationary
plot(log(XOSEDiff))

# fit the models and forecast with normal data
acf2(XOSEts)
sarima(XOSEts, 2, 1, 1)
sarima.for(XOSEts, n.ahead = 12, p = 2, d = 1, q = 1)
sarima(XOSEts, 1, 1, 1 ,0 ,1, 1, 12)
sarima.for(XOSEts, n.ahead = 12, p = 2, d = 1, q = 1, P = 0, D = 1, Q = 1, S = 12)

# fit the models and forecast with differentiated data
acf2(XOSEDiff)
#sarima(XOSEDiff, 1, 1, 1, 0, 1, 1, 12)
sarima(XOSEDiff, 3, 1, 0, 0, 0, 0, 0)
results <- sarima.for(XOSEDiff, n.ahead = 12, p = 1, d = 1, q = 1, P = 0, D = 1, Q = 1, S = 12)

# print undifferenced forecast
print(results$pred)
print(results$se)
dataframeResults <- data.frame(c('Jul', 'Ago', 'Sep', 'Oct', 'Nov', 'Dic', 'Ene', 'Feb', 'Mar', 'Abr', 'May', 'Jun'), results$pred, results$se, results$se*2, results$se*-1, results$se*-2)
print(dataframeResults)
dataframeResults <- dataframeResults %>% rename(Fecha = c..Jul....Ago....Sep....Oct....Nov....Dic....Ene....Feb....Mar..., Prediction = results.pred, StandardError = results.se,
                                                StandardError2 = results.se...2, StandardErrorN = results.se....1,
                                                StandardError2N = results.se....2)
print(dataframeResults)
#results2 <- sarima.for(XOSEDiff, n.ahead = 12, p = 1, d = 1, q = 1, P = 1, D = 0, Q = 1, S = 12)
results2 <- sarima.for(XOSEDiff, n.ahead = 12, p = 4, d = 1, q = 0, P = 0, D = 0, Q = 0, S = 0)
undiff <- XOSEts[66] + cumsum(results2$pred)
tsplot(undiff, ylim = c(10000000, 20000000), main = "Dolares por mes", ylab = "Dolares")
undiffData <- data.frame(undiff)
undiffDatats <- ts(undiffData, start = 0, end = 11, frequency = 1)
print(undiffDatats)
is.ts(undiffDatats)
allTogether <- ts(c(XOSEts, undiffDatats), start = start(XOSEts), frequency = frequency(XOSEts))
tsplot(allTogether, ylim = c(0, 20000000), fill="undiffDatats")
abline(v=65.5, col = "red")

# auto-find best parameters for model
best <- auto.arima(XOSEDiff,
                   d = 1,
                   D = 1,
                   max.p = 4,
                   max.q = 4,
                   max.P = 4,
                   max.Q = 4,
                   max.order = 5,
                   max.d = 3,
                   max.D = 3,
                   allowdrift = TRUE,
                   stepwise = TRUE,
                   approximation = FALSE,
                   allowmean = TRUE,
                   trace = TRUE)
print(best)
sarima(XOSEDiff, 1, 1, 2, 0, 1, 1, 12)
sarima.for(XOSEDiff, n.ahead = 12, p = 1, d = 1, q = 2, P = 0, D = 1, Q = 1, S = 12)