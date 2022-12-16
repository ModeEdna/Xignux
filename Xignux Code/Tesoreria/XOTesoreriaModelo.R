# load packages to use
library(astsa)
library(readxl)
library(tidyverse)
library(tseries)
library(forecast)

# load excel from desktop, select the Amount column
XO <- read_excel("C:/Users/Eduardo.Armenta/Desktop/Xignux/Proyectos/Tesoreria/Data/XOTesoreriaPronostico.xlsx")
XO1 <- XO %>% select(Amount)
print(XO1)

# turn data into a time series and check to see if it changed format
XOts <- ts(XO1, start = 0, end = 32, frequency = 1)
is.ts(XOts)
print(XOts)

# plot the data with vertical lines at 12-month interval
tsplot(x = XOts, ylim = c(0, 10000000), 
       main = "Dolares por mes por año", xlab = "Numero del mes", ylab = "Dolares", xaxt = "n", yaxt = "n")
axis(1, at=seq(0, 45, by=12))
marks <- c(0, 2500000, 5000000, 7500000, 10000000)
axis(2, at=marks, labels=format(marks, scientific=FALSE))
abline(v=c(11, 23, 35), col = "red")

# look at the differentiated and log versions of the time series
XOdiff <- diff(XOts)
plot(XOdiff)
adf.test(XOdiff) #smaller than 0.05 means stationary
plot(log(XOdiff))

# fit the models and forecast with normal data
acf2(XOts)
sarima(XOts, 1, 0, 1)
sarima.for(XOts, n.ahead = 12, p = 1, d = 0, q = 1)
sarima(XOts, 1, 0, 1, 0, 1, 1, 12)
sarima.for(XOts, n.ahead = 12, p = 1, d = 0, q = 0, P = 0, D = 1, Q = 1, S = 12)

# fit the models and forecast with differentiated data
acf2(XOdiff)
sarima(XOdiff, 1, 1, 1, 0, 1, 1, 12)
resultsXO <- sarima.for(XOdiff, n.ahead = 12, p = 1, d = 1, q = 1, P = 0, D = 1, Q = 1, S = 12)

# print undifferenced forecast
print(resultsXO$pred)
print(resultsXO$se)
dataframeresultsXO <- data.frame(c('Jul', 'Ago', 'Sep', 'Oct', 'Nov', 'Dic', 'Ene', 'Feb', 'Mar', 'Abr', 'May', 'Jun'), resultsXO$pred, resultsXO$se, resultsXO$se*2, resultsXO$se*-1, resultsXO$se*-2)
print(dataframeresultsXO)
dataframeresultsXO <- dataframeresultsXO %>% rename(Fecha = c..Jul....Ago....Sep....Oct....Nov....Dic....Ene....Feb....Mar..., Prediction = resultsXO.pred, StandardError = resultsXO.se,
                                                StandardError2 = resultsXO.se...2, StandardErrorN = resultsXO.se....1,
                                                StandardError2N = resultsXO.se....2)
print(dataframeresultsXO)
resultsXO2 <- sarima.for(XOdiff, n.ahead = 12, p = 1, d = 1, q = 1, P = 0, D = 1, Q = 1, S = 12)
undiff <- XOts[33] + cumsum(resultsXO2$pred)
print(XOts[33])
tsplot(undiff, ylim = c(0, 10000000), main = "Dolares por mes", ylab = "Dolares")
undiffData <- data.frame(undiff)
undiffDatats <- ts(undiffData, start = 0, end = 11, frequency = 1)
print(undiffDatats)
is.ts(undiffDatats)
allTogether <- ts(c(XOts, undiffDatats), start = start(XOts), frequency = frequency(XOts))
tsplot(allTogether, ylim = c(0, 10000000), fill="undiffDatats")
abline(v=32.5, col = "red")

# auto-find best parameters for model
best <- auto.arima(XOdiff,
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
sarima(XOdiff, 1, 1, 2, 0, 1, 1, 12)
sarima.for(XOdiff, n.ahead = 12, p = 1, d = 1, q = 2, P = 0, D = 1, Q = 1, S = 12)