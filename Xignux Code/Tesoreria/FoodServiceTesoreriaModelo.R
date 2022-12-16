# load packages to use
library(astsa)
library(readxl)
library(tidyverse)
library(tseries)
library(forecast)

# load excel from desktop, select the Amount column
FS <- read_excel("C:/Users/Eduardo.Armenta/Desktop/Xignux/Proyectos/Tesoreria/FoodServiceTesoreriaPronostico.xlsx")
FS1 <- FS %>% select(Amount)
print(FS1)

# turn data into a time series and check to see if it changed format
FSts <- ts(FS1, start = 0, end = 57, frequency = 1)
is.ts(FSts)
print(FSts)

# plot the data with vertical lines at 12-month interval
tsplot(x = FSts, ylim = c(0, 2500000), 
       main = "Dolares por mes por año", xlab = "Numero del mes", ylab = "Dolares", xaxt = "n", yaxt = "n")
axis(1, at=seq(0, 60, by=12))
marks <- c(0, 1000000, 1500000, 2000000, 2500000)
axis(2, at=marks, labels=format(marks, scientific=FALSE))
abline(v=c(11, 23, 35, 47, 59), col = "red")

# look at the differentiated and log versions of the time series
FSdiff <- diff(FSts)
plot(FSdiff)
adf.test(FSdiff) #smaller than 0.05 means stationary

# fit the models and forecast with normal data
acf2(FSts)
sarima(FSts, 1, 0, 2)
sarima.for(FSts, n.ahead = 12, p = 1, d = 0, q = 2)
sarima(FSts, 1, 0, 1, 0, 1, 1, 12)
sarima.for(FSts, n.ahead = 12, p = 1, d = 0, q = 1, P = 0, D = 1, Q = 1, S = 12)

# fit the models and forecast with differentiated data
acf2(FSdiff)
sarima(FSdiff, 1, 1, 1, 0, 1, 1, 12)
resultsFS <- sarima.for(FSdiff, n.ahead = 12, p = 1, d = 1, q = 1, P = 0, D = 1, Q = 1, S = 12)

# print undifferenced forecast
print(resultsFS$pred)
print(resultsFS$se)
dataframeresultsFS <- data.frame(c('Jul', 'Ago', 'Sep', 'Oct', 'Nov', 'Dic', 'Ene', 'Feb', 'Mar', 'Abr', 'May', 'Jun'), resultsFS$pred, resultsFS$se, resultsFS$se*2, resultsFS$se*-1, resultsFS$se*-2)
print(dataframeresultsFS)
dataframeresultsFS <- dataframeresultsFS %>% rename(Fecha = c..Jul....Ago....Sep....Oct....Nov....Dic....Ene....Feb....Mar..., Prediction = resultsFS.pred, StandardError = resultsFS.se,
                                                    StandardError2 = resultsFS.se...2, StandardErrorN = resultsFS.se....1,
                                                    StandardError2N = resultsFS.se....2)
print(dataframeresultsFS)
resultsFS2 <- sarima.for(FSdiff, n.ahead = 12, p = 1, d = 1, q = 1, P = 0, D = 1, Q = 1, S = 12)
undiff <- FSts[58] + cumsum(resultsFS2$pred)
print(FSts[58])
tsplot(undiff, ylim = c(0, 2500000), main = "Dolares por mes", ylab = "Dolares")
undiffData <- data.frame(undiff)
undiffDatats <- ts(undiffData, start = 0, end = 11, frequency = 1)
print(undiffDatats)
is.ts(undiffDatats)
allTogether <- ts(c(FSts, undiffDatats), start = start(FSts), frequency = frequency(FSts))
tsplot(allTogether, ylim = c(0, 2500000), fill="undiffDatats")
abline(v=57.5, col = "red")

# auto-find best parameters for model
best <- auto.arima(FSdiff,
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
sarima(FSdiff, 4, 1, 1, 0, 1, 1, 12)
sarima.for(FSdiff, n.ahead = 12, p = 4, d = 1, q = 1, P = 0, D = 1, Q = 1, S = 12)
