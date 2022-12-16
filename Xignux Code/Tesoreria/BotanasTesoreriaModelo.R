# load packages to use
library(astsa)
library(readxl)
library(tidyverse)
library(tseries)
library(forecast)

# load excel from desktop, select the Amount column
B <- read_excel("C:/Users/Eduardo.Armenta/Desktop/Xignux/Proyectos/Tesoreria/Data/BotanasTesoreriaPronostico.xlsx")
B1 <- B %>% select(Amount)
print(B1)

# turn data into a time series and check to see if it changed format
Bts <- ts(B1, start = 0, end = 65, frequency = 1)
is.ts(Bts)
print(Bts)

# plot the data with vertical lines at 12-month interval
tsplot(x = Bts, ylim = c(0, 2500000), 
       main = "Dolares por mes por año", xlab = "Numero del mes", ylab = "Dolares", xaxt = "n", yaxt = "n")
axis(1, at=seq(0, 60, by=12))
marks <- c(0, 1000000, 1500000, 2000000, 2500000)
axis(2, at=marks, labels=format(marks, scientific=FALSE))
abline(v=c(11, 23, 35, 47, 59, 71), col = "red")

# look at the differentiated and log versions of the time series
Bdiff <- diff(Bts)
plot(Bdiff)
adf.test(Bdiff) #smaller than 0.05 means stationary

# fit the models and forecast with normal data
acf2(Bts)
sarima(Bts, 1, 0, 0)
sarima.for(Bts, n.ahead = 12, p = 1, d = 0, q = 0)
sarima(Bts, 1, 0, 0, 0, 1, 1, 12)
sarima.for(Bts, n.ahead = 12, p = 1, d = 0, q = 0, P = 0, D = 1, Q = 1, S = 12)

# fit the models and forecast with differentiated data
acf2(Bdiff)
sarima(Bdiff, 2, 1, 1, 0, 1, 1, 12)
resultsB <- sarima.for(Bdiff, n.ahead = 12, p = 2, d = 1, q = 1, P = 0, D = 1, Q = 1, S = 12)

# print undifferenced forecast
print(resultsB$pred)
print(resultsB$se)
dataframeresultsB <- data.frame(c('Jul', 'Ago', 'Sep', 'Oct', 'Nov', 'Dic', 'Ene', 'Feb', 'Mar', 'Abr', 'May', 'Jun'), resultsB$pred, resultsB$se, resultsB$se*2, resultsB$se*-1, resultsB$se*-2)
print(dataframeresultsB)
dataframeresultsB <- dataframeresultsB %>% rename(Fecha = c..Jul....Ago....Sep....Oct....Nov....Dic....Ene....Feb....Mar..., Prediction = resultsB.pred, StandardError = resultsB.se,
                                                    StandardError2 = resultsB.se...2, StandardErrorN = resultsB.se....1,
                                                    StandardError2N = resultsB.se....2)
print(dataframeresultsB)
resultsB2 <- sarima.for(Bdiff, n.ahead = 12, p = 1, d = 1, q = 1, P = 0, D = 1, Q = 1, S = 12)
undiff <- Bts[66] + cumsum(resultsB2$pred)
print(Bts[66])
tsplot(undiff, ylim = c(0, 2500000), main = "Dolares por mes", ylab = "Dolares")
undiffData <- data.frame(undiff)
undiffDatats <- ts(undiffData, start = 0, end = 11, frequency = 1)
print(undiffDatats)
is.ts(undiffDatats)
allTogether <- ts(c(Bts, undiffDatats), start = start(Bts), frequency = frequency(Bts))
tsplot(allTogether, ylim = c(0, 2500000), fill="undiffDatats")
abline(v=65.5, col = "red")

# auto-find best parameters for model
best <- auto.arima(Bdiff,
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
sarima(Bdiff, 4, 1, 1, 0, 1, 1, 12)
sarima.for(Bdiff, n.ahead = 12, p = 4, d = 1, q = 1, P = 0, D = 1, Q = 1, S = 12)
