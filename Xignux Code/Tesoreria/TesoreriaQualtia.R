# load packages to use
library(astsa)
library(readxl)
library(tidyverse)

# load excel from desktop, select the Amount column
datos <- read_excel("Desktop/Prueba R Tesoreria.xlsx", sheet = 2)
datos1 <- datos %>% select(Amount)
print(datos1)

# turn data into a time series and check to see if it changed format
datosts <- ts(datos1, start = 0, end = 65, frequency = 1)
is.ts(datosts)
print(datosts)

# plot the data with vertical lines at 12-month interval
tsplot(x = datosts, ylim = c(5000000, 30000000), 
       main = "Dólares por mes por año", xlab = "Número del mes", ylab = "Dólares", xaxt = "n", yaxt = "n")
axis(1, at=seq(0, 66, by=12))
marks <- c(5000000, 10000000, 15000000, 20000000, 25000000, 30000000)
axis(2, at=marks, labels=format(marks, scientific=FALSE))
abline(v=c(11, 23, 35, 47, 59, 71), col = "red")

# look at the differentiated and log versions of the time series
plot(diff(datosts))
plot(diff(log(datosts)))

# fit the models and forecast with normal data
acf2(datosts)
sarima(datosts, 1, 1, 0)
sarima.for(datosts, n.ahead = 12, p = 1, d = 1, q = 0)
sarima(datosts, 1, 0, 0 ,0 ,1, 1, 12)
sarima.for(datosts, n.ahead = 12, p = 1, d = 0, q = 0, P = 0, D = 1, Q = 1, S = 12)

# fit the models and forecast with differentiated data
datosdiff <- diff(datosts)
acf2(datosdiff)
sarima(datosdiff, 1, 0, 1, 0, 1, 1, 12)
sarima.for(datosdiff, n.ahead = 12, p = 1, d = 0, q = 1, P = 0, D = 1, Q = 1, S = 12)

# fit the models and forecast with differentiated and log data
datoslogdiff <- diff(log(datosts))
acf2(datoslogdiff)
sarima(datoslogdiff, 1, 0, 1)
sarima.for(datoslogdiff, n.ahead = 12, 1, 0, 1)
sarima(datoslogdiff, 1, 0, 1, 0, 1, 1, 12)
sarima.for(datoslogdiff, n.ahead = 12, 1, 0, 1, 0, 1, 1, 12)
