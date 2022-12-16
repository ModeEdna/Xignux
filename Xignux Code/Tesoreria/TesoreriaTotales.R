# load packages to use
library(astsa)
library(readxl)
library(tidyverse)

# load excel from desktop, select the Amount column
totales <- read_excel("/Users/eduardoarmenta/Desktop/WORK/Codes/Prueba R Tesoreria.xlsx", sheet = 5)
totales1 <- totales %>% select(Amount)
print(totales1)

# turn data into a time series and check to see if it changed format
totalests <- ts(totales1, start = 0, end = 65, frequency = 1)
is.ts(totalests)
print(totalests)

# plot the data with vertical lines at 12-month interval
tsplot(x = totalests, ylim = c(0, 30000000), 
       main = "Dólares por mes por año", xlab = "Número del mes", ylab = "Dólares", xaxt = "n", yaxt = "n")
axis(1, at=seq(0, 66, by=12))
marks <- c(0, 5000000, 10000000, 15000000, 20000000, 25000000, 30000000)
axis(2, at=marks, labels=format(marks, scientific=FALSE))
abline(v=c(11, 23, 35, 47, 59, 71), col = "red")

# look at the differentiated and log versions of the time series
plot(diff(totalests))
plot(diff(log(totalests)))

# fit the models and forecast with normal data
acf2(totalests)
sarima(totalests, 1, 0, 0)
sarima.for(totalests, n.ahead = 12, p = 1, d = 0, q = 0)
sarima(totalests, 1, 0, 0 ,0 ,1, 1, 12)
sarima.for(totalests, n.ahead = 12, p = 1, d = 0, q = 0, P = 0, D = 1, Q = 1, S = 12)

# fit the models and forecast with differentiated data
totalesdiff <- diff(totalests)
acf2(totalesdiff)
sarima(totalesdiff, 1, 1, 1, 0, 1, 1, 12)
sarima.for(totalesdiff, n.ahead = 12, p = 1, d = 1, q = 1, P = 0, D = 1, Q = 1, S = 12)

# fit the models and forecast with differentiated and log data
totaleslogdiff <- diff(log(totalests))
acf2(totaleslogdiff)
sarima(totaleslogdiff, 1, 0, 1)
sarima.for(totaleslogdiff, n.ahead = 12, 1, 0, 1)
sarima(totaleslogdiff, 1, 2, 1, 0, 1, 1, 12)
sarima.for(totaleslogdiff, n.ahead = 12, 1, 2, 1, 0, 1, 1, 12)
