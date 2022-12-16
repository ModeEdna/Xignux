library(astsa)
library(tidyverse)
library(readxl)
library(forecast)
library(tseries)

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
' plot(diff(log(totalests))) '

# fit the models and forecast with normal data
acf2(diff(totalests))

# dickey fuller test
adf.test(diff(totalests))
' if p-value is less than 0.05 then it is stationary '

# auto arima checking
difftotales <- diff(totalests)
results <- auto.arima(difftotales,
                      d = 1,
                      max.p = 4,
                      max.q = 4,
                      max.P = 3,
                      max.Q = 3,
                      max.order = 5,
                      max.d = 2,
                      max.D = 2,
                      allowdrift = TRUE)
print(results)

# checking model
sarima(difftotales, 4, 1, 0)
sarima.for(difftotales, n.ahead = 12, 4, 1, 0)

# auto arima checking with seasonal
results2 <- auto.arima(difftotales,
                      d = 1,
                      max.p = 4,
                      max.q = 4,
                      max.P = 3,
                      max.Q = 3,
                      max.order = 5,
                      max.d = 2,
                      max.D = 2,
                      allowdrift = TRUE,
                      stepwise = FALSE,
                      approximation = FALSE)
print(results2)

# checking model with seasonality
sarima(difftotales, 4, 1, 0, 1, 0, 1, 12)
sarima.for(difftotales, n.ahead = 12, 4, 1, 0, 1, 0, 1, 12)
