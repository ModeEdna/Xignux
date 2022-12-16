# load packages to use
library(astsa)
library(readxl)
library(tidyverse)

# load excel from desktop, select the Amount column
botanas <- read_excel("Desktop/Prueba R Tesoreria.xlsx", sheet = 4)
botanas1 <- botanas %>% select(Amount)
print(botanas1)

# turn data into a time series and check to see if it changed format
botanasts <- ts(botanas1, start = 0, end = 65, frequency = 1)
is.ts(botanasts)
print(botanasts)

# plot the data with vertical lines at 12-month interval
tsplot(x = botanasts, ylim = c(0, 3000000), 
       main = "Dólares por mes por año", xlab = "Número del mes", ylab = "Dólares", xaxt = "n", yaxt = "n")
axis(1, at=seq(0, 66, by=12))
marks <- c(0, 500000, 1000000, 1500000, 2000000, 2500000, 3000000)
axis(2, at=marks, labels=format(marks, scientific=FALSE))
abline(v=c(11, 23, 35, 47, 59, 71), col = "red")

# look at the differentiated and log versions of the time series
plot(diff(botanasts))
plot(diff(log(botanasts)))

# fit the models and forecast with normal data
acf2(botanasts)
sarima(botanasts, 1, 0, 0)
sarima.for(botanasts, n.ahead = 12, p = 1, d = 0, q = 0)
sarima(botanasts, 1, 0, 0 ,0 ,1, 1, 12)
sarima.for(botanasts, n.ahead = 12, p = 1, d = 0, q = 0, P = 0, D = 1, Q = 1, S = 12)

# fit the models and forecast with differentiated data
botanasdiff <- diff(botanasts)
acf2(botanasdiff)
sarima(botanasdiff, 1, 0, 2, 0, 1, 1, 12)
sarima.for(botanasdiff, n.ahead = 12, p = 1, d = 0, q = 2, P = 0, D = 1, Q = 1, S = 12)

# fit the models and forecast with differentiated and log data
botanaslogdiff <- diff(log(botanasts))
acf2(botanaslogdiff)
sarima(botanaslogdiff, 1, 0, 1)
sarima.for(botanaslogdiff, n.ahead = 12, 1, 0, 1)
sarima(botanaslogdiff, 1, 0, 1, 0, 1, 1, 12)
sarima.for(botanaslogdiff, n.ahead = 12, 1, 0, 1, 0, 1, 1, 12)
