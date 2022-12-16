# load packages to use
library(astsa)
library(readxl)
library(tidyverse)
library(tibble)

# load excel from desktop, select the Amount column
totales <- read_excel("/Users/eduardoarmenta/Desktop/WORK/Codes/Prueba R Tesoreria.xlsx", sheet = 5)
totales <- totales %>% column_to_rownames('Date')
totales1 <- totales %>% select(Amount)
print(totales1)
print(totales)

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

# fit the models and forecast with differentiated data
totalesdiff <- diff(totalests)
acf2(totalesdiff)
sarima(totalesdiff, 1, 1, 1, 1, 0, 1, 12)
results <- sarima.for(totalesdiff, n.ahead = 12, p = 1, d = 1, q = 1, P = 0, D = 1, Q = 1, S = 12)
print(results$pred)
print(results$se)
dataframeResults <- data.frame(c('Jul', 'Ago', 'Sep', 'Oct', 'Nov', 'Dic', 'Ene', 'Feb', 'Mar', 'Abr', 'May', 'Jun'), results$pred, results$se, results$se*2, results$se*-1, results$se*-2)
print(dataframeResults)
dataframeResults <- dataframeResults %>% rename(Fecha = c..Jul....Ago....Sep....Oct....Nov....Dic....Ene....Feb....Mar..., Prediction = results.pred, StandardError = results.se,
                                                StandardError2 = results.se...2, StandardErrorN = results.se....1,
                                                StandardError2N = results.se....2)
print(dataframeResults)
results2 <- sarima.for(totalesdiff, n.ahead = 12, p = 1, d = 1, q = 1, P = 1, D = 0, Q = 1, S = 12)
undiff <- totalests[65] + cumsum(results2$pred)
tsplot(undiff, ylim = c(10000000, 20000000), main = "Dolares por mes", ylab = "Dolares")
undiffData <- data.frame(undiff)
undiffDatats <- ts(undiffData, start = 0, end = 11, frequency = 1)
print(undiffDatats)
is.ts(undiffDatats)
allTogether <- ts(c(totalests, undiffDatats), start = start(totalests), frequency = frequency(totalests))
tsplot(allTogether, ylim = c(0, 30000000), fill="undiffDatats")
abline(v=63.5, col = "red")


