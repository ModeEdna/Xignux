# load packages to use
library(astsa)
library(readxl)
library(tidyverse)
library(zoo)
library(lubridate)
# library(tseries) # for adf test

# set the date and +12 months of prediction
D <- data.frame(month(Sys.Date()), year(Sys.Date()))
print(D)
D <- rename(D, Month = month.Sys.Date..., Year = year.Sys.Date...)
print(D)
D <- as.yearmon(paste0(D$Year, "-", D$Month)) + 0:11/12
print(D)
D1 <- data.frame(D)
print(D1)
D2 <- separate(D1, col = "D", into = c("Mes", "Año"), sep = " ")
print(D2)
D2[D2 == "ago."] <- ("08")
D2[D2 == "sep."] <- ("09")
D2[D2 == "oct."] <- ("10")
D2[D2 == "nov."] <- ("11")
D2[D2 == "dic."] <- ("12")
D2[D2 == "ene."] <- ("01")
D2[D2 == "feb."] <- ("02")
D2[D2 == "mar."] <- ("03")
D2[D2 == "abr."] <- ("04")
D2[D2 == "may."] <- ("05")
D2[D2 == "jun."] <- ("06")
D2[D2 == "jul."] <- ("07")

# load excel from desktop, select the Amount column
totales <- read_excel("C:/Users/Eduardo.Armenta/Desktop/Xignux/Proyectos/Tesoreria/Data/QualtiaTesoreriaPronostico.xlsx")
totales1 <- totales %>% select(Amount)
nrow(totales1)

# turn data into a time series and check to see if it changed format
totalests <- ts(totales1, start = 0, end = nrow(totales1)-1, frequency = 1)
print(totalests)

# look at the differentiated time series
totalesdiff <- diff(totalests)
# adf.test(totalesdiff)

# fit the models and make sure data is stationary
acf2(totalesdiff)
sarima(totalesdiff, 1, 1, 1, 1, 0, 1, 12)

# print undifferenced forecast
results <- sarima.for(totalesdiff, n.ahead = 12, p = 1, d = 1, q = 1, P = 1, D = 0, Q = 1, S = 12)
results1 <- totalests[nrow(totales)] + cumsum(results$pred)
dataframeResults <- data.frame(D2$Mes, D2$Año, results1, results$se, results$se*2, results$se*-1, results$se*-2)
dataframeResults <- dataframeResults %>% rename(Mes = D2.Mes, Año = D2.Año, Prediction = results1, StandardError = results.se,
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