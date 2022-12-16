library(prophet)
library(readxl)

data <- read_excel("/Users/eduardoarmenta/Desktop/WORK/Codes/Prueba R Tesoreria.xlsx", sheet = 6)
str(data)
m <- prophet(data, algorithm='LBFGS')
future <- make_future_dataframe(m, periods = 12, freq = "month")
print(future)
forecast <- predict(m, future, freq = 'month')
tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])
plot(m, forecast)




m2 <- prophet(daily.seasonality = FALSE, weekly.seasonality = FALSE, yearly.seasonality = FALSE)
m2 <- add_seasonality(m2, name="monthly", period=30.5, fourier.order = 5)
m2 <- fit.prophet(m2, data, algorithm='LBFGS')
future <- make_future_dataframe(m2, periods=12, freq='month')
forecast <- predict(m2, future)
plot(m2, forecast)
prophet_plot_components(m2, forecast)



m2 <- prophet(data, seasonality.mode = 'multiplicative', algorithm='LBFGS', growth = "linear",
              daily.seasonality = FALSE, weekly.seasonality = FALSE, yearly.seasonality = FALSE)
m2 <- fit.prophet(m2, data)
future <- make_future_dataframe(m, periods = 12, freq = 'month')
fcst <- predict(m2, future)
plot(m2, fcst)
prophet_plot_components(m2, fcst)
