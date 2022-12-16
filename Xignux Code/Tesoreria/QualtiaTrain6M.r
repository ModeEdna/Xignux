library(astsa)
library(dplyr)
library(tidyverse)
library(zoo)
library(lubridate)
library(tseries)
library(TTR)
library(ggplot2)
library(scales)
library(gridExtra)
library(grid)

options(scipen = 999)

# Global parameters

business <- 'Qualtia'
prediction_date <- '2021-08-01'
year_start <- 2016
month_start <- 01
prediction_date_error <- '2021-02-01'
graph_line <- 2021 + (.10/.12) * .8
  
# Create variables date and month for predictions purposes

year_month <- seq(as.Date(prediction_date), by = "month", length.out = 12) %>% 
  data.frame() %>% 
  mutate(Date = as.numeric(format(as.Date(.), '%Y%m%d')),
         Month = substr(Date, 5, 6),
         Year = substr(Date, 1, 4)) %>% 
  select(Month, Year)

# Load data and identify only amount column

data <- read_csv(paste0("C:/Users/leonardo.verduzco/OneDrive - Xignux Corp/Documents/Dynamic Exposition/", business, "_Data_v2.csv")) %>% 
  select(Amount) %>% 
  mutate(Amount = as.double(Amount))

# Convert data into time series (NexT Step = Parametrization start argument)

data_ts <- ts(data, start = c(year_start, month_start) , frequency = 12)

plot.ts(data_ts/1000000)

# Decompose seasonal data

plot(decompose(data_ts/1000000))

# Obtain differentiated time series

data_diff <- diff(data_ts)

plot.ts(data_diff/1000000)

# Verify ACF and PACF

acf2(data_diff)

# Fit model with different hyperparameter p, q and P and obtain predictions

result_df <- data.frame(p = as.integer(), 
                        q = as.integer(),
                        P = as.integer())

prediction_df <- data.frame(p = as.integer(), 
                            q = as.integer(),
                            P = as.integer(),
                            Month = as.character(),
                            Year = as.character(),
                            Prediction = as.double(),
                            Standard_Error = as.double(),
                            Standard_Error_2 = as.double(),
                            Standard_Error_N = as.double(),
                            Standard_Error_2_N = as.double(),
                            Real_Amount = as.double())

year_month <- seq(as.Date(prediction_date_error), by = "month", length.out = 6) %>% 
  data.frame() %>% 
  mutate(Date = as.numeric(format(as.Date(.), '%Y%m%d')),
         Month = substr(Date, 5, 6),
         Year = substr(Date, 1, 4)) %>% 
  select(Month, Year)

data <- read_csv(paste0("C:/Users/leonardo.verduzco/OneDrive - Xignux Corp/Documents/Dynamic Exposition/", business, "_Data_v2.csv")) %>% 
  select(Amount) %>% 
  mutate(Amount = as.double(Amount))

data_model <- data[1:(nrow(data) - 6), 1]

data_evaluation <- data[(nrow(data)-5):nrow(data), 1]

data_model <- ts(data_model, start = c(year_start, month_start), frequency = 12)

data_model_diff <- diff(data_model)

for (p in 1:3) for (q in 1:3) for (P in 0:1) {
  
  model_run <- sarima(data_model_diff, p = p, d = 1, q = q, P = P, D = 0,Q = 1, S = 12)
  result_df_ <- data.frame(p, q, P)
  result_df <- rbind(result_df, result_df_)
  
  prediction <- sarima.for(data_model_diff, n.ahead = 6, p = p, d = 1, q = q, P = P, D = 0, Q = 1, S = 12)
  prediction_df_ <- data_model[nrow(data_model)] + cumsum(prediction$pred)
  prediction_df_ <- data.frame(p, q, P, year_month$Month, year_month$Year, prediction_df_, prediction$se, prediction$se * 2, prediction$se * -1, prediction$se * -2)
  prediction_df_ <- bind_cols(prediction_df_, data_evaluation)
  prediction_df_ <- prediction_df_ %>%  rename(p = p, q = q, P = P, Month = year_month.Month, Year = year_month.Year, 
           Prediction = prediction_df_, Standard_Error = prediction.se...7,
           Standard_Error_2 = prediction.se...8, Standard_Error_N = prediction.se....9,
           Standard_Error_2_N = prediction.se....10, Real_Amount = Amount)
  prediction_df = bind_rows(prediction_df, prediction_df_)
  
  
  }

pred_results <- prediction_df %>% 
  select(p, q, P, Prediction, Real_Amount) %>% 
  mutate(Error = Prediction - Real_Amount) %>% 
  group_by(p, q, P) %>% 
  summarise(MSE = (sum(Error))**2,
            MAPE = mean(abs(Error / Real_Amount))) %>% 
  arrange(MSE, MAPE)

# Obtain predictions with champion model

year_month <- seq(as.Date(prediction_date), by = "month", length.out = 12) %>% 
  data.frame() %>% 
  mutate(Date = as.numeric(format(as.Date(.), '%Y%m%d')),
         Month = substr(Date, 5, 6),
         Year = substr(Date, 1, 4)) %>% 
  select(Month, Year)

model <- sarima(data_diff, p = pred_results$p[1], d = 1, q = pred_results$q[1], P = pred_results$P[1], D = 0,Q = 1, S = 12)

results <- sarima.for(data_diff, n.ahead = 12, p = pred_results$p[1], d = 1, q = pred_results$q[1], P = pred_results$P[1], D = 0, Q = 1, S = 12)

results_ts <- data_ts[nrow(data)] + cumsum(results$pred)

results_df <- data.frame(year_month$Month, year_month$Year, results_ts, results$se, results$se * 2, results$se * -1, results$se * -2)

results_df <- results_df %>% 
  rename(Month = year_month.Month, Year = year_month.Year, 
         Prediction = results_ts, Standard_Error = results.se,
         Standard_Error_2 = results.se...2, Standard_Error_N = results.se....1,
         Standard_Error_2_N = results.se....2)


# Graph champion model
  
results_ts <- data.frame(results_ts)

results_ts <- ts(results_ts, start = 0, end = 11, frequency = 1)

data_w_pred <- ts(c(data_ts, results_ts), start = c(year_start, month_start) , frequency = 12)

plot.ts(data_w_pred)

abline(v = graph_line, col = "red")

print(data_w_pred)

# Create ggplot for presentation purposes

data <- read_csv(paste0("C:/Users/leonardo.verduzco/OneDrive - Xignux Corp/Documents/Dynamic Exposition/", business, "_Data_v2.csv")) %>% 
  mutate(Date = as.character((as.Date(Date, format = '%d/%m/%Y'))),
         Month = substr(Date, 6, 7),
         Year = substr(Date, 1, 4),
         Prediction = '',
         Standard_Error = '',
         Standard_Error_2 = '',
         Standard_Error_N = '',
         Standard_Error_2_N = '') %>% 
  select(Month, Year, Amount, Prediction, Standard_Error, Standard_Error_2, Standard_Error_N, Standard_Error_2_N)

data_graph <- rbind(data, results_df %>% 
                      mutate(Amount = '') %>% 
                      select(Month, Year, Amount, Prediction, Standard_Error, Standard_Error_2, Standard_Error_N, Standard_Error_2_N))

data_graph <- data_graph %>% 
  mutate(Prediction = as.double(Prediction),
         Amount = as.double(Amount),
         Standard_Error = as.double(Standard_Error),
         Standard_Error_2 = as.double(Standard_Error_2),
         Standard_Error_N = as.double(Standard_Error_N),
         Standard_Error_2_N = as.double(Standard_Error_2_N),
         Year_Month = paste0(Year, Month))

data_graph %>% 
  ggplot(aes(x = Year_Month, y = Amount)) +
  geom_ribbon(aes(ymin = Prediction + Standard_Error_N,
                  ymax = Prediction + Standard_Error,
                  group = 1), 
              fill = "dark gray", col = 'dark gray', alpha = 0.5) +
  geom_line(group = 1, color = 'blue') +
  geom_ribbon(aes(ymin = Prediction + Standard_Error_2_N,
                  ymax = Prediction + Standard_Error_2,
                  group = 1), 
              fill = "dark gray", col = 'light gray', alpha = 0.5) +
  geom_line(group = 1, color = 'blue') +
  geom_line(aes(y = Prediction), group = 1, color = 'dark red') +
  geom_line(aes(y = Prediction + Standard_Error), group = 1, color = ' dark gray') +
  geom_line(aes(y = Prediction + Standard_Error_2), group = 1, color = 'light gray') +
  geom_line(aes(y = Prediction + Standard_Error_N), group = 1, color = ' dark gray') +
  geom_line(aes(y = Prediction + Standard_Error_2_N), group = 1, color = ' light gray') +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_discrete(breaks = c('201601', '201604', '201607', '201610',
                              '201701', '201704', '201707', '201710',
                              '201801', '201804', '201807', '201810',
                              '201901', '201904', '201907', '201910',
                              '202001', '202004', '202007', '202010',
                              '202101', '202104', '202107', '202110',
                              '202201', '202204', '202207')) +
  labs(title = paste0(business, ' Exposición Dinámica Serie de Tiempo'),
       x ="Año - Mes", y = "$") +
  scale_y_continuous(labels = comma)

grid.newpage()

data_graph %>% 
  select(Year_Month, Prediction, Standard_Error) %>% 
  filter(!is.na(Prediction)) %>% 
  mutate(Predicción = format(Prediction, format = 'd', big.mark = ','),
         Error_Estandar = format(Standard_Error, format = 'd', big.mark = ','),
         Año_Mes = Year_Month) %>% 
  select(Año_Mes, Predicción, Error_Estandar) %>% 
  grid.table()