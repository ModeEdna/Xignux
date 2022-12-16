""" Creating an ARIMA model, forecasting, and printing forecast """
# Create ARIMA(2,1,2) model
arima = SARIMAX(amazon, order=(2,1,2))

# Fit ARIMA model
arima_results = arima.fit()

# Make ARIMA forecast of next 10 values
arima_value_forecast = arima_results.get_forecast(steps=10).predicted_mean

# Print forecast
print(arima_value_forecast)

----------------------------------------------------------------------------------------------

""" Getting the ACF and PACF plots """
# Import
from statsmodels.graphics.tsaplots import plot_acf, plot_pacf

# Create figure
fig, (ax1, ax2) = plt.subplots(2,1, figsize=(12,8))
 
# Plot the ACF of df
plot_acf(df, lags=10, zero=False, ax=ax1)

# Plot the PACF of df
plot_pacf(df, lags=10, zero=False, ax=ax2)

plt.show()

----------------------------------------------------------------------------------------------

""" Finding AIC and BIC scores with loops of many models and printing them to compare """
# Create empty list to store search results
order_aic_bic=[]

# Loop over p values from 0-2
for p in range(3):
  # Loop over q values from 0-2
    for q in range(3):
      	# create and fit ARMA(p,q) model
        model = SARIMAX(df, order=(p, 0, q))
        results = model.fit()
        
        # Append order and results tuple
        order_aic_bic.append((p, q, results.aic, results.bic))
        
  # Construct DataFrame from order_aic_bic
order_df = pd.DataFrame(order_aic_bic, 
                        columns=['p', 'q', 'AIC', 'BIC'])

# Print order_df in order of increasing AIC
print(order_df.sort_values('AIC'))

# Print order_df in order of increasing BIC
print(order_df.sort_values('BIC'))

----------------------------------------------------------------------------------------------

""" Fitting models and looking at AIC and BIC to find the best model """
# Loop over p values from 0-2
for p in range(3):
    # Loop over q values from 0-2
    for q in range(3):
      
        try:
            # create and fit ARMA(p,q) model
            model = SARIMAX(earthquake, order=(p, 0, q))
            results = model.fit()
            
            # Print order and results
            print(p, q, results.aic, results.bic)
            
        except:
            print(p, q, None, None) 

----------------------------------------------------------------------------------------------

""" Summary statistics of the model. Mean absolute error, etc... """
# Fit model
model = SARIMAX(earthquake, order=(1,0,1))
results = model.fit()

# Calculate the mean absolute error from residuals
mae = np.mean(np.abs(results.resid))

# Print mean absolute error
print(mae)

# Make plot of time series for comparison
earthquake.plot()
plt.show()

"""" Model summary """
print(modelresults.summary())

""""Plotting diagnostics of model"""
# Create and fit model
model = SARIMAX(df, order=(1, 1, 1))
results=model.fit()

# Create the 4 diagnostics plots
results.plot_diagnostics()
plt.show()
  # Meaning of plots:
  """"
  standardized results: you want there to be no patterns in the residuals
  Histogram: you want the KDE curve to be very similar to the STD
  Normal Q-Q: most of the adta points should be on the line
  Correlogram: 95% of correlations should not be significant
  """
  
----------------------------------------------------------------------------------------------
  
""" Box Jenkins method for model creation IDENTIFICATION step 1"""
# Plot time series
savings.plot()
plt.show()

# Run Dicky-Fuller test
result = adfuller(savings['savings'])

# Print test statistic
print(result[0])

# Print p-value
print(result[1]) """ if p is under .05 then the series is stationary """

""" Box Jenkins method for model creation IDENTIFICATION step 2"""
# Create figure
fig, (ax1, ax2) = plt.subplots(2,1, figsize=(12,8))
 
# Plot the ACF of savings on ax1
plot_acf(savings, lags=10, zero=False, ax=ax1)

# Plot the PACF of savings on ax2
plot_pacf(savings, lags=10, zero=False, ax=ax2)

plt.show()

""" Box Jenkins method for model creation ESTIMATION step: look over the AIC and BIC
for different model options and find the best choice"""
# Loop over p values from 0-3
for p in range(4):
  
  # Loop over q values from 0-3
    for q in range(4):
      try:
        # Create and fit ARMA(p,q) model
        model = SARIMAX(savings, order=(p, 0, q), trend='c')
        results = model.fit()
        
        # Print p, q, AIC, BIC
        print(p, q, results.aic, results.bic)
        
      except:
        print(p, q, None, None)

""" Box Jenkins method for model creation DIAGNOSTIC step: evaluate the model """
# Create and fit model
model = SARIMAX(savings, order=(1, 0, 2), trend='c')
results = model.fit()

# Create the 4 diagostics plots
results.plot_diagnostics()
plt.show()

# Print summary
print(results.summary())
  """"
  standardized results: you want there to be no patterns in the residuals
  Histogram: you want the KDE curve to be very similar to the STD
  Normal Q-Q: most of the adta points should be on the line
  Correlogram: 95% of correlations should not be significant
  """

----------------------------------------------------------------------------------------------

""" seasonal decomposition """
# Import seasonal decompose
from statsmodels.tsa.seasonal import seasonal_decompose

# Perform additive decomposition
decomp = seasonal_decompose(milk_production['pounds_per_cow'], period=12)

# Plot decomposition
decomp.plot()
plt.show()

""" seasonal differencing to find the seasonal parameters in a SARIMA model """
# Create a SARIMAX model
model = SARIMAX(df3, order=(1, 1, 0), seasonal_order=(0, 1, 1, 12))

# Fit the model
results = model.fit()

# Print the results summary
print(results.summary())

""" Example of full SARIMA model """
# Take the first and seasonal differences and drop NaNs
aus_employment_diff = aus_employment.diff().diff(12).dropna()

""" Not seasonal part"""
# Create the figure 
fig, (ax1, ax2) = plt.subplots(2,1,figsize=(8,6))

# Plot the ACF on ax1
plot_acf(aus_employment_diff, zero=False, lags=11, ax=ax1)

# Plot the PACF on ax2
plot_pacf(aus_employment_diff, zero=False, lags=11, ax=ax2)

plt.show()

""" Seasonal part"""
# Make list of lags
lags = [12, 24, 36, 48, 60]

# Create the figure 
fig, (ax1, ax2) = plt.subplots(2,1,figsize=(8,6))

# Plot the ACF on ax1
plot_acf(aus_employment_diff, lags=lags, ax=ax1)

# Plot the PACF on ax2
plot_pacf(aus_employment_diff, lags=lags, ax=ax2)

plt.show()

""" Predicting and graphing the predictions"""
# Create ARIMA mean forecast
arima_pred = arima_results.get_forecast(steps=25)
arima_mean = arima_pred.predicted_mean

# Create SARIMA mean forecast
sarima_pred = sarima_results.get_forecast(steps=25)
sarima_mean = sarima_pred.predicted_mean

# Plot mean ARIMA and SARIMA predictions and observed
plt.plot(dates, sarima_mean, label='SARIMA')
plt.plot(dates, arima_mean, label='ARIMA')
plt.plot(wisconsin_test, label='observed')
plt.legend()
plt.show()

----------------------------------------------------------------------------------------------
""" Searching over model orders wiht for loops """
import pmdarima as pm
# Non-seasonal orders to test
results = pm.auto_arima(df,
                        d=0,          # non-seasonal difference order
                        start_p=1,    # initial guess for p
                        start_q=1,    # initial quess for q
                        max_p=3,      # max value of p to test
                        max_q=3)      # max value of q to test
                        
# Seasonal orders to test
results = pm.auto_arima(df,
                        seasonal=True, # if seasonal is true
                        m=7,           # the seasonal period
                        D=1            # seasonal difference order
                        start_p=1,     # initial guess for p
                        start_q=1,     # initial quess for q
                        max_p=3,       # max value of p to test
                        max_q=3)       # max value of q to test
                        
# Parameters not involving order
results = pm.auto_arima(df,
                        information_criterion='True'aic, # iused to select best model
                        trace=True,                      # print results while training
                        error_action='ignore',           # ignore orders that don't work
                        stepwise=True)                   # searches from the initial guess
                        
print(results.summary())

----------------------------------------------------------------------------------------------
''' ADFuller test checks if the data is stationary. If p-value is less than 0.05, then
it is stationary '''
# Import augmented dicky-fuller test function
from statsmodels.tsa.stattools import adfuller

# Run test
result = adfuller(earthquake['earthquakes_per_year'])

# Print test statistic
print(result[0])

# Print p-value
print(result[1])

# Print critical values
print(result[4]) 
