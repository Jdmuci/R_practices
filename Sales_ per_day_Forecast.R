### Sales Forecast, example


# This EXAMPLE shows in a simple way how to use some popular methods for Forecast:
# Benchmark Method (Seasonal Naive Method), Exponential Smoothing Model and ARIMA Model.

# This data is not adjusted for inflation. So these are nomimal values. (practice excersice)
# Monthly retail sales per day


library(fpp2)
library(ggplot2)
library(readxl)

# Load data
data2 <- read.csv("https://raw.githubusercontent.com/Jdmuci/R_practices/main/sales_per_day.csv")


# declare as time series data
Y <- ts(data[,2], start = c(1992,1), end = c(2017,12), frequency = 12)




###################################
#             Preliminary analysisi
###################################

# Time Plot
autoplot(Y) +
  ggtitle("Time plot: US Retail Sales Per Day") +
  ylab("Millions of dollars")


### 1- Seasonality and Trend
### Data has a strong trend and seasonality. Need to investigate transformations.

## Take the first difference to remove trend.
DY <- diff(Y)

# Time plot of differenced data:
autoplot(DY) +
  ggtitle("Time plot: Change in US Retail Sales Per Day (1st difference)") +
  ylab("Millions of dollars")



### Lets now focus on seasonality ###
# Series appears trend-stationary, use to investigate seasonality.
ggseasonplot(DY) +
  ggtitle("Seasonal plot: Change in Daily Retail Sales") +
  ylab("Millions of dollars")
# This function makes a series for each year and plot them together in a 12 months period to see the seasonality.
# Apparently there is a drop between december and january, and a rise from november to december (christmas)

# Lets look at another seasonal plot, the subseries plot
ggsubseriesplot(DY)
# This function graphs each month of the series separately. Example: every January together. Average: blue line.

# So, we confirm the fluctuations we saw in the first plot is seasonality every december and january




######################################
#           Forecast with various methods (considering seasonality).
######################################


####################
# FIRST METHOD:    Use a Benchmark method to forecast. (there are many)

# So, lets use the Seasonal naive method as our benchmark:
# y_t = y_{t-s} + e_t
# Using the differenced series (1st difference)
####################
fit <- snaive(DY)  # Residual SD = 231.45, this is our benchmark
print(summary(fit))
checkresiduals(fit)

# We see residuals ACF are not ideal, lets look if we can find a better model


###################
# SECOND METHOD:    Fit ETS method
# Another type we can try is Exponential smoothing model.
# They (exponential smoothing model) use weighted averages of past observations to forecast new values
# exponential smoothing models are based on a description of the trend and seasonality in the data
# we can use regular data (not the differenced one)
####################
fit_ets <- ets(Y)          # Residual SD = 0.0197
print(summary(fit_ets))
checkresiduals(fit_ets)

# Apparently it did not improve, still correlation left in the data and residuals look almost the same
# But it fits better, since the  Residual SD is much smaller.
# Lets do another type of model

####################
# THIRD METHOD:   Fit an ARIMA Model
# Exponential smoothing and ARIMA models are the two most widely used approaches to time series forecasting
# ARIMA models aim to describe the autocorrelation in the data.
# Needs to be stationary data

# So lets use differenced data, there is seasonality
####################

#   We can use regular data and tell it to take 1st difference (d=1) and 1st seasonality difference (D=1)
# "stepwise = FALSE" finds the model that fits better and try every single possibility, so it could take a lot of time
# "aproxximation = FALSE" will show the exactly result, not an approximation
# "Trace" will print all the models 

fit_arima <- auto.arima(Y, d=1, D=1, stepwise = FALSE, approximation = FALSE, trace = TRUE)   # Residual SD = 174.89
#It takes around a minute to find the "best model"
print(summary(fit_arima))
checkresiduals(fit_arima)

#SD = sqrt(sigma^2)
sqrt(30589)

# This model fits better, there is not much autorrelation in residuals (ACF), but is not perfect
# It means there is better model for this time series, but lets use this model.


###     FORECAST 

####################
# Forecast with ARIMA Model
####################
fcst <- forecast(fit_arima, h=24)
autoplot(fcst, include = 60)
print(summary(fcst))

# it looks realistic 
# "h = 24" forecasted period (months)
# function Include, means the number of months (historic data)


