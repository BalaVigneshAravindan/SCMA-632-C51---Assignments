# Load required libraries
library(tidyquant)
library(dplyr)
library(lubridate)
library(tseries)
library(forecast)
library(rugarch)

# Download data from Yahoo Finance
data <- tq_get('AAPL', from = "2010-01-01", to = "2024-07-19")

# Ensure adjusted column is an xts object and calculate returns
data_xts <- xts(data$adjusted, order.by = data$date)
returns <- dailyReturn(data_xts, type = "log")
data <- data %>% mutate(Returns = as.numeric(returns)) %>% na.omit()

library(FinTS)

# Check for ARCH/GARCH effects
arch_test <- ArchTest(data$Returns, lags = 1)
print(arch_test$p.value)

# Fit a GARCH(1,1) model
spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(0, 0))
)
model <- ugarchfit(spec = spec, data = data$Returns)

# Forecast three-month (90 days) volatility
forecasts <- ugarchforecast(model, n.ahead = 90)
volatility_forecasts <- sigma(forecasts)

# Create a data frame for plotting
data_plot <- data.frame(Date = data$date, Returns = data$Returns)
forecast_dates <- seq.Date(from = as.Date(tail(data$date, 1)), by = "days", length.out = 90)
forecast_data <- data.frame(Date = forecast_dates, Volatility = as.numeric(volatility_forecasts))

# Plot returns and forecasted volatility
plot(data_plot$Date, data_plot$Returns, type = "l", main = "AAPL Stock Returns and Forecasted Volatility", xlab = "Date", ylab = "Returns/Volatility", col = "black", ylim = c(-0.1, 0.1))
lines(forecast_data$Date, forecast_data$Volatility, col = "red")
legend("topright", legend = c("Returns", "Forecasted Volatility"), col = c("black", "red"), lty = 1, cex = 0.8)
