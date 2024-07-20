# Load required libraries
library(tidyquant)
library(dplyr)
library(lubridate)

# Download data from Yahoo Finance
data <- tq_get('AAPL', from = "2010-01-01", to = "2024-07-19")

# Convert data to monthly
data$Date <- ymd(data$date)
data_monthly <- data %>% 
  mutate(Month = format(Date, "%Y-%m")) %>% 
  group_by(Month) %>% 
  summarise(Close = mean(close))

#Holt Winters Model

# Load required libraries
library(forecast)

# Convert the Month column to a date format
data_monthly$Month <- as.Date(paste0(data_monthly$Month, "-01"))

# Create a ts object with a frequency of 12 (for monthly data)
data_monthly_ts <- ts(data_monthly$Close, start = start(data_monthly$Month), frequency = 12)

# Fit a Holt Winters model to the data
model_hw <- ets(data_monthly_ts, model = "AAA")
summary(model_hw)

# Forecast for the next year
forecast_hw <- forecast(model_hw, h = 12)
plot(forecast_hw)

ARIMA Model (Daily Data)

# Fit an ARIMA model to the daily data
model_arima_daily <- auto.arima(data$close)
summary(model_arima_daily)

# Diagnostic check
checkresiduals(model_arima_daily)

# Fit a Seasonal-ARIMA (SARIMA) model
model_sarima_daily <- auto.arima(data$close, seasonal = TRUE)
summary(model_sarima_daily)

# Forecast for the next three months
forecast_arima_daily <- forecast(model_sarima_daily, h = 90)
plot(forecast_arima_daily)

ARIMA Model (Monthly Data)

# Fit an ARIMA model to the monthly data
model_arima_monthly <- auto.arima(data_monthly_ts)
summary(model_arima_monthly)

# Diagnostic check
checkresiduals(model_arima_monthly)

# Forecast for the next year
forecast_arima_monthly <- forecast(model_arima_monthly, h = 12)
plot(forecast_arima_monthly)


# Load required libraries
library(randomForest)
library(rpart)

# Create a Random Forest model
model_rf <- randomForest(close ~., data = data)

# Create a Decision Tree model
model_dt <- rpart(close ~., data = data)

# Forecast for the next three months
forecast_rf <- predict(model_rf, newdata = data)
forecast_dt <- predict(model_dt, newdata = data)
plot(forecast_dt)
plot(forecast_rf)

library(keras)
seq_len <- 10 

# Create the LSTM model
model_lstm <- keras_model_sequential()

model_lstm <- add_layer(model_lstm, layer_lstm(units = 50, return_sequences = TRUE, input_shape = c(seq_len, 1)))
model_lstm <- add_layer(model_lstm, layer_dropout(rate = 0.2))
model_lstm <- add_layer(model_lstm, layer_lstm(units = 50, return_sequences = FALSE))
model_lstm <- add_layer(model_lstm, layer_dropout(rate = 0.2))
model_lstm <- add_layer(model_lstm, layer_dense(units = 1))

# Compile the model
model_lstm %>% compile(loss = "mean_squared_error", optimizer = "adam")

# Fit the model
history <- model_lstm %>% 
  fit(
    X_train, 
    y_train, 
    epochs = 50, 
    batch_size = 1, 
    validation_split = 0.2
  )

# Evaluate the model
mse <- model_lstm %>% 
  evaluate(X_test, y_test)

cat("MSE: ", mse)

# Generate forecasts
forecast_lstm <- model_lstm %>% 
  predict(X_test)

plot(forecast_lstm)
