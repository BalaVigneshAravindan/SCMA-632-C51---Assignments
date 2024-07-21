# Install and load required packages
library(vars)
library(urca)

# Load data
data <- readxl::read_excel("C:\\Users\\Bala Vignesh.A\\Downloads\\CMO-Historical-Data-Annual.xlsx", sheet = "Sheet 1")

# Set index to Year
data <- ts(data, start = data$Year[1], end = data$Year[nrow(data)], frequency = 1)

# Select commodities of interest
selected_commodities <- c("Crude oil, average", "Sugar, world", "Gold", "Silver", "Wheat, US SRW", "Soybeans")
data_selected <- data[, selected_commodities]

# Check for stationarity using ADF test
adf_test <- function(timeseries) {
  test <- urca::ur.df(timeseries, type = "trend", lags = 1)
  return(test)
}

adf_results <- sapply(data_selected, adf_test)
print(adf_results)

# Fit VAR model
var_model <- VAR(data_selected)
summary(var_model)

# Fit VECM model
vecm_model <- ca.jo(data_selected, type = "eigen", ecdet = "none", spec = "longrun")
summary(vecm_model)

