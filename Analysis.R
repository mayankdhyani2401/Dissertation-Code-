install.packages("readxl")
library(readxl)

data <- read_excel("C:/Users/mayan/OneDrive/Desktop/Data Analysis/Final.xlsx")


head(data)       # View the first few rows of the dataset
summary(data)    # Summary statistics for each variable

ts_data <- ts(data[, c("Close", "Value.IIP", "Value.CPI", "Value.EX")], start = c(2018, 1), frequency = 12)
summary(ts_data)


# Graphical representation
library(forecast)
autoplot(ts_data)

# Tabular representation
table(ts_data)

# Load the e1071 package for skewness and kurtosis
install.packages("e1071")
library(e1071)

# Calculate skewness
skewness_close <- skewness(ts_data[, "Close"])
skewness_iip <- skewness(ts_data[, "Value.IIP"])
skewness_cpi <- skewness(ts_data[, "Value.CPI"])
skewness_ex <- skewness(ts_data[, "Value.EX"])

# Calculate kurtosis
kurtosis_close <- kurtosis(ts_data[, "Close"])
kurtosis_iip <- kurtosis(ts_data[, "Value.IIP"])
kurtosis_cpi <- kurtosis(ts_data[, "Value.CPI"])
kurtosis_ex <- kurtosis(ts_data[, "Value.EX"])

# Print skewness and kurtosis
print(paste("Skewness (Close):", skewness_close))
print(paste("Kurtosis (Close):", kurtosis_close))
print(paste("Skewness (Value.IIP):", skewness_iip))
print(paste("Kurtosis (Value.IIP):", kurtosis_iip))
print(paste("Skewness (Value.CPI):", skewness_cpi))
print(paste("Kurtosis (Value.CPI):", kurtosis_cpi))
print(paste("Skewness (Value.EX):", skewness_ex))
print(paste("Kurtosis (Value.EX):", kurtosis_ex))

# Calculate coefficient of dispersion (COD)
cod_close <- sd(ts_data[, "Close"]) / mean(ts_data[, "Close"])
cod_iip <- sd(ts_data[, "Value.IIP"]) / mean(ts_data[, "Value.IIP"])
cod_cpi <- sd(ts_data[, "Value.CPI"]) / mean(ts_data[, "Value.CPI"])
cod_ex <- sd(ts_data[, "Value.EX"]) / mean(ts_data[, "Value.EX"])

# Print COD
print(paste("COD (Close):", cod_close))
print(paste("COD (Value.IIP):", cod_iip))
print(paste("COD (Value.CPI):", cod_cpi))
print(paste("COD (Value.EX):", cod_ex))


install.packages("tseries")
library(tseries)
adf.test(ts_data[, "Close"], alternative = "stationary")
adf.test(ts_data[, "Value.IIP"], alternative = "stationary")
adf.test(ts_data[, "Value.CPI"], alternative = "stationary")
adf.test(ts_data[, "Value.EX"], alternative = "stationary")

# Perform differencing on the entire dataset
ts_data_diff <- diff(ts_data)

# Check the structure of the differenced dataset
head(ts_data_diff)
summary(ts_data_diff)

# Calculate skewness
skewness_close <- skewness(ts_data_diff[, "Close"])
skewness_iip <- skewness(ts_data_diff[, "Value.IIP"])
skewness_cpi <- skewness(ts_data_diff[, "Value.CPI"])
skewness_ex <- skewness(ts_data_diff[, "Value.EX"])

# Calculate kurtosis
kurtosis_close <- kurtosis(ts_data_diff[, "Close"])
kurtosis_iip <- kurtosis(ts_data_diff[, "Value.IIP"])
kurtosis_cpi <- kurtosis(ts_data_diff[, "Value.CPI"])
kurtosis_ex <- kurtosis(ts_data_diff[, "Value.EX"])

# Print skewness and kurtosis
print(paste("Skewness (Close):", skewness_close))
print(paste("Kurtosis (Close):", kurtosis_close))
print(paste("Skewness (Value.IIP):", skewness_iip))
print(paste("Kurtosis (Value.IIP):", kurtosis_iip))
print(paste("Skewness (Value.CPI):", skewness_cpi))
print(paste("Kurtosis (Value.CPI):", kurtosis_cpi))
print(paste("Skewness (Value.EX):", skewness_ex))
print(paste("Kurtosis (Value.EX):", kurtosis_ex))

# Calculate coefficient of dispersion (COD)
cod_close <- sd(ts_data[, "Close"]) / mean(ts_data[, "Close"])
cod_iip <- sd(ts_data[, "Value.IIP"]) / mean(ts_data[, "Value.IIP"])
cod_cpi <- sd(ts_data[, "Value.CPI"]) / mean(ts_data[, "Value.CPI"])
cod_ex <- sd(ts_data[, "Value.EX"]) / mean(ts_data[, "Value.EX"])

# Print COD
print(paste("COD (Close):", cod_close))
print(paste("COD (Value.IIP):", cod_iip))
print(paste("COD (Value.CPI):", cod_cpi))
print(paste("COD (Value.EX):", cod_ex))




adf_result <- adf.test(ts_data_diff[, "Close"], alternative = "stationary")
print(adf_result)

adf_result1 <- adf.test(ts_data_diff[, "Value.IIP"], alternative = "stationary")
print(adf_result1)

adf_result2 <- adf.test(ts_data_diff[, "Value.CPI"], alternative = "stationary")
print(adf_result2)

adf_result3 <- adf.test(ts_data_diff[, "Value.EX"], alternative = "stationary")
print(adf_result3)

ts_data_diff

install.packages("tsDyn")
# Load required library
library(tsDyn)

# Assuming ts_data is your time series data object
# Estimate VECM with lag order 1
vecm_model <- VECM(ts_data_diff, lag = 1)

# Print summary of VECM model
summary(vecm_model)

# Load required libraries
library(lmtest)

# Assuming ts_data_diff is your differenced time series data object

# Perform Granger causality tests for each pair of variables
granger_test_result_C_to_IIP <- grangertest(ts_data_diff[, "Close"], ts_data_diff[, "Value.IIP"])
granger_test_result_IIP_to_C <- grangertest(ts_data_diff[, "Value.IIP"], ts_data_diff[, "Close"])

granger_test_result_C_to_CPI <- grangertest(ts_data_diff[, "Close"], ts_data_diff[, "Value.CPI"])
granger_test_result_CPI_to_C <- grangertest(ts_data_diff[, "Value.CPI"], ts_data_diff[, "Close"])

granger_test_result_C_to_EX <- grangertest(ts_data_diff[, "Close"], ts_data_diff[, "Value.EX"])
granger_test_result_EX_to_C <- grangertest(ts_data_diff[, "Value.EX"], ts_data_diff[, "Close"])

# Print the test results
print(granger_test_result_C_to_IIP)
print(granger_test_result_IIP_to_C)

print(granger_test_result_C_to_CPI)
print(granger_test_result_CPI_to_C)

print(granger_test_result_C_to_EX)
print(granger_test_result_EX_to_C)

# Load required library
install.packages("urca")
library(urca)

# Assuming ts_data_diff is your differenced time series data object

# Perform Johansen cointegration test
johansen_test_result <- ca.jo(ts_data_diff, type = "trace", ecdet = "none", K = 2)

# Print summary of Johansen cointegration test
summary(johansen_test_result)
