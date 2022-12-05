# Load zone
library(tidyverse)
library(forecast)
library(lubridate)
library(gridExtra)
source("/Users/ashersavin/Desktop/ECON422/subset_by_date.R")

# Import data
JOB <- read_csv("PAYEMS.csv")

# Subset by date - excluding old data and COVID data
JOB.ts <- subset_by_date(JOB, c(1960,1), c(2022,1), freq = 12, lags = "fd")
ggtsdisplay(window(JOB.ts, end = c(2020,1)), lag.max = 48)

# ARMA model + remove seasonality
JOB.ar <- auto.arima(window(JOB.ts, end = c(2020,1)), d = 0, seasonal = FALSE, trace = TRUE, ic = "aic")
checkresiduals(JOB.ar)

# Forecast using ARMA model
JOB.for <- forecast(JOB.ts, model = JOB.ar, h = 12, level = .95)

# Graph forecast
autoplot(window(JOB.ts, start = 2021), ylab = "Monthly Change") +
  autolayer(JOB.for, showgap = FALSE)

# Calculate probability of over 300,000 in Feb
pnorm(q = 300, mean = 473.38, sd = 135.0112, lower.tail = FALSE)

# Had cool grid graph made but R broke and it got deleted:(
as.data.frame(JOB.for)
