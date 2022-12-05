# PROJECT 3

# Load Zone
library(forecast)
library(tidyverse)
source("/Users/ashersavin/Desktop/ECON422/Functions/subset_by_date.R")
source("/Users/ashersavin/Desktop/ECON422/Functions/theil.R")

# Import Data
PCE <- read_csv("DGDSRC1.csv")

# FIRST RECESSION
# Log, and convert to time series
PCE.ts <- subset_by_date(PCE, c(1984,1), c(2021,12), freq = 12, lags = "lg")
ts1 <- window(PCE.ts, end = c(2008,12))

# ARIMA models
trend = seq_along(ts1)
auto.arima(ts1, seasonal = FALSE, d = 0, xreg = trend, stepwise = FALSE, approximation = FALSE, trace = TRUE, ic = "aic")
auto.arima(ts1, seasonal = FALSE, d = 0, xreg = trend, stepwise = FALSE, approximation = FALSE, trace = TRUE, ic = "bic")
ts1.ar1 <- Arima(ts1, c(1,0,1), xreg = trend)
checkresiduals(ts1.ar1)

auto.arima(ts1, seasonal = FALSE, stepwise = FALSE, approximation = FALSE, trace = TRUE, ic = "aic")
auto.arima(ts1, seasonal = FALSE, stepwise = FALSE, approximation = FALSE, trace = TRUE, ic = "bic")
ts1.ar2 <- Arima(ts1, order = c(0,1,1), include.drift = TRUE)
checkresiduals(ts1.ar2)

# Forecasts
fc1.det <- forecast(ts1.ar1, xreg = 300 + 1:72, level = 95)
actuals <- as.vector(window(PCE.ts, start = c(2009,1), end = c(2014,12)))
theil(actuals, fc1.det)
fc1.sto <- forecast(ts1.ar2, h = 12 * 6, level = 95)

# Anti-log data


# Graphs
ggplot(data = fortify(window(PCE.ts, start = c(2000,1), end = c(2014,12)))) +
  geom_line(aes(x = x, y = exp(y))) +
  geom_smooth(data = fortify(fc1.det$mean),
              aes(color = "blue", x = x, y = exp(y), ymax = exp(fortify(fc1.det$upper)$y),
                                ymin = exp(fortify(fc1.det$lower)$y)),
              fill = "blue",
              stat = 'identity',
              alpha = 0.2) +
  geom_smooth(data = fortify(fc1.sto$mean), 
              aes(color = "red", x = x, y = exp(y), ymax = exp(fortify(fc1.sto$upper)$y),
                                ymin = exp(fortify(fc1.sto$lower)$y)),
              fill = "red",
              stat = 'identity',
              alpha = 0.2) +
  labs(x = "Year", y = "PCE (Millions of Dollars)") +
  scale_color_identity(name = "Model",
                       breaks = c("blue", "red"),
                       labels = c("Deterministic", "Stochastic"),
                       guide = "legend") +
  ggtitle("Competing Forecasts for Consumer Expenditure of Goods: 2009 Recession")

# SECOND RECESSION
# Establish time series
ts2 <- PCE.ts

# ARIMA models
trend = seq_along(ts2)
ts2.ar1 <- auto.arima(ts2, seasonal = FALSE, d = 0, xreg = trend, stepwise = FALSE, 
                      approximation = FALSE, trace = TRUE, ic = "aic")
checkresiduals(ts2.ar1)

ts2.ar2 <- auto.arima(ts2, seasonal = FALSE, stepwise = FALSE, approximation = FALSE, 
                      trace = TRUE, ic = "aic")
checkresiduals(ts2.ar2)

# Forecasts
fc2.det <- forecast(ts2.ar1, xreg = 456 + 1:72, level = 95)
fc2.sto <- forecast(ts2.ar2, h = 12 * 6, level = 95)


# Anti-log data


# Graphs
ggplot(data = fortify(window(PCE.ts, start = c(2014,1), end = c(2021,12)))) +
  geom_line(aes(x = x, y = exp(y))) +
  geom_smooth(data = fortify(fc2.det$mean), 
              aes(color = "blue", x = x, y = exp(y), ymax = exp(fortify(fc2.det$upper)$y),
                  ymin = exp(fortify(fc2.det$lower)$y)),
              fill = "blue",
              stat = 'identity',
              alpha = 0.2) +
  geom_smooth(data = fortify(fc2.sto$mean), 
              aes(color = "red", x = x, y = exp(y), ymax = exp(fortify(fc2.sto$upper)$y),
                  ymin = exp(fortify(fc2.sto$lower)$y)),
              fill = "red",
              stat = 'identity',
              alpha = 0.2) +
  labs(x = "Year", y = "PCE (Millions of Dollars)") +
  scale_color_identity(name = "Model",
                       breaks = c("blue", "red"),
                       labels = c("Deterministic", "Stochastic"),
                       guide = "legend") +
  ggtitle("Competing Forecast of Consumer Spending on Goods: Current")
