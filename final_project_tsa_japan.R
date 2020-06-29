# loading packages

library("forecast")
library("tseries")
library("Kendall")
library("smooth")

# preparing data set
ev <- read.csv("final_project_tsa/NEV Sales.csv")
colnames(ev)[1] <- "Date"
ev$Date <- as.character(ev$Date)
ev$Date <- as.Date(ev$Date, format = "%b-%y")

japan <- ev[c(1:65),-c(2,3)]
nobs_full <- nrow(japan)
japan_ts_full <- ts(japan[,2], start = c(2014,1), end=c(2019,5), frequency = 12)

# creating a clone dataset with less points
japan_ts <- ts(japan[,2],start=c(2014,1), end=c(2018,5),frequency=12) 
nobs <- nrow(japan[c(1:53),])
t <- 1:nobs

# ACF and PACF plots
par(mfrow=c(1,2))
acf <- Acf(japan_ts, lag.max = 40, main = "")
pacf <- Pacf(japan_ts, lag.max = 40, main = "")

# Exploratory Analysis
par(mfrow=c(1,1))
summary(japan_ts)
plot(t, japan_ts, type = 'l', ylab = "EV sales", xlab = "Months",
     main = "Japan EV Sales (Jan 2014 - May 2018)")

# Deseasoning 
japan_decompose <- decompose(japan_ts)
plot(japan_decompose)

# ADF and Mann Kendall tests
adf.test(japan_ts, alternative = "stationary")
summary(MannKendall(japan_ts))
#### so there is no stationarity

# Model 1: Naive Forecast
naive <- naive(japan_ts, h=259) #384, 396
plot(naive, ylim = c(0,65000))
accuracy(naive$mean, japan_ts_full[(nobs_full-12):nobs_full])
plot(naive$residuals, main = "Naive")
abline(h = 0, lty = 2)

# Model 1.5: Simple Moving Average
ma=sma(japan_ts, h=259, holdout = FALSE,silent=FALSE, order=2, xlab = "EV Sales")
plot(ma)
accuracy(ma$forecast, japan_ts_full[(nobs_full-12):nobs_full])
plot(ma$residuals, main = "Simple Moving Average")
abline(h = 0, lty = 2)

# Model 2: ARIMA
arima <- auto.arima(japan_ts, max.P = 0, max.D = 0, max.Q = 0)
summary(arima)
arima.forecast <- forecast(arima, h = 259)
plot(arima.forecast)
accuracy(arima.forecast$mean, japan_ts_full[(nobs_full-12):nobs_full])
plot(arima.forecast$residuals, main = "ARIMA")

# Model 3: Exponential smoothing
exp_smooth <- es(japan_ts, model = 'ZZZ', h = 259, holdout = FALSE)
summary(exp_smooth)
plot(exp_smooth)
accuracy(exp_smooth$forecast, japan_ts_full[(nobs_full-12):nobs_full])
plot(exp_smooth$residuals, main = "ES Model")

# fixed=c(0,12,0.001,NA))
# Model 4: BSM State Space (least MAPE)
bsm_1 <- StructTS(japan_ts, type="BSM", fixed=c(0.1,0.1,NA,NA))
bsm_1.forecast <- forecast(bsm_1, h = 259)
plot(bsm_1.forecast, ylim = c(0,10000), ylab = "EV Sales", xlab = "Years",
     main = "SSM - I")
accuracy(bsm_1.forecast$mean, japan_ts_full[(nobs_full-12):nobs_full])

# Model 4.5: BSM State Space (better residuals)
bsm_2 <- StructTS(japan_ts, type="BSM", fixed=c(0.1,1000,NA,NA))
bsm_2.forecast <- forecast(bsm_2, h = 259)
plot(bsm_2.forecast, ylim = c(0,30000))
accuracy(bsm_2.forecast$mean, japan_ts_full[(nobs_full-12):nobs_full])

par(mfrow=c(1,2))
plot(bsm_1.forecast, ylim = c(0,10000), ylab = "EV Sales", xlab = "Years",
     main = "SSM - I")
plot(bsm_2.forecast, ylim = c(0,30000), ylab = "EV Sales", xlab = "Years",
     main = "SSM - II")

par(mfrow=c(1,2))
plot(bsm_1$residuals, ylab = "Residuals", xlab = "Years",
     main = "SSM - I")
abline(h = 0, lty = 2)
plot(bsm_2$residuals, ylab = "Residuals", xlab = "Years",
     main = "SSM - II")
abline(h = 0, lty = 2)

par(mfrow=c(2,2))
Acf(bsm_2.forecast$residuals)
Pacf(bsm_2.forecast$residuals)

#comparing forecasts
par(mfrow=c(1,1))
plot(japan_ts_full,col="black", type="l", xlab = "Years", ylab = "EV Sales")
lines(naive$mean, col="blue", lwd = 2)
lines(ma$forecast, col="cyan", lwd = 2)
lines(arima.forecast$mean, col="green", lwd = 2)
lines(exp_smooth$forecast, col="orange",lwd = 2)
lines(bsm_1.forecast$mean, col = "red", lwd =2)
lines(bsm_2.forecast$mean, col = "red", lty = 2, lwd = 2)
abline(v = 2018.42, lty = 2 )
legend("topleft",legend=c("Original Japan EV sales", "Naive", "Moving Average", "ARIMA(0,1,2)","Exponential Smoothing", "SSM - I", "SSM - II"), lty=c(1,1,1,1,1,1,2),
       col=c("black","blue","cyan", "green","orange","red", "red"))

# Forecast to dataframe
cars_2019 <- 5195216

# SSM-I forecasting
ev_sales_2040 <- data.frame()
ev_sales_2040 <- bsm_1.forecast$mean       
ev_sales_2040 <- as.data.frame(ev_sales_2040)
forecast_length <- nrow(ev_sales_2040)
ev_sum_2040 <- sum(ev_sales_2040[c((forecast_length-12):forecast_length),1])
ev_sum_2040 / cars_2019 * 100

# SSM-II forceasting

ev_sales_2040_2 <- data.frame()
ev_sales_2040_2 <- bsm_2.forecast$mean       
ev_sales_2040_2 <- as.data.frame(ev_sales_2040_2)
forecast_length <- nrow(ev_sales_2040_2)
ev_sum_2040_2 <- sum(ev_sales_2040_2[c((forecast_length-12):forecast_length),1])
ev_sum_2040_2 / cars_2019 * 100
