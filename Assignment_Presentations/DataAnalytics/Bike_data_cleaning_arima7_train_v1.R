#install.packages(c('ggplot2', 'forecast','tseries'))
library('ggplot2')
library('forecast')
library('tseries')
#setwd('C:/Users/Kriti Srivastava/Desktop/Bike-Sharing-Dataset')
#setwd('D:/NUS/DataAnalytics/Assignments/Barry')

daily_data = read.csv('./Bikeshare_Train.csv', header=TRUE, stringsAsFactors=FALSE)
daily_data$Date = as.Date(daily_data$dteday)
# Plot for cnt
ggplot(daily_data, aes(Date, cnt)) + geom_line() + scale_x_date('month')  + ylab("Daily Count") +
  xlab("")

#Plot for Casual
ggplot(daily_data, aes(Date, casual)) + geom_line() + scale_x_date('month')  + ylab("Casual Count") +
  xlab("")

#Plot for Registered
ggplot(daily_data, aes(Date, registered)) + geom_line() + scale_x_date('month')  + ylab("Registered Count") +
  xlab("")

#To timeseries
count_ts = ts(daily_data[, c('cnt')])
casual_ts = ts(daily_data[, c('casual')])
registered_ts = ts(daily_data[, c('registered')])

#Cleaning
daily_data$clean_cnt = tsclean(count_ts)
ggplot() +
  geom_line(data = daily_data, aes(x = Date, y = clean_cnt)) + ylab('Cleaned Bike Count')

daily_data$clean_casual_cnt = tsclean(casual_ts)
ggplot() +
  geom_line(data = daily_data, aes(x = Date, y = clean_casual_cnt)) + ylab('Cleaned casual Count')

daily_data$clean_registered_cnt = tsclean(registered_ts)
ggplot() +
  geom_line(data = daily_data, aes(x = Date, y = clean_registered_cnt)) + ylab('Cleaned registered Count')

#weekly MA
daily_data$cnt_ma = ma(daily_data$clean_cnt, order=7) # using the clean count with no outliers
daily_data$cnt_casual_ma = ma(daily_data$clean_casual_cnt, order=7) # using the clean count with no outliers
daily_data$cnt_registered_ma = ma(daily_data$clean_registered_cnt, order=7) # using the clean count with no outliers


#monthly MA
daily_data$cnt_ma30 = ma(daily_data$clean_cnt, order=30)
#monthly MA for casual
daily_data$cnt_casual_ma30 = ma(daily_data$clean_casual_cnt, order=30)
#monthly MA for casual
daily_data$cnt_registered_ma30 = ma(daily_data$clean_registered_cnt, order=30)

#Overlay for all counts
ggplot() +
  geom_line(data = daily_data, aes(x = Date, y = clean_cnt, colour = "Counts")) +
  geom_line(data = daily_data, aes(x = Date, y = cnt_ma,   colour = "Weekly Moving Average"))  +
  geom_line(data = daily_data, aes(x = Date, y = cnt_ma30, colour = "Monthly Moving Average"))  +
  ylab('Bike Count')


ggplot() +
  geom_line(data = daily_data, aes(x = Date, y = clean_casual_cnt, colour = "Counts")) +
  geom_line(data = daily_data, aes(x = Date, y = cnt_casual_ma,   colour = "Weekly Moving Average"))  +
  geom_line(data = daily_data, aes(x = Date, y = cnt_casual_ma30, colour = "Monthly Moving Average"))  +
  ylab('Bike Count')


ggplot() +
  geom_line(data = daily_data, aes(x = Date, y = clean_registered_cnt, colour = "Counts")) +
  geom_line(data = daily_data, aes(x = Date, y = cnt_registered_ma,   colour = "Weekly Moving Average"))  +
  geom_line(data = daily_data, aes(x = Date, y = cnt_registered_ma30, colour = "Monthly Moving Average"))  +
  ylab('Bike Count')

daily_data$newcnt = daily_data$cnt_casual_ma + daily_data$cnt_registered_ma

#############Old_Code######################################
# count_ma = ts(na.omit(daily_data$cnt_ma), frequency=30)
# decomp = stl(count_ma, s.window="periodic")
# deseasonal_cnt <- seasadj(decomp)
# plot(decomp)
###########################################################

count_ma = ts(na.omit(daily_data$newcnt), frequency=30)
decomp = stl(count_ma, s.window="periodic")
deseasonal_cnt <- seasadj(decomp)
plot(decomp)


# Computes the Augmented Dickey-Fuller test for the null that x has a unit root.
# 
# Usage
# 
# adf.test(x, alternative = c("stationary", "explosive"),
#          k = trunc((length(x)-1)^(1/3)))

adf.test(count_ma, alternative = "stationary")
acf = Acf(count_ma, main='')
pacf = Pacf(count_ma, main='')
plot(acf)
plot(pacf)
count_d1 = diff(deseasonal_cnt, differences = 1)
plot(count_d1)
adf.test(count_d1, alternative = "stationary")
acf1 = Acf(count_d1, main='ACF for Differenced Series')

pacf1 = Pacf(count_d1, main='PACF for Differenced Series')
plot(acf1)
plot(pacf1)

auto.arima(deseasonal_cnt, seasonal=FALSE)
fit<-auto.arima(deseasonal_cnt, seasonal=FALSE)
tsdisplay(residuals(fit), lag.max=45, main='(1,1,1) Model Residuals')



seas_fcast <- forecast(fit, h=366)
forecast_df <- as.data.frame(seas_fcast)
point_estimate <- forecast_df['Point Forecast']
test_bike_data <- read.csv("Bikeshare_Test.csv")
summary(test_bike_data)
min(forecast_df$`Point Forecast`, test_bike_data$cnt)
length(test_bike_data$cnt)
length(forecast_df$`Point Forecast`)
head(forecast_df$`Point Forecast`, 50)
head(test_bike_data$cnt,50)
Total_revenue <- sum(pmin(forecast_df$`Point Forecast`, test_bike_data$cnt)*3)
Total_revenue
Total_cost<-sum(forecast_df$`Point Forecast`*2)
Total_cost
Profit=Total_revenue-Total_cost
Profit
str(forecast_df)
# head(as.data.frame(seas_fcast3),10)
# plot(as.data.frame(seas_fcast3))

#################################################

fit2 = arima(deseasonal_cnt, order=c(2,1,7))
tsdisplay(residuals(fit2), lag.max=45, main='(2,1,7) Model Residuals')

fit2
seas_fcast2 <- forecast(fit2, h=366)
plot(seas_fcast2)

forecast_df2 <- as.data.frame(seas_fcast2)
point_estimate2 <- forecast_df2['Point Forecast']
test_bike_data <- read.csv("Bikeshare_Test.csv")
summary(test_bike_data)
#min(forecast_df2$`Point Forecast`, test_bike_data$cnt)

Total_revenue <- sum(pmin(forecast_df2$`Point Forecast`, test_bike_data$cnt)*3)
Total_revenue
Total_cost<-sum(forecast_df2$`Point Forecast`*2)
Total_cost
Profit=Total_revenue-Total_cost
Profit


################
fcast <- forecast(fit2, h=30)
plot(fcast)
hold <- window(ts(deseasonal_cnt), start=700)
fit_no_holdout = arima(ts(deseasonal_cnt[-c(700:725)]), order=c(1,1,7))
fcast_no_holdout <- forecast(fit_no_holdout,h=25)
plot(fcast_no_holdout, main=" ")
lines(ts(deseasonal_cnt))
fit_w_seasonality = auto.arima(deseasonal_cnt, seasonal=TRUE)
fit_w_seasonality
seas_fcast <- forecast(fit_w_seasonality, h=366)
plot(seas_fcast)



write.csv(daily_data, "forothertool.csv", row.names = FALSE )
