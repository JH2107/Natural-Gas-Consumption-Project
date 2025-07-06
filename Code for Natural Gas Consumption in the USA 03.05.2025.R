#start
library(forecast)
library(tseries)
library(urca)
library(timetk)
library(TSstudio)

nat_gas<-read.csv("C:/Users/juliu/OneDrive/Desktop/PSA/NATURALGAS.csv")
nat_gas
cons<-nat_gas$NATURALGAS
date<-as.Date(nat_gas$observation_date)

########################General Plots#####################################
#General Plot of the data, there is a seasonal structure
par(mfrow=c(1,1))
plot(date,cons,type="l",xlab="Date",ylab="Billion Cubic Feet",main="Natural Gas consumption in the US")
ts_data <- ts(nat_gas$NATURALGAS,start = c(2000, 1), frequency = 12)
USgas_split <- ts_split(ts_data, sample.out = 12)
USgas_split
train <- USgas_split$train
test <- USgas_split$test


#test for differentiating
normal_diff<-ndiffs(train)
normal_diff
seas_diff<-nsdiffs(train)
seas_diff

par(mfrow=c(1,1))
plot(diff(train),main="Natural Gas Consumption in the US after the first differencing")
arima(train, order = c(1,0,0))
# Initial ACF and PACF Plot
par(mfrow=c(1,2))
acf(train, lwd=2, main="ACF train data",lag.max=60)
pacf(train, lwd=2, main="PACF train data",lag.max=60)

#checking for stationarity using the augmented dickey fuller test

adf.test(train)
ts_diff<-diff(train)
ts_diff
plot(ts_diff,ylab="value",main="Train data set difference once")

adf.test(ts_diff) 



######################Finding the right ARIMA model
gas_model<-auto.arima((train),seasonal=FALSE)
gas_model
checkresiduals(gas_model)
summary(gas_model)
accuracy(forecast(gas_model,h=12),test)
par(mfrow=c(1,1))
plot(forecast(gas_model,h=12))

###ARIMA Residuals ACF/PACF
par(mfrow=c(1,2))
acf(gas_model$residuals)
pacf(gas_model$residuals)

###ARIMA Ljung-Box Plots
Box.test(gas_model$residuals, type="Ljung-Box", lag=20)
###KPSS Unit Root Test
summary(ur.kpss(ts_data, type = "tau"))
###QQ-Plots
qqnorm(gas_model$residuals)
qqline(gas_model$residuals)
###Histogramm, Shapiro Wilk Test
hist(gas_model$residuals)
shapiro.test(gas_model$residuals)


ggseasonplot(train,xlab="Date",ylab="Billion Cubic Feet",main="")




####################################################




#looking for the right SARIMA model

sarima_model<-auto.arima(train,seasonal=TRUE)
sarima_model
print(sarima_model)
checkresiduals(sarima_model)
accuracy(forecast(sarima_model,h=12),test)

par(mfrow=c(1,1))
plot(forecast(sarima_model,h=12))
##############Model diagnostics

#ACF and PACF Plot
par(mfrow=c(1,2))
acf(sarima_model$residuals)
pacf(sarima_model$residuals)

#Ljung-Box
Box.test(sarima_model$residuals, type="Ljung-Box", lag=20)

kpss_test <- ur.kpss(ts_seasonal_diff)
summary(kpss_test)

#QQ-Plots
qqnorm(sarima_model$residuals)
qqline(sarima_model$residuals)

par(mfrow=c(1,1))
#Histogramm,Shapiro Wilk Test
hist(sarima_model$residuals)
shapiro.test(sarima_model$residuals)











