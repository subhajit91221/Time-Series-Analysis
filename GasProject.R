library('ggplot2')
library('forecast')
library('tseries')
library("stats")
library("data.table")
library('fpp2')
library('xts')

## Read the Gas data set from Forecast project

data<- forecast::gas
plot(data)
str(data)
head(data)
tail(data)

gasdata <- ts(gas, start=c(1970,1), frequency=12)
plot(gasdata)
str(gasdata)
head(gasdata)
tail(gasdata)

monthplot(gasdata)

#Decompose the dataset
GasDec<-stl(gasdata, s.window='p') #constant seasonality
plot(GasDec)
GasDec

GasDec<-stl(gasdata, s.window=7) #changing seasonality
plot(GasDec)
GasDec
str(GasDec)

## Removing the seasonality component 
DeseasonGasDec <- (GasDec$time.series[,2]+GasDec$time.series[,3])

## Plotting deseasonal vs original dataset 
ts.plot(gasdata,DeseasonGasDec,
        gpars = list(col = c("black", "red")),
        main="Comparison of Original Production and Deseasonalized Production")

## Since the series is multiplicative , taking log of the series
logGas <- log(gasdata)
plot(logGas)
logGasDec <- stl(logGas, s.window=7)
logGasDec$time.series[1:12,1]
str(logGasDec)
## Plotting the seasonaily component 
GasSeason <- exp(logGasDec$time.series[1:12,1])
plot(GasSeason, type="l")

GasTrend<-exp(logGasDec$time.series[1:12,2])
plot(GasTrend, type="l")

## Check periodicy by using periodicity() under xts package 
periodicity(gasdata)
## result - Monthly periodicity from Jan 1970 to Aug 2009 

## Stationary Checking - ADF Test 

adf.test(gasdata)

## p value 0.27-> accept null hypothesis ( seris is not stationary )

adf.test(DeseasonGasDec)

## p value 0.45-> accept null hypothesis ( seris is not stationary )

##Deseasonalize the series
decomp = stl(gasdata, s.window = 7)
deseasonal_gasdata=seasadj(decomp)
plot(decomp)
plot(deseasonal_gasdata)

acf(gasdata)

acf(gasdata, lag.max = 120)
pacf(gasdata, lag.max = 120)

##Differencing the time series data

count_d1 = diff(deseasonal_gasdata, differences = 1)
plot(count_d1)
adf.test(count_d1)

## p value 0.01-> reject the null hypothesis (series is stationary)

Acf(count_d1, main='ACF for Differenced Series')
Pacf(count_d1, main='PACF for Differenced Series')

acf(count_d1, lag.max = 120)

##Splitting into training and test sets

GasTStrain = window(deseasonal_gasdata, start=c(1970,1), end=c(1993,12))
GasTStest= window(deseasonal_gasdata, start=1994)

GasARIMA = arima(GasTStrain, order=c(2,1,0))
summary(GasARIMA)
tsdisplay(residuals(GasARIMA),  main='Model Residuals')

##Fitting with Auto ARIMA

fit<-auto.arima(GasTStrain, seasonal=FALSE)
summary(fit)
tsdisplay(residuals(fit), lag.max=45, main='Auto ARIMA Model Residuals')


##Auto ARIMA  has a slightly lower AIC and MAPE

#Ljung box test
##H0: Residuals are independent
##Ha: Residuals are not independent


library(stats)

Box.test(GasARIMA$residuals)

#p value = 0.2, so Residuals are independent

#Forecasting with the ARIMA model

fcast <- forecast(GasARIMA, h=12)
plot(fcast)


#Accuracy of the forecast

f7=forecast(GasARIMA)
summary(f7)
plot(f7)
accuracy(f7, GasTStest)

#Now we will use the complete series to forecase 
ManualARIMA = arima(gasdata, order=c(2,1,0))
manualfcast <- forecast(ManualARIMA, h=12)
plot(manualfcast)
manualf7=forecast(ManualARIMA)
summary(manualf7)





fcast1 <- forecast(fit, h=12)
plot(fcast1)

fit1<-auto.arima(GasTStest, seasonal=FALSE)
summary(fit1)
#Accuracy of the forecast

f8=forecast(fit1)
plot(f8)
accuracy(f8, GasTStest)
autoarima<-auto.arima(gasdata, seasonal=FALSE)
AutoArimaForecast <- forecast(autoarima, h=12)
summary(AutoArimaForecast)










