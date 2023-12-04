library(TSA)
library(tseries)
library(forecast)
library(lmtest) #for model diagnostic

##dataset
library(readr) 
bbri <- read_csv("Downloads/BBRI.JK (1).csv") #https://drive.google.com/file/d/1LRdMMTAGbelGlf7INfyu9RJt7bWsUVxY/view?usp=drive_link

##time series variable
ts <- ts(bbri[,5],start = c(2003,12), frequency = 12)

##differencing
dts <- diff(ts,1)
tsdisplay(dts)
BoxCox.ar(ts)

##transforming
tts <- BoxCox(ts, BoxCox.lambda(ts))
dtts <- diff(tts,1)
tsdisplay(dtts)

adf.test(dtts)
eacf(dtts)
#suggested model: ARIMA(0,1,0); ARIMA(0,1,1); ARIMA(1,1,1)

model1 <- Arima(tts, order = c(0,1,0), include.constant = TRUE)
model2 <- Arima(tts, order = c(0,1,1), include.constant = TRUE)
model3 <- Arima(tts, order = c(1,1,1), include.constant = TRUE)
cbind(model1, model2, model3)
#dari nilai aicc, kita pilih model ARIMA(1,1,1)

##model diagnostic 
#model3
checkresiduals(model3)
adf.test(model3$residuals)
jarque.bera.test(model3$residuals)

#model1
checkresiduals(model1)
adf.test(model1$residuals)
jarque.bera.test(model1$residuals)

#model2
checkresiduals(model2)
adf.test(model2$residuals)
jarque.bera.test(model2$residuals)
#Model yang memenuhi asumsi adalah model ARIMA(0,1,0)

##overfitting
overfit_1 <- Arima(tts, order = c(1,1,0), include.constant = TRUE)
coeftest(model1)
coeftest(overfit_1)
#Estimasi parameter dari model original tidak berbeda jauh dari model overfit

##forecast
model1forecast <- Arima(ts, order = c(0,1,0), include.constant = TRUE, lambda = BoxCox.lambda(ts))
forecast <- forecast(model1forecast, h = 5)
autoplot(forecast)

##cross validation
test <- window(ts,start = c(2023,3))
train <- window(ts,end = c(2023,2))
trainmodel <- Arima(train,order = c(0,1,0))
crossvalidation <- forecast(trainmodel, h = 3)
plot(crossvalidation)
