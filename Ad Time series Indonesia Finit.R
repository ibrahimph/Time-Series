library(forecast)
library(CADFtest)
library(fGarch)
library(vars)
library (urca)
library(xtable)


attach(Ina_Monthly_98)
Ina <- Ina_Monthly_98[22:237,]
detach(Ina_Monthly_98)
attach(Ina)
summary(Ina)

### 1. Univariate ANalysis
##########################

### 1.1.a Time Series Plot
Ex_ts <- ts(Exchange,frequency=12,start=c(1999,10)); #Exchange Rate Indonesian Rupiah per one US DOllar
Gold_ts <- ts((Gold/100),frequency=12,start=c(1999,10)); #Price of Gold per one centigram in IDR 
Res_ts <- ts(TotReserve,frequency=12,start=c(1999,10)); #in Trillion IDR Total reserves minus gold comprise special drawing rights, reserves of IMF members held by the IMF, and holdings of foreign exchange under the control of monetary authorities. Gold holdings are excluded. Data are in current U.S. dollars.

ts.plot(Ex_ts, Res_ts, Gold_ts, col=c("Violet","Green","gold"))

trend_Ex = ma(Ex_ts, order = 12, centre = T)
plot(as.ts(Ex_ts))
lines(trend_Ex)
plot(as.ts(trend_Ex))

trend_Res = ma(Res_ts, order = 12, centre = T)
plot(as.ts(Res_ts))
lines(trend_Res)
plot(as.ts(trend_Res))

trend_Gold = ma(Gold_ts, order = 12, centre = T)
plot(as.ts(Gold_ts))
lines(trend_Gold)
plot(as.ts(trend_Gold))

#Indeed that THere is trend and possibility of random walk with drift

### 1.1.b.1 Checking trend and season effect
TREND_Ex<- 1:216
M1<-rep(c(1,0,0,0,0,0,0,0,0,0,0,0),18)
M2<-rep(c(0,1,0,0,0,0,0,0,0,0,0,0),18)
M3<-rep(c(0,0,1,0,0,0,0,0,0,0,0,0),18)
M4<-rep(c(0,0,0,1,0,0,0,0,0,0,0,0),18)
M5<-rep(c(0,0,0,0,1,0,0,0,0,0,0,0),18)
M6<-rep(c(0,0,0,0,0,1,0,0,0,0,0,0),18)
M7<-rep(c(0,0,0,0,0,0,1,0,0,0,0,0),18)
M8<-rep(c(0,0,0,0,0,0,0,1,0,0,0,0),18)
M9<-rep(c(0,0,0,0,0,0,0,0,1,0,0,0),18)
M10<-rep(c(0,0,0,0,0,0,0,0,0,1,0,0),18)
M11<-rep(c(0,0,0,0,0,0,0,0,0,0,1,0),18)
M12<-rep(c(0,0,0,0,0,0,0,0,0,0,0,1),18)

fit_Ex<-lm(Ex_ts~TREND_Ex+M1+M2+M3+M4+M5+M6+M7+M8+M9+M10+M11+M12)
summary(fit_Ex)

TREND_Res<- 1:216
M1<-rep(c(1,0,0,0,0,0,0,0,0,0,0,0),18)
M2<-rep(c(0,1,0,0,0,0,0,0,0,0,0,0),18)
M3<-rep(c(0,0,1,0,0,0,0,0,0,0,0,0),18)
M4<-rep(c(0,0,0,1,0,0,0,0,0,0,0,0),18)
M5<-rep(c(0,0,0,0,1,0,0,0,0,0,0,0),18)
M6<-rep(c(0,0,0,0,0,1,0,0,0,0,0,0),18)
M7<-rep(c(0,0,0,0,0,0,1,0,0,0,0,0),18)
M8<-rep(c(0,0,0,0,0,0,0,1,0,0,0,0),18)
M9<-rep(c(0,0,0,0,0,0,0,0,1,0,0,0),18)
M10<-rep(c(0,0,0,0,0,0,0,0,0,1,0,0),18)
M11<-rep(c(0,0,0,0,0,0,0,0,0,0,1,0),18)
M12<-rep(c(0,0,0,0,0,0,0,0,0,0,0,1),18)

fit_Res<-lm(Res_ts~TREND_Res+M1+M2+M3+M4+M5+M6+M7+M8+M9+M10+M11+M12)
summary(fit_Res)

TREND_Gold<- 1:216
M1<-rep(c(1,0,0,0,0,0,0,0,0,0,0,0),18)
M2<-rep(c(0,1,0,0,0,0,0,0,0,0,0,0),18)
M3<-rep(c(0,0,1,0,0,0,0,0,0,0,0,0),18)
M4<-rep(c(0,0,0,1,0,0,0,0,0,0,0,0),18)
M5<-rep(c(0,0,0,0,1,0,0,0,0,0,0,0),18)
M6<-rep(c(0,0,0,0,0,1,0,0,0,0,0,0),18)
M7<-rep(c(0,0,0,0,0,0,1,0,0,0,0,0),18)
M8<-rep(c(0,0,0,0,0,0,0,1,0,0,0,0),18)
M9<-rep(c(0,0,0,0,0,0,0,0,1,0,0,0),18)
M10<-rep(c(0,0,0,0,0,0,0,0,0,1,0,0),18)
M11<-rep(c(0,0,0,0,0,0,0,0,0,0,1,0),18)
M12<-rep(c(0,0,0,0,0,0,0,0,0,0,0,1),18)

fit_Gold<-lm(Gold_ts~TREND_Gold+M1+M2+M3+M4+M5+M6+M7+M8+M9+M10+M11+M12)
summary(fit_Gold)

# Trend is alwas significant in all indicating the trend need to be removed, 
# However Monthly Effect deemed not significant


### 1.1.b.2 Unit Root Test to see Stocahstic Trend
dim(Ina)
max.lag<-round(sqrt(216))
CADFtest(Ex_ts,type="trend",criterion="BIC",max.lag.y = max.lag)
CADFtest(Res_ts,type="trend",criterion="BIC",max.lag.y = max.lag)
CADFtest(Gold_ts,type="trend",criterion="BIC",max.lag.y = max.lag)
#p-value Exchange 0.643, do not reject unit root ADF test for trend, thus trend is stochastic
#p-value Reserves 0.7273, do not reject unit root ADF test for trend, thus trend is stochastic
#p-value Gold 0.1861, do not reject unit root ADF test for trend, thus trend is stochastic
#non significant->stochastic
#significant ->deterministic


### 1.1.b.3 Transforming time series to log value
logEx_ts <- log(Ex_ts)
logRes_ts<-log(Res_ts)
logGold_ts<-log(Gold_ts)
ts.plot(logEx_ts, logRes_ts, logGold_ts, col=c("Violet","Green","gold"))

### 1.1.c.1 Time Series in Differences
dlogEx_ts<-diff(log(Ex_ts))
dlogRes_ts<-diff(log(Res_ts))
dlogGold_ts<-diff(log(Gold_ts))

ts.plot(dlogEx_ts, dlogRes_ts, dlogGold_ts, col=c("Violet","Green","gold"))

### 1.1.c.2 Unit Root Test (ADF) to see stationarity of drift
dim(Ina)
max.lag<-round(sqrt(216))
CADFtest(dlogEx_ts,type="drift",criterion="BIC",max.lag.y = max.lag)
CADFtest(dlogRes_ts,type="drift",criterion="BIC",max.lag.y = max.lag)
CADFtest(dlogGold_ts,type="drift",criterion="BIC",max.lag.y = max.lag)
#p-value Exchange p<0.0001, reject unit root ADF test for drift, thus indeed data is stationary
#p-value Reserves p<0.0001, reject unit root ADF test for drift, thus indeed data is stationary
#p-value Gold p<0.0001, reject unit root ADF test for drift, thus indeed data is stationary


### 1.1.d Checking Seasonal Differences
par(mfrow =c(1,3))
monthplot(dlogEx_ts, col="Violet")
monthplot(dlogRes_ts, col="green")
monthplot(dlogGold_ts, col="gold")
#even though the initial analysis says that monthly effects is not signiicant, 
#indded that the monthly plot shows there are slight differences, however ADFtest indicate that data is stationary, this may caused by the high variation of data monthly
#no need to apply correction for season
#at this stage, time series is concluded to be stationary

### 1.1.d.xxx Correcting Seasonal differences
dlogsEx_ts<-diff(diff(log(Ex_ts)),lag=12)
dlogsRes_ts<-diff(diff(log(Res_ts)),lag=12)
dlogsGold_ts<-diff(diff(log(Gold_ts)),lag=12)

par(mfrow =c(1,3))
ts.plot(Ex_ts, Res_ts, Gold_ts, col=c("Violet","Green","gold"),gpars=list(xlab="Time", ylab="Level Value"))
ts.plot(logEx_ts, logRes_ts, logGold_ts, col=c("Violet","Green","gold"),gpars=list(xlab="Time", ylab="Log Value"))
ts.plot(dlogEx_ts, dlogRes_ts, dlogGold_ts, col=c("Violet","Green","gold"),gpars=list(xlab="Time", ylab="DiffLog Value"))
#ts.plot(dlogsEx_ts, dlogsRes_ts, dlogsGold_ts, col=c("Violet","Green","gold"),gpars=list(xlab="Time", ylab="DiffLog Season adj Value"))

### 1.1.e Boxtest after making sure TS is stationary to see if residual is white noise
Box.test(dlogEx_ts,lag=max.lag,type="Ljung-Box")
Box.test(dlogRes_ts,lag=max.lag,type="Ljung-Box")
Box.test(dlogGold_ts,lag=max.lag,type="Ljung-Box")
#dlogEx_ts p-values 0.00938 
#dlogRes_ts p-values 0.8125
#dlogGold_ts p-values 0.0343

par(mfrow =c(2,3))
acf(dlogEx_ts, col="Violet",lag.max = 36)
acf(dlogRes_ts, col="Green",lag.max = 36)
acf(dlogGold_ts, col="gold",lag.max = 36)
pacf(dlogEx_ts, col="Violet",lag.max = 36)
pacf(dlogRes_ts, col="Green",lag.max = 36)
pacf(dlogGold_ts, col="gold",lag.max = 36)
#ACF and PACF for dlogEx shows auto correlations at lag 1
#meanwhile for dlogGold shows possible auto correlations at lag 2
#meanwhile for dlogRes indeed seems stationary
#the conclusions supporting Ljung-Box.Test 


### 1.2. Fitting Model
### 1.2.1 Exchange Rate : ARIMA model
### 1.2.1.1 Exchange Rate : MA (1)
fit_Ex_MA1<-arima(logEx_ts,order=c(0,1,1), seasonal=c(0,0,0))
summary(fit_Ex_MA1)
abs(fit_Ex_MA1$coef/sqrt(diag(fit_Ex_MA1$var.coef)))

par(mfrow =c(1,3))
plot.ts(fit_Ex_MA1$residuals, col="Violet")
acf(fit_Ex_MA1$residuals,col="Violet",lag.max = 36)
pacf(fit_Ex_MA1$residuals,col="Violet",lag.max = 36)
Box.test(fit_Ex_MA1$residuals, lag=max.lag, type="Ljung-Box")

### 1.2.1.2 Exchange Rate : AR (1)
fit_Ex_AR1<-arima(logEx_ts,order=c(1,1,0), seasonal=c(0,0,0))
summary(fit_Ex_AR1)
abs(fit_Ex_AR1$coef/sqrt(diag(fit_Ex_AR1$var.coef)))

plot.ts(fit_Ex_AR1$residuals, col="Violet")
par(mfrow =c(1,2))
acf(fit_Ex_AR1$residuals, col="Violet",lag.max = 36)
pacf(fit_Ex_AR1$residuals, col="Violet",lag.max = 36)
Box.test(fit_Ex_AR1$residuals, lag=max.lag, type="Ljung-Box")

### 1.2.1.3 Exchange Rate : AR (2)
fit_Ex_AR2<-arima(logEx_ts,order=c(2,1,0), seasonal=c(0,0,0))
summary(fit_Ex_AR2)
abs(fit_Ex_AR2$coef/sqrt(diag(fit_Ex_AR2$var.coef)))

plot.ts(fit_Ex_AR2$residuals, col="Violet")
acf(fit_Ex_AR2$residuals, col="Violet",lag.max = 36)
Box.test(fit_Ex_AR2$residuals, lag=max.lag, type="Ljung-Box")

### 1.2.1.4 Exchange Rate : ARIMA (1,1,1)
fit_Ex_ARIMA11<-arima(logEx_ts,order=c(1,1,1), seasonal=c(0,0,0))
summary(fit_Ex_ARIMA11)
abs(fit_Ex_ARIMA11$coef/sqrt(diag(fit_Ex_ARIMA11$var.coef)))

plot.ts(fit_Ex_ARIMA11$residuals, col="Violet")
acf(fit_Ex_ARIMA11$residuals, col="Violet",lag.max = 36)
pacf(fit_Ex_ARIMA11$residuals, col="Violet",lag.max = 36)
Box.test(fit_Ex_ARIMA11$residuals, lag=max.lag, type="Ljung-Box")


AIC(fit_Ex_MA1,fit_Ex_AR1,fit_Ex_AR2,fit_Ex_ARIMA11,k=log(216))
#MA1: p-value = 0.3975 
#AR1: p-value = 0.2611 
#AR2: p-value = 0.5264
#ARMA11: p-value = 0.6034
#               df       BIC
#fit_Ex_MA1      2 -889.8816
#fit_Ex_AR1      2 -887.0310
#fit_Ex_AR2      3 -885.7643
#fit_Ex_ARIMA11  3 -885.6691

#indeed that MA1 is the most parsimonious model that can be fitted, also has the best p-value, 


### 1.3 Forecasting


tahunawal<-2016
tahunakhir<-2019
jmlblnprediksi <-12

### 1.3.1 Exchange Rate : ARIMA model
### 1.3.1.1 Exchange Rate : MA (1)
fc_fit_Ex_MA1<-predict(fit_Ex_MA1,n.ahead = jmlblnprediksi)
expected_fc_fit_Ex_MA1<-fc_fit_Ex_MA1$pred
low_fc_fit_Ex_MA1<-fc_fit_Ex_MA1$pred-qnorm(0.975)*fc_fit_Ex_MA1$se
up_fc_fit_Ex_MA1<-fc_fit_Ex_MA1$pred+qnorm(0.975)*fc_fit_Ex_MA1$se
cbind(low_fc_fit_Ex_MA1, expected_fc_fit_Ex_MA1, up_fc_fit_Ex_MA1)

plot.ts(logEx_ts, xlim=c(tahunawal,tahunakhir), ylim=c(9,9.8))
lines(expected_fc_fit_Ex_MA1, col="red")
lines(low_fc_fit_Ex_MA1, col="blue")
lines(up_fc_fit_Ex_MA1, col="blue")

### 1.3.1.2 Exchange Rate : AR (1)
fc_fit_Ex_AR1<-predict(fit_Ex_AR1,n.ahead = jmlblnprediksi)
expected_fc_fit_Ex_AR1<-fc_fit_Ex_AR1$pred
low_fc_fit_Ex_AR1<-fc_fit_Ex_AR1$pred-qnorm(0.975)*fc_fit_Ex_AR1$se
up_fc_fit_Ex_AR1<-fc_fit_Ex_AR1$pred+qnorm(0.975)*fc_fit_Ex_AR1$se
cbind(low_fc_fit_Ex_AR1, expected_fc_fit_Ex_AR1, up_fc_fit_Ex_AR1)

plot.ts(logEx_ts, xlim=c(tahunawal,tahunakhir), ylim=c(9,10))
lines(expected_fc_fit_Ex_AR1, col="red")
lines(low_fc_fit_Ex_AR1, col="blue")
lines(up_fc_fit_Ex_AR1, col="blue")

### 1.3.1.3 Exchange Rate : AR (2)
fc_fit_Ex_AR2<-predict(fit_Ex_AR2,n.ahead = jmlblnprediksi)
expected_fc_fit_Ex_AR2<-fc_fit_Ex_AR2$pred
low_fc_fit_Ex_AR2<-fc_fit_Ex_AR2$pred-qnorm(0.975)*fc_fit_Ex_AR2$se
up_fc_fit_Ex_AR2<-fc_fit_Ex_AR2$pred+qnorm(0.975)*fc_fit_Ex_AR2$se
cbind(low_fc_fit_Ex_AR2, expected_fc_fit_Ex_AR2, up_fc_fit_Ex_AR2)

plot.ts(logEx_ts, xlim=c(tahunawal,tahunakhir), ylim=c(9,10))
lines(expected_fc_fit_Ex_AR2, col="red")
lines(low_fc_fit_Ex_AR2, col="blue")
lines(up_fc_fit_Ex_AR2, col="blue")


### 1.3.1.4 Exchange Rate : ARIMA (1,1,1)
fc_fit_Ex_ARIMA11<-predict(fit_Ex_ARIMA11,n.ahead = jmlblnprediksi)
expected_fc_fit_Ex_ARIMA11<-fc_fit_Ex_ARIMA11$pred
low_fc_fit_Ex_ARIMA11<-fc_fit_Ex_ARIMA11$pred-qnorm(0.975)*fc_fit_Ex_ARIMA11$se
up_fc_fit_Ex_ARIMA11<-fc_fit_Ex_ARIMA11$pred+qnorm(0.975)*fc_fit_Ex_ARIMA11$se
cbind(low_fc_fit_Ex_ARIMA11, expected_fc_fit_Ex_ARIMA11, up_fc_fit_Ex_ARIMA11)

plot.ts(logEx_ts, xlim=c(tahunawal,tahunakhir), ylim=c(9,10))
lines(expected_fc_fit_Ex_ARIMA11, col="red")
lines(low_fc_fit_Ex_ARIMA11, col="blue")
lines(up_fc_fit_Ex_ARIMA11, col="blue")


y<-logEx_ts
S=162
h=1
error1.1.h<-c()
for (i in S:(length(y)-h))
{
  mymodel.sub<-arima(y[1:i],order=c(0,1,1),seasonal=c(0,0,0))
  predict.1.h<-predict(mymodel.sub,n.ahead=h)$pred[h]
  error1.1.h<-c(error1.1.h, y[i+h]-predict.1.h)
}

error1.2.h<-c()
for (i in S:(length(y)-h))
{
  mymodel.sub<-arima(y[1:i],order=c(1,1,0),seasonal=c(0,0,0))
  predict.2.h<-predict(mymodel.sub,n.ahead=h)$pred[h]
  error1.2.h<-c(error1.2.h,y[i+h]-predict.2.h)
}

error1.3.h<-c()
for (i in S:(length(y)-h))
{
  mymodel.sub<-arima(y[1:i],order=c(2,1,0),seasonal=c(0,0,0))
  predict.3.h<-predict(mymodel.sub,n.ahead=h)$pred[h]
  error1.3.h<-c(error1.3.h,y[i+h]-predict.3.h)
}

error1.4.h<-c()
for (i in S:(length(y)-h))
{
  mymodel.sub<-arima(y[1:i],order=c(1,1,1),seasonal=c(0,0,0))
  predict.4.h<-predict(mymodel.sub,n.ahead=h)$pred[h]
  error1.4.h<-c(error1.4.h,y[i+h]-predict.4.h)
}
summary(abs(error1.1.h))
summary(abs(error1.2.h))
summary(abs(error1.3.h))
summary(abs(error1.4.h))
cbind(error1.1.h,error1.2.h,error1.3.h,error1.4.h)

MAE_Ex_MA1<-mean(abs(error1.1.h))
MAE_Ex_AR1<-mean(abs(error1.2.h))
MAE_Ex_AR2<-mean(abs(error1.3.h))
MAE_Ex_ARIMA11<-mean(abs(error1.4.h))
cbind(MAE_Ex_MA1,MAE_Ex_AR1,MAE_Ex_AR2,MAE_Ex_ARIMA11)

dm.test(error1.1.h,error1.2.h,h=h, power=1)
dm.test(error1.1.h,error1.3.h,h=h, power=1)
dm.test(error1.1.h,error1.4.h,h=h, power=1)
dm.test(error1.2.h,error1.3.h,h=h, power=1)
dm.test(error1.2.h,error1.4.h,h=h, power=1)
dm.test(error1.3.h,error1.4.h,h=h, power=1)



#with forecast horizon 1
#using Mean Absolute Error Criteria
#      MAE_Ex_MA1 MAE_Ex_AR1 MAE_Ex_AR2 MAE_Ex_ARIMA11
#      0.01446202 0.01456047   0.014409     0.01435619

# MA1 vs AR1 p-value = 0.6369
# MA1 vs AR2 p-value = 0.8166
# MA1 vs ARIMA11 p-value = 0.5585
# AR1 vs AR2 p-value = 0.6839
# AR1 vs ARIMA11 p-value = 0.5401
# AR2 vs ARIMA11 p-value = 0.7615

# Based on MAE criteria ARIMA11 is preferable because it has the least MAE value


MSE_Ex_MA1<-mean(error1.1.h^2)
MSE_Ex_AR1<-mean(error1.2.h^2)
MSE_Ex_AR2<-mean(error1.3.h^2)
MSE_Ex_ARIMA11<-mean(error1.4.h^2)
cbind(MSE_Ex_MA1,MSE_Ex_AR1,MSE_Ex_AR2,MSE_Ex_ARIMA11)

dm.test(error1.1.h,error1.2.h,h=h, power=2)
dm.test(error1.1.h,error1.3.h,h=h, power=2)
dm.test(error1.1.h,error1.4.h,h=h, power=2)
dm.test(error1.2.h,error1.3.h,h=h, power=2)
dm.test(error1.2.h,error1.4.h,h=h, power=2)
dm.test(error1.3.h,error1.4.h,h=h, power=2)


#with forecast horizon 1
#using Mean Square Error Criteria
#         MSE_Ex_MA1   MSE_Ex_AR1   MSE_Ex_AR2 MSE_Ex_ARIMA11
#       0.0003950568 0.0004035242 0.0004032018   0.0003983655

# MA1 vs AR1 p-value = 0.2579
# MA1 vs AR2 p-value = 0.4848
# MA1 vs ARIMA11 p-value = 0.6777
# AR1 vs AR2 p-value = 0.9816
# AR1 vs ARIMA11 p-value = 0.6506
# AR2 vs ARIMA11 p-value = 0.5694

#MA1 is preferable because the model has the least Mean Squared Error


#MAPE
error_m1_ts <- ts(error1.1.h, frequency = 12, start = c(2013,4))
error_m2_ts <- ts(error1.2.h, frequency = 12, start = c(2013,4))
error_m3_ts <- ts(error1.3.h, frequency = 12, start = c(2013,4))
error_m4_ts <- ts(error1.4.h, frequency = 12, start = c(2013,4))
MAPE_Ex_MA1 <- mean(abs(error_m1_ts/logEx_ts))*100
MAPE_Ex_AR1 <- mean(abs(error_m2_ts/logEx_ts))*100
MAPE_Ex_AR2 <- mean(abs(error_m3_ts/logEx_ts))*100
MAPE_Ex_ARIMA11 <- mean(abs(error_m4_ts/logEx_ts))*100
cbind(MAPE_Ex_MA1,MAPE_Ex_AR1,MAPE_Ex_AR2,MAPE_Ex_ARIMA11)

# MAPE_Ex_MA1 MAPE_Ex_AR1 MAPE_Ex_AR2 MAPE_Ex_ARIMA11
#   0.1534857   0.1545251   0.1529258       0.1523776



#1.3.2 Plot Pseudo Out of Sample Rolling Window Forecast
y<-logEx_ts
S=162
h=1
forc.1.h<-c()
error1.1.h<-c()
for (i in S:(length(y)-h))
{
  mymodel.sub<-arima(y[1:i],order=c(0,1,1),seasonal=c(0,0,0))
  predict.1.h<-predict(mymodel.sub,n.ahead=h)$pred[h]
  forc.1.h<-c(forc.1.h, predict.1.h)
  error1.1.h<-c(error1.1.h, y[i+h]-predict.1.h)
}

#making it a time series:
myforecast_rolling_pseudo_ts <- ts(forc.1.h,frequency=12,start=c(2013,4))
#Graphical comparison of the forecasted values with the observed ones
ts.plot(logEx_ts,myforecast_rolling_pseudo_ts,ylab="LogEx", col=c("black","red"),xlim=c(2010,2018), ylim=c(9,9.6))

par(mfrow =c(1,2))
plot.ts(logEx_ts, xlim=c(2010,tahunakhir), ylim=c(9,9.8),xlab="Time  (a)")
lines(expected_fc_fit_Ex_MA1, col="red")
lines(low_fc_fit_Ex_MA1, col="blue")
lines(up_fc_fit_Ex_MA1, col="blue")
ts.plot(logEx_ts,myforecast_rolling_pseudo_ts,xlab="Time  (b)", ylab="LogEx", col=c("black","red"),xlim=c(2010,2018), ylim=c(9,9.8))


### 2 ARCH GARCH Model
fit_Ex_MA1
plot(fit_Ex_MA1$residuals, col="violet")
acf(fit_Ex_MA1$residuals, col="violet")
Box.test(fit_Ex_MA1$residuals, lag=max.lag, type="Ljung-Box") #indeed white noise
acf(fit_Ex_MA1$residuals^2)
# MA1 is concluded as valid and the most parsimonious model to explain Exchange
# the residual of MA1 is white noise
# However there are significant auro correlations in the squared residuals
# which indicates the presence of heteroskedasticity

fit_garch_Ex_1<-garchFit(~arma(0,1)+garch(1,1),data=dlogEx_ts)
plot(fit_garch_Ex_1)
summary(fit_garch_Ex_1)
# Model Validated but data is not normal as normality assumption of normality is rejected, use QMLE

fit_garch_Ex_2<-garchFit(~arma(0,1)+garch(1,1),cond.dist = "QMLE",data=dlogEx_ts)
plot(fit_garch_Ex_2)
summary(fit_garch_Ex_2)


### 3 Bivariate of time series
# All time series are stationary in differences   
# Hence, all time series are integrated of order one I(1).
# Assuming The Multivariate White noise (correlated to each other at same lag, but not between lag)


# 3.1 Predictive Model for Exchange rate by Reserves
fit_Ex_Res <- lm(dlogEx_ts ~ dlogRes_ts)
summary(fit_Ex_Res)
# dlogRes_ts  significant as covariates for dlogEx_ts
# R2= 30.71% of delta log Exchange are explained by deltalog Reserves
# p-val < 0.00 significant thus all covariates are jointly significant to predict delta log Exchange
par(mfrow =c(1,3))
plot.ts(fit_Ex_Res$residuals)
acf(fit_Ex_Res$residuals)
pacf(fit_Ex_Res$residuals)
Box.test(fit_Ex_Res$residuals, lag = max.lag, type = "Ljung-Box")
# auto correlations at lag one indicated from acf and pacf graph  
# However  Q-test on the residuals 0.251>0.05 thus do not reject Ho White noise and conclude that model is valid

fit_dlm0 <- lm(dlogEx_ts ~ 1)
anova(fit_Ex_Res,fit_dlm0)


# 3.2 DLM model
lag <- 1
n <- length(dlogEx_ts)
dlogEx.0 <- dlogEx_ts[(lag+1):n]
dlogRes.0 <- dlogRes_ts[(lag+1):n]
dlogRes.1 <- dlogRes_ts[lag:(n-1)]
fit_dlm1 <- lm(dlogEx.0 ~ dlogRes.0+dlogRes.1)
par(mfrow =c(1,3))
plot.ts(fit_dlm1$residuals)
acf(fit_dlm1$residuals)
pacf(fit_dlm1$residuals)
Box.test(fit_dlm1$residuals, lag = max.lag, type = "Ljung-Box")


# 3.2 ADLM model
lag <- 1
dlogEx.0 <- dlogEx_ts[(lag+1):n]
dlogRes.0 <- dlogRes_ts[(lag+1):n]
dlogRes.1 <- dlogRes_ts[lag:(n-1)]
dlogEx.1 <- dlogEx_ts[lag:(n-1)]
fit_adlm <- lm(dlogEx.0 ~ dlogEx.1+dlogRes.1)

par(mfrow =c(1,3))
plot.ts(fit_adlm$residuals)
acf(fit_adlm$residuals)
pacf(fit_adlm$residuals)
Box.test(fit_adlm$residuals, lag = max.lag, type = "Ljung-Box")
summary(fit_adlm)

fit_adlm_nox <- lm(dlogEx.0 ~ dlogEx.1)
anova(fit_adlm,fit_adlm_nox)
# the dlog res is significant because the anova model show it is significant

# Engel -Granger test
fit_ci2<-lm(logEx_ts~logRes_ts)
res_fit_ci2<-fit_ci2$residuals
CADFtest(res_fit_ci2,type="drift", criterion="BIC", max.lag.y=max.lag)
#the test statistics ADF(2) = -2.2493 is larger than the Engle-Granger ADF statistics for one explanatory variable -3.41
#thus accept the H0 of no cointegration

fit_ci3<-lm(logRes_ts~logEx_ts)
res_fit_ci3<-fit_ci3$residuals
CADFtest(res_fit_ci3,type="drift", criterion="BIC", max.lag.y=max.lag)
#the test statistics ADF(2) = -1.8259 is larger than the Engle-Granger ADF statistics for one explanatory variable -3.41
#thus accept the H0 of no cointegration
#logEx_ts~logres_ts is not co-integrated thus regression between them in level is not possible
#however it is possible to regress delta logEx_ts~ delta logres_ts


### 4 Multivariate of time series

# All time series are in differences only not in seasonal differences  
# Hence, all time series are integrated of order one.
# The Multivariate White noise (correlated to each other at same lag, but not between lag)

### 4.1 Fitting VAR Model
detach(dlogdata)
attach(Ina)
dlogdata<-data.frame(diff(log(Exchange)),diff(log(TotReserve)),diff(log(Gold)))
names(dlogdata)<-c("dlogEx","dlogRes","dlogGold")
detach(Ina)
attach(dlogdata)

### 4.1.1 VAR(1) Model
fit_var_1<-VAR(dlogdata,type="const",p=1)
summary(fit_var_1)
var1_residuals<-resid(fit_var_1)

# if Response dlogEx -> R-Square: 0.07572 -- p: 0.000 dlogGold deemed not significant as covariates
# if Response dlogRes -> R-Square: 0.02509 -- p: 0.1479 none of covariates are significant
# if Response dlogGold -> R-Square: 0.01205 -- p: 0.466 none of covariates are significant

# only 7,572% of variance dlogEx is explained by the lagged observations of 
#      dlogEx, dlogRes and dlogGold at lag 1
#      the F-sattistics for dlogEx,regression are jointly significant but not for dlogRes and dlogGold

par(mfrow=c(2,3))
acf(var1_residuals[,1])
acf(var1_residuals[,2])
acf(var1_residuals[,3])
ccf(var1_residuals[,1],var1_residuals[,2])
ccf(var1_residuals[,1],var1_residuals[,3])
ccf(var1_residuals[,2],var1_residuals[,3])
par(mfrow=c(1,1))

# residuals collerogram VAR(1) for dlogEx, dlogRes and dlogGold and cross collerogram between the three var
#     shows there none significant correlations which indicating multivariate whitenoise

# 4.1.2 Auto Var lag selection
VARselect(dlogdata,lag.max=10,type="const")

fit_varautom<-VAR(dlogdata,type="const",p=1)
summary(fit_varautom)
varautom_residuals<-resid(fit_varautom)
#p selected = 1 same with above

# 4.1.3 Breusch-Godfrey tests
var_test <- serial.test(fit_varautom, lags.pt=14, type = "PT.adjusted")
var_test2 <- serial.test(fit_varautom, lags.pt=14, type = "BG")

# 4.2 Impulse response Function
irf_var_auto<-irf(fit_varautom, ortho="F", boot="T")
plot(irf_var_auto)

# 4.3 Johanssen test
detach(dlogdata)
attach(Ina)
logdata<-data.frame((log(Exchange)),(log(TotReserve)),(log(Gold)))
names(logdata)<-c("logEx","logRes","logGold")
detach(Ina)
attach(logdata)

VARselect(logdata,lag.max=10,type="const")

jotest=ca.jo(data.frame(logdata), type="trace", K=2, ecdet="none", spec="longrun")
summary(jotest)

trace_test=ca.jo(data.frame(logdata), type="trace", K=2, ecdet="const", spec="transitory")
summary(trace_test)

maxeigen_test=ca.jo(data.frame(logdata), type="eigen", K=2, ecdet="const", spec="transitory")
summary(maxeigen_test)

# 4.4 VECM
fit_vecm1<-cajorls(trace_test,r=1)
fit_vecm1
fit_vecm2<-cajorls(maxeigen_test,r=1)
fit_vecm2

fit_var<-vec2var(maxeigen_test,r=1)
myforecast<-predict(fit_var,n.ahead=12)
par(mfrow=c(3,1))

logExVECM_forecast<-ts(myforecast$fcst$logEx[,1],frequency=12,start=c(2017,10))
logExVECM_lower<-ts(myforecast$fcst$logEx[,2],frequency=12,start=c(2017,10))
logExVECM_upper<-ts(myforecast$fcst$logEx[,3],frequency=12,start=c(2017,10))
ts.plot(logEx_ts, logExVECM_forecast,logExVECM_lower,logExVECM_upper,col=c("black","blue","red","red"),xlim=c(2000,2018.2))
title(main = "12-step-ahead forecast of log(Exchange Rate)")

logResVECM_forecast<-ts(myforecast$fcst$logRes[,1],frequency=12,start=c(2017,10))
logResVECM_lower<-ts(myforecast$fcst$logRes[,2],frequency=12,start=c(2017,10))
logResVECM_upper<-ts(myforecast$fcst$logRes[,3],frequency=12,start=c(2017,10))
ts.plot(logRes_ts, logResVECM_forecast,logResVECM_lower,logResVECM_upper,col=c("black","blue","red","red"),xlim=c(2000,2018.2))
title(main = "12-step-ahead forecast of log(Total Reserves)")

logGoldVECM_forecast<-ts(myforecast$fcst$logGold[,1],frequency=12,start=c(2017,10))
logGoldVECM_lower<-ts(myforecast$fcst$logGold[,2],frequency=12,start=c(2017,10))
logGoldVECM_upper<-ts(myforecast$fcst$logGold[,3],frequency=12,start=c(2017,10))
ts.plot(logGold_ts, logGoldVECM_forecast,logGoldVECM_lower,logGoldVECM_upper,col=c("black","blue","red","red"),xlim=c(2000,2018.2))
title(main = "12-step-ahead forecast of log(Gold Price)")

par(mfrow=c(3,1))

ts.plot(logEx_ts, logExVECM_forecast,logExVECM_lower,logExVECM_upper,col=c("black","blue","red","red"),xlim=c(2000,2018.2))
title(main = "12-step-ahead forecast of log(Exchange Rate)")

ts.plot(logRes_ts, logResVECM_forecast,logResVECM_lower,logResVECM_upper,col=c("black","blue","red","red"),xlim=c(2000,2018.2))
title(main = "12-step-ahead forecast of log(Total Reserves)")

ts.plot(logGold_ts, logGoldVECM_forecast,logGoldVECM_lower,logGoldVECM_upper,col=c("black","blue","red","red"),xlim=c(2000,2018.2))
title(main = "12-step-ahead forecast of log(Gold Price)")


## Obsolete Univariate


### 1.2.2 Total Reserves : ARIMA model
### 1.2.2.1 Total Reserves : MA (5)
fit_Res_MA5<-arima(logRes_ts,order=c(0,1,5), seasonal=c(0,0,0))
fit_Res_MA5
abs(fit_Res_MA5$coef/sqrt(diag(fit_Res_MA5$var.coef)))

plot.ts(fit_Res_MA5$residuals, col="Green")
acf(fit_Res_MA5$residuals,col="Green",lag.max = 36)
Box.test(fit_Res_MA5$residuals, lag=max.lag, type="Ljung-Box")


### 1.2.2.2 Total Reserves : AR (4)
fit_Res_AR4<-arima(logRes_ts,order=c(4,1,0), seasonal=c(0,0,0))
fit_Res_AR4
abs(fit_Res_AR4$coef/sqrt(diag(fit_Res_AR5$var.coef)))

plot.ts(fit_Res_AR4$residuals, col="Green")
acf(fit_Res_AR4$residuals, col="Green",lag.max = 36)
Box.test(fit_Res_AR4$residuals, lag=max.lag, type="Ljung-Box")


### 1.2.2.3 Total Reserves : ARIMA (3,1,2)
fit_Res_ARIMA32<-arima(logRes_ts,order=c(3,1,2), seasonal=c(0,0,0))
fit_Res_ARIMA32
abs(fit_Res_ARIMA32$coef/sqrt(diag(fit_Res_ARIMA32$var.coef)))

plot.ts(fit_Res_ARIMA32$residuals, col="Green")
acf(fit_Res_ARIMA32$residuals, col="Green",lag.max = 36)
pacf(fit_Res_ARIMA32$residuals, col="Green",lag.max = 36)
Box.test(fit_Res_ARIMA32$residuals, lag=max.lag, type="Ljung-Box")


AIC(fit_Res_MA5,fit_Res_AR4,fit_Res_ARIMA32,k=log(216))
#MA5: p-value = 0.08003
#AR4: p-value = 0.1111
#ARMA32: p-value = 0.0732
#                df       BIC
#fit_Res_MA5      6 -652.0971
#fit_Res_AR4      5 -660.7411
#fit_Res_ARIMA32  6 -658.7997

#that AR 4 is the most parsimonious model that can be fitted, 
#The use of ARIMA model no less than 3,1,2 will not produce white noise thus AR 4 is preffered 
#AIC AR4 also the lowest


### 1.2.3 Gold Price : ARIMA model
### 1.2.3.1 Gold Price : MA (3)
fit_Gold_MA3<-arima(logGold_ts,order=c(0,1,3), seasonal=c(0,0,0))
fit_Gold_MA3
abs(fit_Gold_MA3$coef/sqrt(diag(fit_Gold_MA3$var.coef)))

plot.ts(fit_Gold_MA3$residuals, col="Gold")
acf(fit_Gold_MA3$residuals,col="Gold",lag.max = 36)
Box.test(fit_Gold_MA3$residuals, lag=max.lag, type="Ljung-Box")


### 1.2.3.2 Gold Price : AR (3)
fit_Gold_AR3<-arima(logGold_ts,order=c(3,1,0), seasonal=c(0,0,0))
fit_Gold_AR3
abs(fit_Gold_AR3$coef/sqrt(diag(fit_Gold_AR3$var.coef)))

plot.ts(fit_Gold_AR3$residuals, col="Gold")
acf(fit_Gold_AR3$residuals, col="Gold",lag.max = 36)
Box.test(fit_Gold_AR3$residuals, lag=max.lag, type="Ljung-Box")


### 1.2.3.3 Gold Price : ARIMA (2,1,1)
fit_Gold_ARIMA21<-arima(logGold_ts,order=c(2,1,1), seasonal=c(0,0,0))
fit_Gold_ARIMA21
abs(fit_Gold_ARIMA21$coef/sqrt(diag(fit_Gold_ARIMA21$var.coef)))

plot.ts(fit_Gold_ARIMA21$residuals, col="Gold")
acf(fit_Gold_ARIMA21$residuals, col="Gold",lag.max = 36)
pacf(fit_Gold_ARIMA21$residuals, col="Gold",lag.max = 36)
Box.test(fit_Gold_ARIMA21$residuals, lag=max.lag, type="Ljung-Box")


AIC(fit_Gold_MA3,fit_Gold_AR3,fit_Gold_ARIMA21,k=log(216))
#MA3: p-value = 0.4895
#AR3: p-value = 0.3422
#ARMA21: p-value = 0.3888
#                 df       BIC
#fit_Gold_MA3      4 -666.7195
#fit_Gold_AR3      4 -664.3841
#fit_Gold_ARIMA21  4 -665.0723

#MA3 is the most parsimonious model that can be fitted, 
#Supported by the lowwest value of BIC



### 1.3.2 Total Reserves : ARIMA model
### 1.3.2.1 Total Reserves : MA (5)
fc_fit_Res_MA5<-predict(fit_Res_MA5,n.ahead = jmlblnprediksi)
Expected_fc_fit_Res_MA5<-fc_fit_Res_MA5$pred
low_fc_fit_Res_MA5<-fc_fit_Res_MA5$pred-qnorm(0.975)*fc_fit_Res_MA5$se
up_fc_fit_Res_MA5<-fc_fit_Res_MA5$pred+qnorm(0.975)*fc_fit_Res_MA5$se
cbind(low_fc_fit_Res_MA5, Expected_fc_fit_Res_MA5, up_fc_fit_Res_MA5)

plot.ts(logRes_ts, xlim=c(tahunawal,tahunakhir), ylim=c(6.5,8))
lines(Expected_fc_fit_Res_MA5, col="red")
lines(low_fc_fit_Res_MA5, col="blue")
lines(up_fc_fit_Res_MA5, col="blue")

### 1.3.2.2 Total Reserves : AR (4)
fc_fit_Res_AR4<-predict(fit_Res_AR4,n.ahead = jmlblnprediksi)
Expected_fc_fit_Res_AR4<-fc_fit_Res_AR4$pred
low_fc_fit_Res_AR4<-fc_fit_Res_AR4$pred-qnorm(0.975)*fc_fit_Res_AR4$se
up_fc_fit_Res_AR4<-fc_fit_Res_AR4$pred+qnorm(0.975)*fc_fit_Res_AR4$se
cbind(low_fc_fit_Res_AR4, Expected_fc_fit_Res_AR4, up_fc_fit_Res_AR4)

plot.ts(logRes_ts, xlim=c(tahunawal,tahunakhir), ylim=c(6.5,8))
lines(Expected_fc_fit_Res_AR4, col="red")
lines(low_fc_fit_Res_AR4, col="blue")
lines(up_fc_fit_Res_AR4, col="blue")

### 1.3.2.3 Total Reserves : ARIMA (3,1,2)
fc_fit_Res_ARIMA32<-predict(fit_Res_ARIMA32,n.ahead = jmlblnprediksi)
Expected_fc_fit_Res_ARIMA32<-fc_fit_Res_ARIMA32$pred
low_fc_fit_Res_ARIMA32<-fc_fit_Res_ARIMA32$pred-qnorm(0.975)*fc_fit_Res_ARIMA32$se
up_fc_fit_Res_ARIMA32<-fc_fit_Res_ARIMA32$pred+qnorm(0.975)*fc_fit_Res_ARIMA32$se
cbind(low_fc_fit_Res_ARIMA32, Expected_fc_fit_Res_ARIMA32, up_fc_fit_Res_ARIMA32)

plot.ts(logRes_ts, xlim=c(tahunawal,tahunakhir), ylim=c(6.5,8))
lines(Expected_fc_fit_Res_ARIMA32, col="red")
lines(low_fc_fit_Res_ARIMA32, col="blue")
lines(up_fc_fit_Res_ARIMA32, col="blue")



y<-logRes_ts
S=150
h=1
error2.1.h<-c()
for (i in S:(length(y)-h))
{
  mymodel.sub<-arima(y[1:i],order=c(0,1,5),seasonal=c(0,0,0))
  predict.h<-predict(mymodel.sub,n.ahead=h)$pred[h]
  error2.1.h<-c(error2.1.h, y[i+h]-predict.h)
}

error2.2.h<-c()
for (i in S:(length(y)-h))
{
  mymodel.sub<-arima(y[1:i],order=c(4,1,0),seasonal=c(0,0,0))
  predict.h<-predict(mymodel.sub,n.ahead=h)$pred[h]
  error2.2.h<-c(error2.2.h,y[i+h]-predict.h)
}

error2.3.h<-c()
for (i in S:(length(y)-h))
{
  mymodel.sub<-arima(y[1:i],order=c(3,1,2),seasonal=c(0,0,0))
  predict.h<-predict(mymodel.sub,n.ahead=h)$pred[h]
  error2.3.h<-c(error2.3.h,y[i+h]-predict.h)
}
summary(abs(error2.1.h))
summary(abs(error2.2.h))
summary(abs(error2.3.h))
cbind(error2.1.h,error2.2.h,error2.3.h)

MAE_Res_MA5<-mean(abs(error2.1.h))
MAE_Res_AR4<-mean(abs(error2.2.h))
MAE_Res_ARIMA32<-mean(abs(error2.3.h))
cbind(MAE_Res_MA5,MAE_Res_AR4,MAE_Res_ARIMA32)

dm.test(error2.1.h,error2.2.h,h=h, power=1)
dm.test(error2.1.h,error2.3.h,h=h, power=1)
dm.test(error2.2.h,error2.3.h,h=h, power=1)

#with forecast horizon 1
#using Mean Absolute Error Criteria
#    MAE_Res_MA5    MAE_Res_AR4 MAE_Res_ARIMA32
#     0.02832164     0.02876839      0.02842192
# MA5 vs AR4 p-value = 0.5084
# MA5 vs ARIMA32 p-value = 0.9135
# AR4 vs ARIMA32 p-value = 0.7387
#MA5 is preferable due to lowest least Mean ABsolute Error
#DM Test  Shows that all models is not significantly different than the other

MSE_Res_MA5<-mean(error2.1.h^2)
MSE_Res_AR4<-mean(error2.2.h^2)
MSE_Res_ARIMA32<-mean(error2.3.h^2)
cbind(MSE_Res_MA5,MSE_Res_AR4,MSE_Res_ARIMA32)

dm.test(error2.1.h,error2.2.h,h=h, power=2)
dm.test(error2.1.h,error2.3.h,h=h, power=2)
dm.test(error2.2.h,error2.3.h,h=h, power=2)

#with forecast horizon 1
#using Mean Square Error Criteria
#       MSE_Res_MA5      MSE_Res_AR4  MSE_Res_ARIMA32
#       0.001232895      0.001302581      0.001186847
# MA5 vs AR4 p-value = 0.2422
# MA5 vs ARIMA32 p-value = 0.419
# AR4 vs ARIMA32 p-value = 0.1117
#ARIMA 32 is preferable because the model has the least Mean Squared Error
#However DM Test Shows all models are not significantly different than the other



### 1.3.3 Gold Price : ARIMA model
### 1.3.3.1 Gold Price : MA (3)
fc_fit_Gold_MA3<-predict(fit_Gold_MA3,n.ahead = jmlblnprediksi)
Expected_fc_fit_Gold_MA3<-fc_fit_Gold_MA3$pred
low_fc_fit_Gold_MA3<-fc_fit_Gold_MA3$pred-qnorm(0.975)*fc_fit_Gold_MA3$se
up_fc_fit_Gold_MA3<-fc_fit_Gold_MA3$pred+qnorm(0.975)*fc_fit_Gold_MA3$se
cbind(low_fc_fit_Gold_MA3, Expected_fc_fit_Gold_MA3, up_fc_fit_Gold_MA3)

plot.ts(logGold_ts, xlim=c(tahunawal,tahunakhir), ylim=c(8,9))
lines(Expected_fc_fit_Gold_MA3, col="red")
lines(low_fc_fit_Gold_MA3, col="blue")
lines(up_fc_fit_Gold_MA3, col="blue")

### 1.3.3.2 Gold Price : AR (3)
fc_fit_Gold_AR3<-predict(fit_Gold_AR3,n.ahead = jmlblnprediksi)
Expected_fc_fit_Gold_AR3<-fc_fit_Gold_AR3$pred
low_fc_fit_Gold_AR3<-fc_fit_Gold_AR3$pred-qnorm(0.975)*fc_fit_Gold_AR3$se
up_fc_fit_Gold_AR3<-fc_fit_Gold_AR3$pred+qnorm(0.975)*fc_fit_Gold_AR3$se
cbind(low_fc_fit_Gold_AR3, Expected_fc_fit_Gold_AR3, up_fc_fit_Gold_AR3)

plot.ts(logGold_ts, xlim=c(tahunawal,tahunakhir), ylim=c(8,9))
lines(Expected_fc_fit_Gold_AR3, col="red")
lines(low_fc_fit_Gold_AR3, col="blue")
lines(up_fc_fit_Gold_AR3, col="blue")

### 1.2.3.3 Gold Price : ARIMA (2,1,1)
fc_fit_Gold_ARIMA21<-predict(fit_Gold_ARIMA21,n.ahead = jmlblnprediksi)
Expected_fc_fit_Gold_ARIMA21<-fc_fit_Gold_ARIMA21$pred
low_fc_fit_Gold_ARIMA21<-fc_fit_Gold_ARIMA21$pred-qnorm(0.975)*fc_fit_Gold_ARIMA21$se
up_fc_fit_Gold_ARIMA21<-fc_fit_Gold_ARIMA21$pred+qnorm(0.975)*fc_fit_Gold_ARIMA21$se
cbind(low_fc_fit_Gold_ARIMA21, Expected_fc_fit_Gold_ARIMA21, up_fc_fit_Gold_ARIMA21)

plot.ts(logGold_ts, xlim=c(tahunawal,tahunakhir), ylim=c(8,9))
lines(Expected_fc_fit_Gold_ARIMA21, col="red")
lines(low_fc_fit_Gold_ARIMA21, col="blue")
lines(up_fc_fit_Gold_ARIMA21, col="blue")


y<-logGold_ts
S=150
h=1
error3.1.h<-c()
for (i in S:(length(y)-h))
{
  mymodel.sub<-arima(y[1:i],order=c(0,1,3),seasonal=c(0,0,0))
  predict.h<-predict(mymodel.sub,n.ahead=h)$pred[h]
  error3.1.h<-c(error3.1.h, y[i+h]-predict.h)
}

error3.2.h<-c()
for (i in S:(length(y)-h))
{
  mymodel.sub<-arima(y[1:i],order=c(3,1,0),seasonal=c(0,0,0))
  predict.h<-predict(mymodel.sub,n.ahead=h)$pred[h]
  error3.2.h<-c(error3.2.h,y[i+h]-predict.h)
}

error3.3.h<-c()
for (i in S:(length(y)-h))
{
  mymodel.sub<-arima(y[1:i],order=c(2,1,1),seasonal=c(0,0,0))
  predict.h<-predict(mymodel.sub,n.ahead=h)$pred[h]
  error3.3.h<-c(error3.3.h,y[i+h]-predict.h)
}
summary(abs(error3.1.h))
summary(abs(error3.2.h))
summary(abs(error3.3.h))
cbind(error3.1.h,error3.2.h,error3.3.h)

MAE_Gold_MA3<-mean(abs(error3.1.h))
MAE_Gold_AR3<-mean(abs(error3.2.h))
MAE_Gold_ARIMA21<-mean(abs(error3.3.h))
cbind(MAE_Gold_MA3,MAE_Gold_AR3,MAE_Gold_ARIMA21)

dm.test(error3.1.h,error3.2.h,h=h, power=1)
dm.test(error3.1.h,error3.3.h,h=h, power=1)
dm.test(error3.2.h,error3.3.h,h=h, power=1)

#with forecast horizon 1
#using Mean Absolute Error Criteria
#    MAE_Gold_MA3    MAE_Gold_AR3 MAE_Gold_ARIMA21
#      0.02776218      0.02738996       0.02740185
# MA3 vs AR3 p-value = 0.5038
# MA3 vs ARIMA21 p-value = 0.5386
# AR3 vs ARIMA21 p-value = 0.9731
#AR3 is preferable due to lowest least Mean ABsolute Error
#DM Test  Shows that all models is not significantly different than the other

MSE_Gold_MA3<-mean(error3.1.h^2)
MSE_Gold_AR3<-mean(error3.2.h^2)
MSE_Gold_ARIMA21<-mean(error3.3.h^2)
cbind(MSE_Gold_MA3,MSE_Gold_AR3,MSE_Gold_ARIMA21)

dm.test(error3.1.h,error3.2.h,h=h, power=2)
dm.test(error3.1.h,error3.3.h,h=h, power=2)
dm.test(error3.2.h,error3.3.h,h=h, power=2)

#with forecast horizon 1
#using Mean Square Error Criteria
#       MSE_Gold_MA3      MSE_Gold_AR3  MSE_Gold_ARIMA21
#        0.001281698       0.001230922      0.001210531
# MA3 vs AR3 p-value = 0.2157
# MA3 vs ARIMA21 p-value = 0.1529
# AR3 vs ARIMA21 p-value = 0.3871
#ARIMA 21 is preferable because the model has the least Mean Squared Error
#However DM Test Shows all models are not significantly different than the other





### 2.1 ARCH GARCH Model
fit_Ex_ARIMA11
plot(fit_Ex_ARIMA11$residuals, col="violet")
acf(fit_Ex_ARIMA11$residuals, col="violet")
Box.test(fit_Ex_ARIMA11$residuals, lag=max.lag, type="Ljung-Box") #indeed white noise
acf(fit_Ex_ARIMA11$residuals^2)
# ARIMA11 is concluded as valid and the most parsimonious model to explain Exchange
# the residual of ARIMA 22 is white noise
# However there are significant auro correlations in the squared residuals
# which indicates the presence of heteroskedasticity

fit_garch_Ex_1<-garchFit(~arma(3,0)+garch(1,1),data=dlogEx_ts)
plot(fit_garch_Ex_1)
summary(fit_garch_Ex_1)
# Model Validated but data is not normal as normality assumption of normality is rejected, use QMLE

fit_garch_Ex_2<-garchFit(~arma(2,2)+garch(1,1),cond.dist = "QMLE",data=dlogEx_ts)
plot(fit_garch_Ex_2)
summary(fit_garch_Ex_2)



# 3.1 Predictive Model for Exchange rate by gold Price

fit_dlm1 <- lm(dlogEx_ts ~ dlogGold_ts)
summary(fit_dlm1)
# dlogGold_ts  significant as covariates for dlogEx_ts
# R2= 50.94% of delta log Exchange are explained by deltalog Gold Price
# p-val < 0.00 significant thus all covariates are jointly significant to predict delta log Exchange

plot.ts(fit_dlm1$residuals)
acf(fit_dlm1$residuals)
pacf(fit_dlm1$residuals)
Box.test(fit_dlm1$residuals, lag = max.lag, type = "Ljung-Box")
# All graphics shows no significant auto correlations 
# supported by Q-test on the residuals 0.7913>0.05 thus do not reject Ho White noise and conclude that model is valid

# Engel -Granger test 1
fit_ci1<-lm(logEx_ts~logGold_ts)
res_fit_ci1<-fit_ci1$residuals
CADFtest(res_fit_ci1,type="drift", criterion="BIC", max.lag.y=max.lag)
#the test statistics ADF(2) = -1.571 is larger than the Engle-Granger ADF statistics for one explanatory variable -3.41
#thus accept the H0 of no cointegration
#logEx_ts~logGold_ts is not co-integrated thus regression between them is not possible
#however it is possible to regress delta logEx_ts~ delta logGold_ts

#Distributed Lag Model
lag <- 3
n <- length(dlogEx_ts)
dlogEx.0 <- dlogEx_ts[(lag+1):n]
dlogRes.0 <- dlogRes_ts[(lag+1):n]
dlogRes.1 <- dlogRes_ts[lag:(n-1)]
dlogEx.1 <- dlogEx_ts[lag:(n-1)]
dlogEx.2 <- dlogEx_ts[(lag-1):(n-2)]
dlogRes.2 <- dlogRes_ts[(lag-1):(n-2)]
dlogEx.3 <- dlogEx_ts[(lag-2):(n-3)]
dlogRes.3 <- dlogRes_ts[(lag-2):(n-3)]
fit_adlm <- lm(dlogEx.0 ~ dlogEx.1+dlogEx.2+dlogEx.3+dlogRes.1+dlogRes.2+dlogRes.3)
plot.ts(fit_adlm$residuals)
acf(fit_adlm$residuals)
Box.test(fit_adlm$residuals, lag = max.lag, type = "Ljung-Box")
summary(fit_adlm)

fit_adlm_nox <- lm(dlogEx.0 ~ dlogEx.1+dlogEx.2+dlogEx.3)
anova(fit_adlm,fit_adlm_nox)



# obsolete VAR 3

fit_varautom<-VAR(dlogdata,type="const",p=3)
summary(fit_varautom)
varautom_residuals<-resid(fit_varautom)

# if Response dlogEx -> R-Square: 0.1994 -- p: <0.00
# if Response dlogRes -> R-Square: 0.1587 -- p: <0.00
# if Response dlogGold -> R-Square: 0.1763 -- p: <0.00

# only 19.94% of variance dlogEx is explained by the lagged observations of 
#      dlogEx, dlogRes and dlogGold at lag 3
#      the F-sattistics for regression are jointly significant

par(mfrow=c(2,3))
acf(varautom_residuals[,1])
acf(varautom_residuals[,2])
acf(varautom_residuals[,3])
ccf(varautom_residuals[,1],varautom_residuals[,2])
ccf(varautom_residuals[,1],varautom_residuals[,3])
ccf(varautom_residuals[,2],varautom_residuals[,3])
par(mfrow=c(1,1))

# residuals collerogram VAR(3) for dlogEx, dlogRes and dlogGold and cross collerogram between the three var
#     indicating multivariate whitenoise


##### Obsolete bivariate analysis



