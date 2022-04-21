Chem<-read.csv("ChemicalConcentration.csv")
attach(Chem)
plot(Con,col="red",main = "Concentration",type = 'o')


acf(Con,lag.max = 15)
par(mfrow=c(1,2))
acf(Con,lag.max = 15)
pacf(Con,lag.max = 15)

fit<-arima(Con,order = c(2,0,0))
fit
#0.4245-2*0.0687,0.4245+2*0.0687 =(0.2871,0.5619) 
#0.2531-2*0.0689,0.2531+2*0.0689 =(0.1153,0.3909)

library(TSA)
par(mfrow=c(2,2))
plot(rstandard(fit),ylab='Standardized Residuals',type='o')
hist(rstandard(fit))
qqnorm(residuals(fit))
qqline(residuals(fit))
acf(rstandard(fit))
Box.test(residuals(fit),lag=10,type="Ljung",fitdf=2)

fit1<-arima(Con,order = c(2,0,2))
fit1
#-0.3968-(2*0.3251),-0.3968+(2*0.3251) = (-1.047,0.2534)
fit2<-arima(Con,order = c(3,0,0))
fit2
#0.0793-(2*0.0719),0.0793+(2*0.0719) = (-0.0645,0.2231)

pred = predict(fit,n.ahead=10)
plot(fit,n.ahead=10,type='b',col="red",ylab='Concentration',xlab='batch')
pred$pred
pred$se

upper = pred$pred +1.96*pred$se
lower= pred$pred-1.96*pred$se
preddata=data.frame(upper,lower,'forecast'=pred$pred)
preddata = exp(preddata)
preddata

time<-read.csv("time.csv")
attach(time)
plot(Time,col="red",main = "Time",type = 'o')


par(mfrow=c(1,2))
acf(Time,lag.max = 15)
pacf(Time,lag.max = 15)


fit<-arima(Time,order = c(3,0,0))
fit
library(TSA)
par(mfrow=c(2,2))
plot(rstandard(fit),ylab='Standardized Residuals',type='o')
hist(rstandard(fit))
qqnorm(residuals(fit))
qqline(residuals(fit))
acf(rstandard(fit))
Box.test(residuals(fit),lag=10,type="Ljung",fitdf=3)

