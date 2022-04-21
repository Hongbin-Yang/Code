#1
temp<- read.csv("TemperatureChange.csv")
attach(temp)
plot(Change,col="red",main = "Change",type = 'o')
plot(diff(Change),col="red",main = "Difference Change",type = 'o')

acf(Change,lag.max = 15)

par(mfrow=c(1,2))
acf(diff(Change),type = "correlation")
pacf(diff(Change))

fit<- arima(Change,order = c(1,1,1))
fit
library(TSA)
par(mfrow=c(2,2))
plot(rstandard(fit),ylab='Standardized Residuals',type='o')
hist(rstandard(fit))
qqnorm(residuals(fit))
qqline(residuals(fit))
acf(rstandard(fit))
Box.test(residuals(fit),lag=10,type="Ljung",fitdf=2)

#2
tool<- read.csv("CuttingTool.csv")
attach(tool)
fit <- plot(Sales,col="red",main = "tool",type = 'o')

fit.1<- plot(diff(Sales),col="red",main = "tool",type = 'o')

par(mfrow=c(1,1))
acf(diff(Sales),lag.max = 15,type = "correlation")

par(mfrow=c(1,2))
acf(diff(Sales),lag.max = 15,type = "correlation")
pacf(diff(Sales))

fit2=arima(Sales,order =c(0,1,1))
fit2

library(TSA)
par(mfrow=c(2,2))
plot(rstandard(fit2),ylab='Standardized Residuals',type='o')
hist(rstandard(fit2))
qqnorm(residuals(fit2))
qqline(residuals(fit2))
acf(rstandard(fit2))
Box.test(residuals(fit2),lag=10,type="Ljung",fitdf=1)

#3
drunk<- read.csv("Drunkintakes.csv")
attach(drunk)
plot(Intakes,col="red",type='o',pch=17,xlab='Months',ylab='Intakes')
library(TSA)
fit.1= arima(Intakes,order=c(1,0,0))
plot(rstandard(fit.1),type='o')


detectAO(fit.1)
AO32=1*(seq(Intakes)==32)
AO33=1*(seq(Intakes)==33)
xreg=data.frame(AO32,AO33)
fit.ao= arimax(Intakes,order=c(1,0,0),xreg,seasonal=list(order=c(0,0,0),period=NA))
fit.ao

library(TSA)
par(mfrow=c(2,2))
plot(rstandard(fit.ao),ylab='Standardized Residuals',type='o')
hist(rstandard(fit.ao))
qqnorm(residuals(fit.ao))
qqline(residuals(fit.ao))
acf(rstandard(fit.ao))
Box.test(residuals(fit.ao),lag=10,type="Ljung",fitdf=4)

detectIO(fit.1)
fit.io=arimax(Intakes,order=c(1,0,0),io=c(35))
fit.io

par(mfrow=c(2,2))
plot(rstandard(fit.io),ylab='Standardized Residuals',type='o')
hist(rstandard(fit.io))
qqnorm(residuals(fit.io))
qqline(residuals(fit.io))
acf(rstandard(fit.io))

Box.test(residuals(fit.io),lag=10,type="Ljung",fitdf=3)


#4
ship<- read.csv("PortlandBusRidership.csv")
attach(ship)
plot(Riders,col="red",main = "tool",type = 'o')
library(TSA)
BoxCox.ar(y=Riders)

par(mfrow=c(1,2))
plot(log(Riders),col="red",main="Log(Riders)",type='o',xlab='Months')
acf(log(Riders))

par(mfrow=c(1,2))
plot(diff(log(Riders)),col="red",main="diff(Log(Riders))",type='o',xlab='Months')
acf(diff(log(Riders)))

par(mfrow=c(1,2))
acf(diff(log(Riders),lag = 12))
pacf(diff(log(Riders),lag = 12))


fit2 = arima(log(Riders),order=c(1,0,0),seasonal = list(order=c(0,1,1),period=12))
fit2

library(TSA)
par(mfrow=c(2,2))
plot(rstandard(fit2),ylab='Standardized Residuals',type='o')
hist(rstandard(fit2))
qqnorm(residuals(fit2))
qqline(residuals(fit2))
acf(rstandard(fit2))
Box.test(residuals(fit2),lag=36,type="Ljung",fitdf=2)

pred = predict(fit2,n.ahead=24)
plot(fit2,n.ahead=24,type='b',col="red",ylab='Riders',xlab='months')
pred$pred
pred$se

#5
start<- read.csv("Starts.csv")
attach(start)
plot(Starts,col="red",main = "tool",type = 'o')
library(TSA)
BoxCox.ar(y=Starts,method="yule-walker")

par(mfrow=c(1,2))
plot(log(Starts),col="red",main="Log(Starts)",type='o',xlab='Months')
acf(log(Starts))

par(mfrow=c(1,2))
plot(diff(log(Starts)),col="red",main="diff(Log(Starts))",type='o',xlab='Months')
acf(diff(log(Starts)))

par(mfrow=c(1,2))
acf(diff(log(Starts),lag = 12))
pacf(diff(log(Starts),lag = 12))

plot(diff(diff(log(Starts),lag = 12)),col="red",main="diff(diff(Log(starts),lag=12))",type='o',xlab='Months')

par(mfrow=c(1,2))
acf(diff(diff(log(Starts),lag = 12)))
pacf(diff(diff(log(Starts),lag = 12)))



fit2 = arima(log(Starts),order=c(0,1,1),seasonal = list(order=c(0,1,1),
                                                              period=12))
fit2

par(mfrow=c(2,2))
plot(rstandard(fit2),ylab='Standardized Residuals',type='o')
hist(rstandard(fit2))
qqnorm(residuals(fit2))
qqline(residuals(fit2))
acf(rstandard(fit2))
Box.test(residuals(fit2),lag=36,type="Ljung",fitdf=2)

pred = predict(fit2,n.ahead=24)
plot(fit2,n.ahead=24,type='b',col="red",ylab='Starts',xlab='months')
pred$pred
pred$se
