power=read.csv("powerplant.csv")
library(leaps)
fit1.full=regsubsets(C~.,power, nvmax = 10)
summary(fit1.full)
reg.summary=summary(fit1.full)
reg.summary$adjr2
plot(reg.summary$adjr2,xlab="Number of Variables", ylab="Adjusted RSq", type = "l")
reg.summary$cp
plot(reg.summary$cp,xlab="Number of Variables", ylab=" Cp", type = "l")
reg.summary$bic
plot(reg.summary$bic,xlab="Number of Variables", ylab=" BIC", type = "l")

par(mfrow = c(2,2))

fit1.for=regsubsets(Pct.BF~.,bf, nvmax = 13,method = "forward")
summary(fit1.for)
reg.summary=summary(fit1.for)
reg.summary$adjr2
reg.summary$cp
reg.summary$bic
plot(reg.summary$adjr2,xlab="Number of Variables", ylab="Adjusted RSq", type = "l")
plot(reg.summary$cp,xlab="Number of Variables", ylab=" Cp", type = "l")
plot(reg.summary$bic,xlab="Number of Variables", ylab=" BIC", type = "l")
which.max(reg.summary$adjr2)
which.min(reg.summary$cp)
which.min(reg.summary$bic)

coef(fit1.for,8)
coef(fit1.for,6)
coef(fit1.for,3)

lm<-lm(Pct.BF~ Age+Weight+Neck+Abdomen+Thigh+Bicep+Forearm+Wrist,data=bf[train,] )
summary(lm)
lm1<-lm(Pct.BF~ Age+Weight+Abdomen+Thigh+Bicep+Wrist,data=bf[train,] )
summary(lm1)
lm2<-lm(Pct.BF~ Weight+Abdomen+Wrist,data=bf[train,] )
summary(lm2)
lm3<-lm(Pct.BF~ Age+Abdomen+Wrist,data=bf[train,] )
summary(lm3)

plot(lm3$res~lm3$fitted)
hist(lm3$res)
qqnorm(lm3$res)
shapiro.test(lm3$res)

fit1.bac=regsubsets(Pct.BF~.,bf, nvmax = 13,method = "backward")
summary(fit1.bac)
reg.summary=summary(fit1.bac)
reg.summary$adjr2
reg.summary$cp
reg.summary$bic
plot(reg.summary$adjr2,xlab="Number of Variables", ylab="Adjusted RSq", type = "l")
plot(reg.summary$cp,xlab="Number of Variables", ylab=" Cp", type = "l")
plot(reg.summary$bic,xlab="Number of Variables", ylab=" BIC", type = "l")
which.max(reg.summary$adjr2)
which.min(reg.summary$cp)
which.min(reg.summary$bic)

coef(fit1.bac,10)
coef(fit1.bac,6)
coef(fit1.bac,4)

lm<-lm(Pct.BF~ Age+Height+Neck+Chest+Abdomen++Hip+Thigh+Bicep+Forearm+Wrist,data=bf[train,] )
summary(lm)
lm1<-lm(Pct.BF~ Age+Height+Neck+Abdomen+Forearm+Wrist,data=bf[train,] )
summary(lm1)
lm2<-lm(Pct.BF~ Age+Height+Abdomen+Wrist,data=bf[train,] )
summary(lm2)

lm3<-lm(Pct.BF~ Height+Abdomen+Wrist,data=bf[train,] )
summary(lm3)

plot(lm3$res~lm3$fitted)
hist(lm3$res)
qqnorm(lm3$res)
shapiro.test(lm3$res)

lm<-lm(Pct.BF~ Age+Abdomen+Wrist,data=bf)
summary(lm)
plot(lm$res~lm$fitted)
hist(lm$res)
qqnorm(lm$res)
shapiro.test(lm$res)


high<-read.csv("highway.csv")
attach(high)
plot(log(Rate)~Len)
plot(log(Rate)~Adt)
plot(log(Rate)~Trks)
plot(log(Rate)~Slim)
plot(log(Rate)~Lwid)
plot(log(Rate)~Shld)
plot(log(Rate)~Itg)
plot(log(Rate)~Sigs)
plot(log(Rate)~Acpt)
plot(log(Rate)~Lane)
boxplot(log(Rate)~Fai)
boxplot(log(Rate)~Pa)
boxplot(log(Rate)~Ma)

library(MASS)
boxcox(Rate~.,data = high)

lm<-lm(log(Rate)~.,data = high)
summary(lm)

fit1.full=regsubsets(log(Rate)~.,high, nvmax = 13)
summary(fit1.full)
reg.summary=summary(fit1.full)
reg.summary$adjr2
reg.summary$cp
reg.summary$bic
plot(reg.summary$adjr2,xlab="Number of Variables", ylab="Adjusted RSq", type = "l")
plot(reg.summary$cp,xlab="Number of Variables", ylab=" Cp", type = "l")
plot(reg.summary$bic,xlab="Number of Variables", ylab=" BIC", type = "l")
which.max(reg.summary$adjr2)
which.min(reg.summary$cp)
which.min(reg.summary$bic)

coef(fit1.full,4)
lm<-lm(log(Rate)~Len+Slim+Sigs+Pa,data = high)
summary(lm)


plot(lm$res~lm$fitted)
hist(lm$res)
qqnorm(lm$res)
shapiro.test(lm$res)

library(TSA)
concentration=read.csv("ChemicalConcentration.csv")
plot(concentration$Con,ylab='Chemical Concentration Numbers',xlab='Time',type='o')
acf(concentration$Con,ci.type='ma')
pacf(concentration$Con)
fit=arima(concentration$Con,order =c(2,0,0))
fit

par(mfrow=c(2,2))

plot(rstandard(fit),ylab='Standardized Residuals',type='o')
hist(rstandard(fit))
qqnorm(residuals(fit))
qqline(residuals(fit))
acf(rstandard(fit))
Box.test(residuals(fit),lag=10,type="Ljung",fitdf=2)




fit1=arima(concentration$Con,order =c(2,0,1))
fit1

-0.1694+2*0.1113
-0.1694-2*0.1113

par(mfrow=c(2,2))

plot(rstandard(fit1),ylab='Standardized Residuals',type='o')
hist(rstandard(fit1))
qqnorm(residuals(fit1))
qqline(residuals(fit1))
acf(rstandard(fit1))
Box.test(residuals(fit1),lag=10,type="Ljung",fitdf=2)



fit1=arima(concentration$Con,order =c(3,0,0))
fit1

0.0793+2*0.0719
0.0793-2*0.0719

par(mfrow=c(2,2))

plot(rstandard(fit1),ylab='Standardized Residuals',type='o')
hist(rstandard(fit1))
qqnorm(residuals(fit1))
qqline(residuals(fit1))
acf(rstandard(fit1))
Box.test(residuals(fit1),lag=10,type="Ljung",fitdf=2)

plot(fit,n.ahead=10,type='b')
predict(fit,n.ahead=10)


time<-read.csv("time.csv")
attach(time)
par(mfrow=c(2,2))
plot(Time,type = "o")
acf(Time)
pacf(Time)

fit1=arima(time$Time,order =c(1,0,1))
fit1
-0.6805+2*0.0816
-0.6805-2*0.0816

Box.test(residuals(fit1),lag=10,type="Ljung",fitdf=2)
plot(rstandard(fit1),ylab='Standardized Residuals',type='o')
hist(rstandard(fit1))
qqnorm(residuals(fit1))
qqline(residuals(fit1))
acf(rstandard(fit1))

plot(fit1,n.ahead=10,type='b')
predict(fit1,n.ahead=10)



