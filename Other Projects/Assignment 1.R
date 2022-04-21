library(readr)
ironcontent <- read_csv("ironcontent.csv")
ironcontent
plot(Chem~Magn,data = ironcontent)
lm.fit <- lm(Chem~Magn,data = ironcontent)
summary(lm.fit)

lm.fit$res[1:5]
par(mfrow=c(1,2))
hist(lm.fit$res)
qqnorm(lm.fit$res)
shapiro.test(lm.fit$res)
confint(lm.fit,level = 0.95)

Magn_sample <- data.frame(Magn = 20)

pred_2<- predict(lm.fit ,data.frame(Magn_sample),interval = "confidence",level=0.95)

ManSalary <- read_csv("ManSalary.csv")
par(mfrow=c(1,2))
plot(ManSalary$Experience,ManSalary$Salary)
lm.fit<- lm(Salary~Experience,data = ManSalary)
summary(lm.fit)
lm.fit$res[1:5]
par(mfrow=c(1,2))
plot(lm.fit$fitted.values,lm.fit$residuals)
plot(ManSalary$Experience,lm.fit$res)
par(mfrow=c(1,2))
hist(lm.fit$res)
qqnorm(lm.fit$res)


library(MASS)
bc<- boxcox(ManSalary$Salary~ManSalary$Experience)
lambda_bc<- bc$x[which.max(bc$y)]
lambda_bc
ManSalary$New<- (ManSalary$Salary)^{lambda_bc}
attach(ManSalary)
par(mfrow=c(1,2))
plot(ManSalary$New~Experience,ylab = 'Salary transformed')
plot(ManSalary$Salary~ManSalary$Experience)

lm.fit.trans <- lm(ManSalary$New~Experience)

summary(lm.fit.trans)

par(mfrow=c(2,3))
plot(lm.fit$res~lm.fit$fitted)
hist(lm.fit$res)
qqnorm(lm.fit$res)
plot(lm.fit.trans$res~lm.fit.trans$fitted)
hist(lm.fit.trans$res)
qqnorm(lm.fit.trans$res)
shapiro.test(lm.fit.trans$res)
shapiro.test(lm.fit$res)


BreakDown <- read_csv("breakdown.csv")
par(mfrow=c(1,2))
plot(BreakDown$Voltage,BreakDown$Time)
lm.fit<- lm(Time~Voltage,data = BreakDown)
summary(lm.fit)
lm.fit$res[1:5]
par(mfrow=c(1,2))
plot(lm.fit$fitted.values,lm.fit$residuals)
plot(BreakDown$Voltage,lm.fit$res)
par(mfrow=c(1,2))
hist(lm.fit$res)
qqnorm(lm.fit$res)

library(MASS)
bc<- boxcox(BreakDown$Time~BreakDown$Voltage)
lambda_bc<- bc$x[which.max(bc$y)]
lambda_bc
BreakDown$New<- (BreakDown$Time)^{lambda_bc}
attach(BreakDown)
par(mfrow=c(1,2))
plot(BreakDown$New~Voltage,ylab = 'Time transformed')
plot(BreakDown$Time~BreakDown$Voltage)

lm.fit.trans <- lm(BreakDown$New~Voltage)

summary(lm.fit.trans)

par(mfrow=c(2,3))
plot(lm.fit$res~lm.fit$fitted)
hist(lm.fit$res)
qqnorm(lm.fit$res)
plot(lm.fit.trans$res~lm.fit.trans$fitted)
hist(lm.fit.trans$res)
qqnorm(lm.fit.trans$res)


