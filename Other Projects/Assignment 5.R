library(readr)
highway<- read.csv("highway.csv")
library(leaps)
fit1.full=regsubsets(Rate~., data = highway, nvmax=13)
summary(fit1.full)

reg.summary=summary(fit1.full)
reg.summary$cp
plot(reg.summary$cp,xlab="Number of Variables", ylab="Cp", type = "b",col="cyan",pch=17)
which.min(reg.summary$cp)

fit1.for=regsubsets(Rate~., data =highway,nvmax = 13,method="forward")
summary(fit1.for)

reg.summary1=summary(fit1.for)

reg.summary1$cp
plot(reg.summary1$cp,xlab="Number of Variables", ylab="Cp", type = "b",col="cyan",pch=17)
which.min(reg.summary1$cp)


fit1.bac=regsubsets(Rate~., data =highway,nvmax = 13,method="backward")
summary(fit1.bac)
reg.summary2=summary(fit1.bac)
reg.summary2$cp
plot(reg.summary2$cp,xlab="Number of Variables", ylab="Cp", type = "b",col="cyan",pch=17)
which.min(reg.summary2$cp)

library(boot)
set.seed(1)
glm.fit=glm(Rate~Len+Slim+Sigs+Acpt+Fai,data = highway)
cv.error=cv.glm(highway,glm.fit,K=10)$delta[1]
cv.error

set.seed(1)
glm.fit1=glm(Rate~Len+Slim+Acpt+Fai,data = highway)
cv.error1=cv.glm(highway,glm.fit1,K=10)$delta[1]
cv.error1

set.seed(1)
glm.fit2=glm(Rate~Len+Slim+Sigs+Acpt+Pa,data = highway)
cv.error2=cv.glm(highway,glm.fit2,K=10)$delta[1]
cv.error2

par(mfrow=c(2,2))
plot(glm.fit1)


cereals<- read.csv("cereals.csv")
cereals<- cereals[complete.cases(cereals),]
cereals = cereals[-c(5,22),-1]
head(cereals)

fit1.full=regsubsets(Rating~., data = cereals, nvmax=11)
summary(fit1.full)

reg.summary=summary(fit1.full)
reg.summary$adjr2
plot(reg.summary$adjr2,xlab="Number of Variables", ylab="Adjusted RSq", type = "b",col="blue",pch=15)
which.max(reg.summary$adjr2)


fit1.for=regsubsets(Rating~., data =cereals,nvmax = 11,method="forward")
summary(fit1.for)

reg.summary1=summary(fit1.for)

reg.summary1$adjr2
plot(reg.summary1$adjr2,xlab="Number of Variables", 
     ylab="Adjusted RSq", type = "b",col="blue",pch=15)
which.max(reg.summary1$adjr2)


fit1.bac=regsubsets(Rating~., data =cereals,nvmax = 11,method="backward")
summary(fit1.bac)
reg.summary2=summary(fit1.bac)
reg.summary2$adjr2
plot(reg.summary2$adjr2,xlab="Number of Variables", 
     ylab="Adjusted RSq", type = "b",col="blue",pch=15)
which.max(reg.summary2$adjr2)


set.seed(1)
glm.fit=glm(Rating~+Calories+Protein+Fat+Sodium+Fiber+Carbo+Sugars+Potass+Vitamins,data = cereals)
cv.error=cv.glm(cereals,glm.fit,K=10)$delta[1]
cv.error

par(mfrow=c(2,2))
plot(glm.fit)

