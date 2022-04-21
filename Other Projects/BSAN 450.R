library(readr)
fifa19data <- read_csv("fifa19data.csv")
set.seed(100)
sample_players<- sample(1:nrow(fifa19data),100)
fifa19data_sample <- fifa19data[sample_players,]
plot(SprintSpeed~Acceleration,data=fifa19data_sample,pch=3,col="red")

lm.fit <- lm(SprintSpeed~Acceleration,data=fifa19data_sample)
summary(lm.fit)
plot(SprintSpeed~Acceleration,data=fifa19data_sample,pch=3,col="red")
abline(lm.fit,col="blue")
confint(lm.fit,level = 0.95)
lm.fit$res[1:5]

par(mfrow=c(1,2))
plot(lm.fit$fitted.values,lm.fit$res)
plot(fifa19data_sample$Acceleration,lm.fit$res)

par(mfrow=c(1,2))
hist(lm.fit$res)
qqnorm(lm.fit$res)
shapiro.test(lm.fit$res)

set.seed(1)
idx<- sample(1:nrow(fifa19data[-c(sample_players),]),10,replace = F)
newplayers_Acceleration<- fifa19data[idx,'Acceleration']

# This is the true SprintSpeed score. We will not use this for prediction.
newplayers_SprintSpeed<- fifa19data[idx,'SprintSpeed']

pred_1<- predict(lm.fit ,data.frame(Acceleration=newplayers_Acceleration),interval = "prediction",level=0.95)
cbind(data.frame(Acceleration=newplayers_Acceleration),pred_1)

newdata = data.frame(Acceleration=newplayers_Acceleration)
newdata$SprintSpeed_true<- newplayers_SprintSpeed
plot(newdata$Acceleration,pred_1[,1], xlab ="Acceleration",ylab="SprintSpeed",type="l",pch=17,col="blue",ylim=c(30,110))
points(newdata$Acceleration,pred_1[,2],type="l",col="red")
points(newdata$Acceleration,pred_1[,3],type="l",col="red")
# Plotting the True SprintSpeed score of the 10 new players in green triangles.
points(newdata$Acceleration,newdata$SprintSpeed_true,pch=17,col="green")

stop <- read.csv("stopdistance.csv")
attach(stop)
plot(Stop.Dist~Speed)

lm.fit <- lm(Stop.Dist~Speed)

#print out the results
summary(lm.fit)

par(mfrow=c(1,3))
plot(lm.fit$res~lm.fit$fitted)
hist(lm.fit$res)
qqnorm(lm.fit$res)
shapiro.test(lm.fit$res)

library(MASS)
bc<- boxcox(Stop.Dist~Speed)
lambda_bc<- bc$x[which.max(bc$y)]
lambda_bc
stop$ynew<- (stop$Stop.Dist)^{lambda_bc}
attach(stop)




