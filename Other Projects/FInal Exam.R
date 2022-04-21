ames = read.csv('ames.csv')
library(dplyr)
ames.new<- mutate_if(ames, is.character, as.factor)

ames.new$logsale<- log(ames.new$Sale_Price)
set.seed(1)
idx<- sample(1:nrow(ames.new), ceiling(nrow(ames.new)/2))
ames.train<-ames.new[idx,-79]
ames.test<-ames.new[-idx,]

tree.ames<- tree(logsale~., data=ames.train)
summary(tree.ames)

plot(tree.ames)
text(tree.ames, pretty=0)
title("All Features Tree")

set.seed(2)
cv.tree.ames=cv.tree(tree.ames)
cv.tree.ames
plot(cv.tree.ames$size,cv.tree.ames$dev,type="b")

prune.cv.mytree = prune.tree(tree.ames, best=5)
plot(prune.cv.mytree)
text(prune.cv.mytree)
title("Pruned tree with tree size tuned by CV!")

yhat <- predict(prune.cv.mytree, ames.test)
mse.pruned = mean((yhat - ames.test$logsale)^2)
yhat.unpruned <- predict(tree.ames, ames.test)
mse.unpruned = mean((yhat.unpruned - ames.test$logsale)^2)
mse.pruned
mse.unpruned

library (randomForest)
set.seed (1)
bag.ames =randomForest(logsale~.,data=ames.train,mtry=80, importance =TRUE)
bag.ames

yhat.bag = predict(bag.ames ,newdata =ames.test)
mse.bagged = mean((yhat.bag-ames.test$logsale)^2)
mse.bagged

set.seed (1)
rf.ames =randomForest(logsale~.,data=ames.train,mtry=9, importance =TRUE)
yhat.rf = predict(rf.ames ,newdata =ames.test)
mse.rf = mean((yhat.rf -ames.test$logsale)^2)
mse.rf

library (gbm)
set.seed (1)
boost.ames =gbm(logsale~.,data=ames.train, distribution="gaussian",
                n.trees=500, interaction.depth=4)
yhat.boost=predict(boost.ames ,newdata=ames.test, n.trees=500)
mse.boost = mean((yhat.boost-ames.test$logsale)^2)
mse.boost


x<-read.csv("souvenir.csv")
library(TSA)
attach(x)
par(mfrow=c(1,1))
plot(Sales,ylab='Sales',xlab='Time',type='o')
BoxCox.ar(Sales)
plot(log(Sales),ylab='Sales',xlab='Time',type='o')
plot(diff(log(Sales)),main="Plot of the first difference",col="blue",type="o")
acf(diff(log(Sales)),lag.max = 36)
plot(diff(log(Sales),lag = 12),main="Plot of the 12th difference",col="blue",type="o")
acf(diff(log(Sales),lag = 12),lag.max = 36)
plot(diff(diff(log(Sales),lag = 12)),main="Plot of the 1st and 12th difference",col="blue",type="o")
acf(diff(diff(log(Sales),lag=12)),lag.max=36)
fit=arima(log(Sales),order=c(0,1,1),seasonal=list(order=c(0,1,0),period=12))
fit
library(TSA)
par(mfrow=c(2,2))
plot(rstandard(fit),ylab='Standardized Residuals',type='o')
hist(rstandard(fit))
qqnorm(residuals(fit))
qqline(residuals(fit))
acf(rstandard(fit))
Box.test(residuals(fit),lag=10,type="Ljung",fitdf=1)
detectAO(fit)
detectIO(fit)
#2

x2<-read.csv("emales.csv")
attach(x2)
plot(Employed,ylab='Employed',xlab='Time',type='o')
BoxCox.ar(Employed)
plot(diff(Employed), ylab='Differences in Employed',xlab='Time',type='o')
plot(diff(Employed,lag = 12),main="Plot of the 12th difference",col="blue",type="o")
plot(diff(diff(Employed,lag = 12)),main="Plot of the 1st and 12th difference",col="blue",type="o")

acf(diff(diff(Employed,lag = 12)),main="Plot of the 1st and 12th difference")
fit=arima(Employed,order=c(0,1,1),seasonal=list(order=c(0,1,1),period=12))
fit

par(mfrow=c(2,2))
plot(rstandard(fit),ylab='Standardized Residuals',type='o')
hist(rstandard(fit))
qqnorm(residuals(fit))
qqline(residuals(fit))
acf(rstandard(fit))
Box.test(residuals(fit),lag=10,type="Ljung",fitdf=1)

detectAO(fit)
detectIO(fit)
fit.io=arima(Employed,order=c(0,1,1),seasonal=list(order=c(0,1,1),period=12),io=c(18,21))
fit.io

par(mfrow=c(2,2))
plot(rstandard(fit.io),ylab='Standardized Residuals',type='o')
hist(rstandard(fit.io))
qqnorm(residuals(fit.io))
qqline(residuals(fit.io))
acf(rstandard(fit.io))

Box.test(residuals(fit.io),lag=10,type="Ljung",fitdf=4)
