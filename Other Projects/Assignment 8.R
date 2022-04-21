install.packages("ISLR")
library(ISLR)
attach(Carseats)
summary(Carseats)
names(Carseats)
Carseats <- na.omit(Carseats)
hist(Carseats$Sales)

set.seed(1)
idx= sample(1:nrow(Carseats), ceiling(nrow(Carseats)/2))
Carseats.train = Carseats[idx,]
Carseats.test = Carseats[-idx,]


library(tree)
tree.Carseats= tree(Sales~., data=Carseats.train)
summary(tree.Carseats)
plot(tree.Carseats)
text(tree.Carseats, pretty=0)
title("All Features Tree")


set.seed(1)
cv.tree.Carseats= cv.tree(tree.Carseats, K=3)
cv.tree.Carseats
plot(cv.tree.Carseats$size, cv.tree.Carseats$dev, type="b")
cv.tree.Carseats$size[which.min(cv.tree.Carseats$dev)]
prune.cv.mytree= prune.tree(tree.Carseats, best=6)
plot(prune.cv.mytree)
text(prune.cv.mytree)
title("Pruned Tree")
yhat <- predict(prune.cv.mytree, Carseats.test)
plot(yhat, Carseats.test$Sales)
abline(0,1)
PrunedMSE<-mean((yhat-(Carseats.test$Sales))^2)
PrunedMSE
yhat.unpruned <- predict(tree.Carseats, Carseats.test)
mean((yhat.unpruned-(Carseats.test$Sales))^2)

set.seed(1)
idx= sample(1:nrow(Carseats), ceiling(0.5*nrow(Carseats)))
Carseats1.train= Carseats[idx,]
Carseats1.test= Carseats[-idx,]
library(randomForest)
set.seed(1)
bag.Carseats= randomForest(Sales~.,data=Carseats1.train,mtry=10,importance=TRUE)
bag.Carseats
varImpPlot(bag.Carseats)
yhat.bag= predict(bag.Carseats, newdata=Carseats1.test)
plot(yhat.bag, Carseats1.test$Sales)
abline(0,1)
BagMSE<-mean((yhat.bag- Carseats1.test$Sales)^2)
BagMSE
set.seed(1)
rf.Carseats = randomForest(Sales~., data=Carseats1.train,mtry=3,importance=TRUE)
yhat.rf = predict(rf.Carseats, newdata=Carseats1.test)
rfMSE<-mean((yhat.rf- Carseats1.test$Sales)^2)
rfMSE


library(gbm)
set.seed(1)
boost.Carseats= gbm(Sales~., data=Carseats1.train, distribution="gaussian", n.trees=500, interaction.depth=4)
summary(boost.Carseats)
yhat.boost=predict(boost.Carseats, newdata=Carseats1.test, ntrees=500)
boostMSE<-mean((yhat.boost-Carseats1.test$Sales)^2)
boostMSE

ames<- read.csv("ames.csv")
ames
ames<- na.omit(ames)

hist(ames$Sale_Price)
hist(log(ames$Sale_Price))

set.seed(1)
idx= sample(1:nrow(ames), ceiling(nrow(ames)/2))
ames.train = ames[idx,]
ames.test = ames[-idx,]

tree.ames= tree(log(Sale_Price)~., data=ames.train)
summary(tree.ames)
plot(tree.ames)
text(tree.ames, pretty=0)
title("All Features Tree")

set.seed(1)
cv.tree.ames= cv.tree(tree.ames, K=3)
cv.tree.ames
plot(cv.tree.ames$size, cv.tree.ames$dev, type="b")
cv.tree.ames$size[which.min(cv.tree.ames$dev)]
prune.cv.mytree= prune.tree(tree.ames, best=6)
plot(prune.cv.mytree)
text(prune.cv.mytree)
title("Pruned Tree")
yhat <- predict(prune.cv.mytree, ames.test)
plot(yhat, log(ames.test$Sale_Price))
abline(0,1)
pruneMSE<-mean((yhat- log(ames.test$Sale_Price))^2)
pruneMSE
yhat.unpruned <- predict(tree.ames, ames.test)
mean((yhat.unpruned- log(ames.test$Sale_Price))^2)


#2(d)
set.seed(1)
idx= sample(1:nrow(ames), ceiling(0.5*nrow(ames)))
ames1.train= ames[idx,]
ames1.test= ames[-idx,]
library(randomForest)
set.seed(1)
bag.ames= randomForest(log(Sale_Price)~.,data=ames1.train,mtry=80,importance=TRUE)
bag.ames
varImpPlot(bag.ames)
yhat.bag= predict(bag.ames, newdata=ames1.test)
plot(yhat.bag, log(ames1.test$Sale_Price))
abline(0,1)
bagMSE<-mean((yhat.bag- log(ames1.test$Sale_Price))^2)
bagMSE


#2(e)
set.seed(1)
rf.ames = randomForest(log(Sale_Price)~., data=ames1.train,mtry=9,importance=TRUE)
yhat.rf = predict(rf.ames, newdata=ames1.test)
rfMSE<-mean((yhat.rf- log(ames1.test$Sale_Price))^2)
rfMSE

#2(f)
library(gbm)
set.seed(1)
boost.ames= gbm(log(Sale_Price)~., data=ames1.train, distribution="gaussian", n.trees=500, interaction.depth=4)
summary(boost.ames)
yhat.boost=predict(boost.ames, newdata=ames1.test, ntree=500)
boostMSE<-mean((yhat.boost-log(ames1.test$Sale_Price))^2)
boostMSE













