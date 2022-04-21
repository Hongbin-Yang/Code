library(readr)
expend <- read_csv("Expend.csv")
par(mfrow=c(2,3))
plot(EX~ECAB,data = expend)
plot(EX~MET,data = expend)
plot(EX~GROW,data = expend)
plot(EX~YOUNG,data = expend)
plot(EX~OLD,data = expend)
boxplot(expend$EX~expend$WEST,main = "West")
library(MASS)
lm <- lm(EX~ECAB+MET+GROW+WEST, data=expend)
summary(lm)
par(mfrow=c(2,2))
plot(lm)
lm1 <- lm(EX~ECAB+poly(MET,2)+GROW+WEST, data=expend)
summary(lm1)
lm2 <- lm(EX~ECAB+poly(MET,2)+WEST, data=expend)
summary(lm2)
par(mfrow=c(2,2))
plot(lm2)

expend[lev>0.07,c(2:6)]
par(mfrow=c(1,2))
lev=hat(model.matrix(lm2))
plot(lev,pch=17,col="red")


cook=cooks.distance(lm2)
plot(cook,pch=17,col="blue")

lm2.1<-lm(EX~ECAB+poly(MET,2)+WEST, data=expend, subset= -c(47))
summary(lm2.1)
par(mfrow=c(2,2))
plot(lm2.1)

newdata= data.frame(ECAB=(c(100,100,100,150,150,150,175,175,175)),
                    MET=(c(20,40,60,20,40,60,20,40,60)),
                    WEST=(c(0,0,0,0,0,0,0,0,0)))
preds<- predict(lm2,newdata)
preds1<- predict(lm2.1,newdata)



#2
donner <- read_csv("Donner.csv")
par(mfrow=c(1,1))
boxplot(donner$Age~donner$Surv, main="Surv")

xtabs(~ Gender+Surv, data=donner)
chisq.test(donner$Gender,donner$Surv)
xtabs(~ MultFam+Surv, data=donner)
chisq.test(donner$MultFam,donner$Surv)

lm<-lm(Surv~Age+Gender+MultFam,data = donner)
summary(lm)

lm1<-lm(Surv~Age+MultFam,data = donner)
summary(lm1)

lm3 <- glm(Surv~Age+MultFam+Age*MultFam, family="binomial", data=donner)
summary(lm3)

lm4 <- glm(Surv~poly(Age,2)+MultFam, family="binomial", data=donner)
summary(lm4)

library(ResourceSelection)
hoslem.test(lm3$y, fitted(lm3), g=10)
hoslem.test(lm4$y, fitted(lm4), g=10)

cereals <- read.csv("cereals.csv")
par(mfrow=c(3,4))
boxplot(cereals$Rating~cereals$Manuf)
boxplot(cereals$Rating~cereals$Type)
plot(Rating~Calories, data=cereals)
plot(Rating~Protein, data=cereals)
plot(Rating~Fat, data=cereals)
plot(Rating~Sodium, data=cereals)
plot(Rating~Fiber, data=cereals)
plot(Rating~Carbo, data=cereals)
plot(Rating~Sugars, data=cereals)
plot(Rating~Potass, data=cereals)
plot(Rating~Vitamins, data=cereals)

lm<-lm(Rating~Manuf+Type+Calories+Protein+Fat+Sodium+Fiber+Carbo+Sugars+Potass+Vitamins, data=cereals)
summary(lm)


lm1<-lm(Rating~Calories+Protein+Fat+Sodium+Fiber+Carbo+Sugars+Vitamins, data=cereals)
summary(lm1)

par(mfrow=c(2,2))
plot(lm)

#9/76=0.11*2=0.23


par(mfrow=c(1,1))
lev=hat(model.matrix(lm1))
plot(lev,pch=17,col="red")
cereals[lev>0.23,c(4:10,12)]

cook=cooks.distance(lm1)
plot(cook,pch=17,col="blue")

lm<-lm(Rating~Calories+Protein+Fat+Sodium+Fiber+Carbo+Sugars+Vitamins, data=cereals,subset=-c(4))
summary(lm)
par(mfrow=c(2,2))
plot(lm)


model<- glm(Rating~Calories+Protein+Fat+Sodium+Fiber+Carbo+Sugars+Vitamins, data=cereals)

MSE_LOOCV = cv.glm(cereals,model)
MSE_LOOCV$delta[1]
