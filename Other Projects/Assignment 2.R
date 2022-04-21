#Read the data into R Studio and plot scatter plots of GPM versus all the X variables.
#Comment on these plots. Is there a relationship between the GPM and the possible X
#variables? How would you describe the relationship?
library(readr)
gas <- read_csv("gasconsumption.csv")
gas
par(mfrow=c(2,2))
plot(GPM~WT,data = gas)
plot(GPM~DIS,data = gas)
plot(GPM~NC,data = gas)
#For the first plot GPM vs WT, You can clearly see there is somewhat of a relationship most of the time it will cost more gas
#if the car is heavier
#For the second plot GPM vs DIS, same thing where the if the displacement of engine require more cube inches, it will require
#more gas per mile.
#For the las plot GPM vs MC you can tell that more cylinders you have in the engine will cost more gas per mile.

#Estimate a multiple regression model with the Y variable equal to GPM and the X
#variables equal to WT, DIS, and NC. Print out a summary of this model.
lm.fit <- lm(GPM ~WT+DIS+NC,data = gas)
summary(lm.fit)

#Use the output for this model to test the null hypothesis: ¦Â1 = ¦Â2 = ¦Â3 = 0 versus the
#alternative hypothesis: not all ¦Âi¡¯s are equal to 0. What does your result mean?

#p-value: < 2.2e-16 so we reject the hypothsis, that means atleast one of the ¦Â is not equal to 0.

#Based upon the output, are all of the 3 variables WT, DIS, and NC needed in the
#model? Justify your answer.

#I do not think all three variables are needed in this model becuase NC variable is not needed due to it is not helping the model.

#Perform the usual diagnostic checks for this model. Based upon your analysis is there
#any problems with this model?
par(mfrow=c(2,2))
plot(lm.fit$fitted.values,lm.fit$res)
plot(gas$WT,lm.fit$res)
plot(gas$DIS,lm.fit$res)
plot(gas$NC,lm.fit$res)

par(mfrow=c(1,2))
hist(lm.fit$res)
qqnorm(lm.fit$res)
shapiro.test(lm.fit$res)

#Is there anything that you would do to change the model? If yes, then make the
#appropriate changes and estimate a new model.

lm.fit2<- lm(GPM~WT,data=gas)
summary(lm.fit2)

par(mfrow=c(1,2))
plot(lm.fit2$fitted.values,lm.fit2$res)
plot(gas$WT,lm.fit2$res)

par(mfrow=c(1,2))
hist(lm.fit2$res)
qqnorm(lm.fit2$res)
shapiro.test(lm.fit2$res)


house<- read.csv("HousePrices.csv")
house

#Read the data into R Studio. Plot the following scatter plots: Price vs. SqFt, Price
#vs. Bed, Price vs. Bath, and Price vs. Offers. Comment on these plots. Use the following
#command to read in the data: price=read.csv("HousePrices.csv")
par(mfrow=c(2,2))
plot(Price~SqFt,data = house)
plot(Price~Bed,data = house)
plot(Price~Bath,data = house)
plot(Price~Offers,data = house)

par(mfrow=c(1,2))
boxplot(house$Price~house$Brick,main="Brick (Yes/No)")
boxplot(house$Price~house$Nbrhood,main="Neighborhood")

# Fit a multiple regression model with Price as the dependent variable and all the other
#variables as independent variables. Print out a summary of the model and perform the
#diagnostic checks for this model. Do the diagnostic checks suggest any problems with
#this model? Justify your answer.
lm.fit<- lm(Price~SqFt+Bed+Bath+Offers+Brick+Nbrhood,data = house)
summary(lm.fit)

par(mfrow=c(2,2))
plot(lm.fit$fitted.values,lm.fit$res)
plot(house$SqFt,lm.fit$res)
plot(house$Bed,lm.fit$res)
plot(house$Bath,lm.fit$res)
plot(house$Offers,lm.fit$res)
plot(as.factor(house$Brick),lm.fit$res,xlab='Brick',ylab='Residuals')
plot(as.factor(house$Nbrhood),lm.fit$res,xlab='Brick',ylab='Residuals')
par(mfrow=c(1,2))
hist(lm.fit$res)
qqnorm(lm.fit$res)
shapiro.test(lm.fit$res)

#3
flower=read.csv("meadowform.csv")
group=ifelse(flower$Time=="Early","E","L")
plot(flower$Flowers~flower$Intensity,pch=group)

fit<- lm(Flowers~Intensity+Time,data = flower)
summary(fit)
