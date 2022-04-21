library(MASS)
data(Boston)

?Boston
dim(Boston)
names(Boston)
attach(Boston)



# linear regression model including all the indicators in the dataset:
model0<- lm(medv~., data = Boston)
summary(model0)

model1<- lm(medv~crim+zn+chas+nox+rm+dis+rad+tax+ptratio+black+lstat, data = Boston)
summary(model1)

#2
par(mfrow = c(2,2))
plot(model1)



library(MASS)
boxcox(medv~crim+zn+chas+nox+rm+dis+rad+tax+ptratio+black+lstat, data = Boston)



model1<- lm(log(medv)~crim+zn+chas+nox+rm+dis+rad+tax+ptratio+black+lstat, data = Boston)
par(mfrow = c(2,2))
plot(model1)


#Stepwise variable selection using AIC and BIC criteria.

nullmodel <- lm(medv~1, data = Boston)
fullmodel <- lm(medv~., data = Boston)

stepfit.aic <- step(nullmodel,
                    scope = list(lower=nullmodel, upper=fullmodel),
                    direction = "both", trace = 0)

stepfit.bic <- step(nullmodel,
                    scope = list(lower=nullmodel, upper=fullmodel),
                    direction = "both", trace = 0,
                    k=log(nrow(Boston)))

stepfit.aic$call
stepfit.bic$call

summary(stepfit.aic)
summary(stepfit.bic)

par(mfrow = c(2,2))
plot(stepfit.aic)
plot(stepfit.bic)




#LASSO variable selection.
install.packages("glmnet")
library(glmnet)
library(MASS)
data(Boston)

lm.fit <- lm(medv~., data = Boston)

# fitting lasso model
yind <- which(names(Boston)=="medv")
X <- scale(Boston[,-yind])
Y <- Boston[,yind]
lasso.fit <- glmnet(x=as.matrix(X), y=Y)

dim(coef(lasso.fit))
# 14 rows 76 columns
# we have 14 coefficients, 13 betas and 1 intercept

plot(lasso.fit)
# plotting lasso.fit
# at the very left of the plot you can see it is 0, as it increases and eventually everything becomes non-zero.
# On the very right it is empty which is also called sparse.
# our goal here is to get as much as non-zero coefficient
# L1 norm is a penalty term

plot(lasso.fit, xvar = "lambda")
# better view

## cross-validation to choose the optimal model
cv.lasso.fit <- cv.glmnet(x=as.matrix(X), y=Y)
plot(cv.lasso.fit)
# we are going to use this model to determine which model is the best

# now we are going to extract lambda values
cv.lasso.fit$lambda.min
cv.lasso.fit$lambda.1se

coef(lasso.fit, cv.lasso.fit$lambda.min)
coef(lasso.fit, cv.lasso.fit$lambda.1se)

### split data to training and testing
index <- sample(nrow(Boston), 0.8*nrow(Boston))
train <- Boston[index,]
test <- Boston[-index,]
trainX <- scale(train[,-yind])
trainY <- train[,yind]
testX <- scale(test[,-yind])
testY <- test[,yind]

lm<-lm(medv~crim+chas+nox+rm+dis+ptratio+black+lstat,data = Boston)
summary(lm)


lm<-lm(medv~chas+nox+rm+dis+ptratio+black+lstat,data = Boston)
par(mfrow = c(2,2))
plot(lm)
