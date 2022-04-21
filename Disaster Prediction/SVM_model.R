dtm<-read.csv("disaster.csv")

set.seed(1)
idx= sample(1:nrow(dtm), ceiling(0.7*nrow(dtm)))
train= dtm[idx,]
test= dtm[-idx,]

library(e1071)
svm.model <- svm(formula = target ~ .,
                 data = train,
                 type = 'C-classification',
                 kernel = 'radial')

svm.model
summary(svm.model)
svm_pred <-  predict(svm.model, newdata = test[,-291])
cm2 <- table(test$target , svm_pred)
cm2
error_metric <- function(cm2){
  TN = cm2[1,1]
  TP = cm2[2,2]
  FP = cm2[1,2]
  FN = cm2[2,1]
  accuracy=(TP+TN)/(TP+FP+TN+FN)
  precision =(TP)/(FP+TP)
  recall=(TP)/(FN+TP)
  F_score = 2* ((precision*recall)/(precision+recall))
  CER = (FN+FP)/(TN+TP+FP+FN)
  print(paste("SVM Accuracy",round(accuracy,2)))
  print(paste("SVM Precision",round(precision,2)))
  print(paste("SVM Recall",round(recall,2)))
  print(paste("SVM F_score",round(F_score,2)))
  print(paste("SVM CER",round(CER,2)))
}

SVM_results <- error_metric(cm2)
SVM_results

sample_submission <-  data.frame("id"=test$id,"target"= svm_pred)
