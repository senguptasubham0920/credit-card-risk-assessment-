#library 
library(tidyverse)
library(dplyr)
library(caret)
library(randomForest)
library(e1071)
library(rpart)
#importing dataset
dataset = read.csv("creditrisk.csv")
str(dataset)

#exploratory data analysis 

dataset$SEX = as.factor(dataset$SEX)
dataset$EDUCATION = as.factor(dataset$EDUCATION)
dataset$MARRIAGE = as.factor(dataset$MARRIAGE)
dataset$PAY_0 = as.factor(dataset$PAY_0)
dataset$PAY_2 = as.factor(dataset$PAY_2)
dataset$PAY_3 = as.factor(dataset$PAY_3)
dataset$PAY_4 = as.factor(dataset$PAY_4)
dataset$PAY_5 = as.factor(dataset$PAY_5)
dataset$PAY_6 = as.factor(dataset$PAY_6)
dataset$default.payment = as.factor(dataset$default.payment)

str(dataset)

#missing values
sapply(dataset,function(x){sum(is.na(x))})



#EDA
table(dataset$default.payment,dataset$SEX)
ggplot(dataset)+
  geom_bar(aes(x = default.payment, fill= SEX),width = 0.5)


table(dataset$default.payment,dataset$EDUCATION)
ggplot(dataset)+
  geom_bar(aes(x = default.payment,fill = EDUCATION),width = .5)

round(prop.table(table(dataset$default.payment,dataset$MARRIAGE)),3)
ggplot(dataset)+
  geom_bar(aes(x = default.payment,fill = MARRIAGE),width = .5)


ggplot(dataset)+
  geom_bar(aes(x = AGE))+
  facet_wrap(default.payment~.)

ggplot(dataset)+
  geom_col(aes(x = EDUCATION,y = mean(LIMIT_BAL)),fill = 'blue')+
  facet_grid(SEX~default.payment)
  

#Visualizing the insight drawn 
dataset%>%
  group_by(AGE,EDUCATION,MARRIAGE)%>%
  filter(AGE %in% c(24:31),EDUCATION=="1",MARRIAGE %in% c(2))%>%
  count(default.payment)


#predictive data analysis

dataset = dataset[-1]
set.seed(123)
intrain<-createDataPartition(y=dataset$default.payment,p=0.700,list=FALSE)
training_set<-dataset[intrain,]
test_set<-dataset[-intrain,]
nrow(training_set)
nrow(test_set)

#scaling the values 

training_set[c(1,5,12:23)]= scale(training_set[c(1,5,12:23)])

test_set[c(1,5,12:23)] = scale(test_set[c(1,5,12:23)])
head(training_set)


#Ensemble Classification Algorithm (Random Forest)
folds = createFolds(training_set$default.payment, k = 10)
cv_rf = lapply(folds, function(x) {
  training_fold = training_set[-x, ]
  test_fold = training_set[x, ]
  classifier = randomForest(formula = default.payment ~ .,
                   data = training_fold,
                   ntree = 100)
  y_pred = predict(classifier, newdata = test_fold[-24])
  cm = table(test_fold[, 18], y_pred)
  accuracy = (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
  recall = cm[2,2]/(cm[2,2]+cm[2,1])
  precision = cm[2,2]/(cm[2,2]+cm[1,2])
  F1_score = (2*recall*precision)/(recall+precision)
  metrics_rf = c(accuracy,recall,F1_score)
  return(metrics_rf)
  
})
cv_rf = data.frame(cv_rf)
rownames(cv_rf) = c("Accuracy","Recall","F1_score")
cv_rf = rowMeans(cv_rf)


#SVM

folds = createFolds(training_set$default.payment, k = 10)
cv_svm = lapply(folds, function(x) {
  training_fold = training_set[-x, ]
  test_fold = training_set[x, ]
  classifier = svm(formula = default.payment ~ .,
                            data = training_fold,
                   type = 'C-classification',
                   kernel = 'radial')
  y_pred = predict(classifier, newdata = test_fold[-24])
  cm = table(test_fold[, 24], y_pred)
  accuracy = (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
  recall = cm[2,2]/(cm[2,2]+cm[2,1])
  precision = cm[2,2]/(cm[2,2]+cm[1,2])
  F1_score = (2*recall*precision)/(recall+precision)
  metrics_svm = c(accuracy,recall,F1_score)
  return(metrics_svm)
})
cv_svm = data.frame(cv_svm)
rownames(cv_svm) = c("Accuracy","Recall","F1_score")
cv_svm = rowMeans(cv_svm)

#decesion Tree 

folds = createFolds(training_set$default.payment, k = 10)
cv_dt = lapply(folds, function(x) {
  training_fold = training_set[-x, ]
  test_fold = training_set[x, ]
  classifier = rpart(formula = default.payment ~ .,
                     data = training_fold)
  y_pred = predict(classifier, newdata = test_fold[-24], type = 'class')
  cm = table(test_fold[, 24], y_pred)
  accuracy = (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
  recall = cm[2,2]/(cm[2,2]+cm[2,1])
  precision = cm[2,2]/(cm[2,2]+cm[1,2])
  F1_score = (2*recall*precision)/(recall+precision)
  metrics_dt = c(accuracy,recall,F1_score)
  return(metrics_dt)
})
cv_dt = data.frame(cv_dt)
rownames(cv_dt) = c("Accuracy","Recall","F1_score")
cv_dt = rowMeans(cv_dt)

#Comparing the three Models
eval_metric = rbind(cv_rf,cv_svm,cv_dt)
rownames(eval_metric)= c('Random Forest','SVM','Decision Tree')
eval_metric
