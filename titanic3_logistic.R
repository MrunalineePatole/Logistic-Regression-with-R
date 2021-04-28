###Logistic Regression  on Credit Risk###
##Problem Statement:
#1.Build a predictive model which can predict wether to survive or not survive
#
t3<-read.csv("E:/datasets/titanic3.csv",na.strings = "")
colnames(t3)
t3<-t3[,-c(3,8,10,12,13,14)]
View(t3)
head(t3)
colSums(is.na(t3))
dim(t3)
t3<-t3[,-c(13)]

median(t3$pclass,na.rm =TRUE)
t3$pclass[is.na(t3$pclass)]<-3
colSums(is.na(t3))

table(t3$survived)
t3$survived[is.na(t3$survived)]<-0
colSums(is.na(t3))

#mean(t3$age,na.rm=TRUE)
median(t3$age,na.rm=TRUE)
t3$age[is.na(t3$age)]<-28
colSums(is.na(t3))

table(t3$sibsp)
t3$sibsp[is.na(t3$sibsp)]<-0
colSums(is.na(t3))

median(t3$parch,na.rm = TRUE)
t3$parch[is.na(t3$parch)]<-0
colSums(is.na(t3))

mean(t3$fare,na.rm = TRUE)
t3$fare[is.na(t3$fare)]<-33.29548
colSums(is.na(t3))

mean(t3$fare,na.rm = TRUE)
t3$fare[is.na(t3$fare)]<-33.29548
colSums(is.na(t3))

table(t3$sex)
t3$sex[is.na(t3$sex)]<-"male"
colSums(is.na(t3))

table(t3$embarked)
t3$embarked[is.na(t3$embarked)]<-"S"
colSums(is.na(t3))

dim(t3)
View(t3)
colSums(is.na(t3))

#convert categorial into numerical

t3$sex<-as.numeric(as.factor(t3$sex))
t3$embarked<-as.numeric(as.factor(t3$embarked))

#Apply Sampling
library(dplyr)
sample_t3<-sample(2,nrow(t3),replace=TRUE,prob=c(0.8,0.2))
train_t3<-t3[sample_t3==1,]
test_t3<-t3[sample_t3==2,]
dim(train_t3)
dim(test_t3)
dim(t3)


#Apply logistic model & Prediction
model_t3<-glm(survived~.,data=train_t3,family=binomial)
summary(model_t3)

pred_t3<-predict(model_t3,test_t3,type="response")

### To build the confusion matrix prediction has to be  in 0 and in 1
pred_actual <- data.frame(pred_t3,test_t3$survived)
pred_actual
pred_actual <- mutate(pred_actual,pred_Val=ifelse(pred_t3>=.5,1,0))
pred_actual
View(pred_actual)
colnames(pred_actual)[2]<-"Actual"
# Confusion Matrix
tab1=table(pred_actual$pred_Val,pred_actual$Actual)
tab1
#tab2=table(pred_actual$Actual,pred_actual$pred_Val)
#tab2
Acc=sum(diag(tab1))*100/sum(tab1)
Acc
Tpr<-66/(66+31)
Tpr
Fpr<-145/(145+26)
Fpr

