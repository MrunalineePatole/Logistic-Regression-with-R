####Study of IRIS Data Set
library("dplyr")
iris
head(iris)
tail(iris)
dim(iris)
summary(iris)
colnames(iris)
colSums(is.na(iris))
View(iris)
iris$Species=as.numeric(as.factor(iris$Species))
View(iris)

###Apply sampling
iris_sample<-sample(2,nrow(iris),replace=TRUE,prob=c(0.8,0.2))
train_iris<-iris[iris_sample==1,]
test_iris<-iris[iris_sample==2,]


###Apply Logistic Regression
log_iris<-glm(Species~.,data=train_iris,family =binomial)
summary(lm_iris)
#####Prediciton on Test Data
iris_pred<-predict(log_iris,test_iris)
iris_pred
error<-test_iris$Species-iris_pred
error
mean(error)
MAPE  <- mean(abs(error*100/test_iris$Species))
MAPE
library(dplyr)
#Draw Graph
hist(error)
plot(error)

#####Prediction of train data

iris_pred_train<-predict(lm_iris,train_iris)
iris_pred_train
error_train<-train_iris$Species-iris_pred_train
error_train
MAPE<-mean(abs(error_train*100/train_iris$Species))
MAPE
hist(error_train)
plot(error_train)
plot(train_iris$Species,iris_pred_train)
a<-boxplot(iris$Species)
a
