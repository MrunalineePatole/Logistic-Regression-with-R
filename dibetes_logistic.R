db<-read.csv("E:/datasets/diabetesLogistic.csv")
View(db)
colSums(db)
head(db)
colSums(is.na(db))
###Null are not Present and not  non  numerical data
#db<-data.frame(db[,-c(9)])
#View(db)

##Apply sampling
sample_db<-sample(2,nrow(db),replace = TRUE,prob = c(0.8,.2))
train_db<-db[sample_db==1,]
test_db<-db[sample_db==2,]
dim(db)
dim(train_db)
dim(test_db)

###Apply logistic  model

model_db<-glm(Outcome~.,data = train_db,family = binomial)
summary(model_db)

####Apply prediction
pred_db<-predict(model_db,test_db,type = "response")
pred_db


###CCode to produce confuion marix
pred_db<-data.frame(pred_db,test_db$Outcome)
pred_db
library(dplyr)
pred_db<-mutate(pred_db,pred_val=ifelse(pred_db>=0.5,1,0))
pred_db
View(pred_db)
colnames(pred_db)[2]<-"Actual"
View(pred_db)
# Confusion Matrix
tab1=table(pred_db$pred_val,pred_db$Actual)
tab1
tab2=table(pred_db$Actual,pred_db$pred_val)
tab2

Acc=sum(diag(tab1))*100/sum(tab1)
Acc
Tpr<-35/(35+16)
Tpr
Fpr<-106/(106+25)
Fpr
###Code to draw ROC curve on Test Data
#install.packages("ROSE")

pred_db<-predict(model_db,test_db,type = "response")
pred_db
pred_actual <- data.frame(pred_db,test_db$Outcome)
pred_actual
colnames(pred_actual)[2]<-"Actual"
pred_actual
library(ROSE)
roc.curve(pred_actual$Actual,pred_db)


