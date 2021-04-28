######Network_Intrusion_Train Data set Logistic Model
nt<-read.csv("E:/datasets/Network_Intrusion_Train.csv")
head(nt)
View(nt)
colSums(is.na(nt))
nt$protocol_type<-as.numeric(as.factor(nt$protocol_type))
nt$service<-as.numeric(as.factor(nt$service))
nt$flag<-as.numeric(as.factor(nt$flag))

nt<-mutate(nt,class = ifelse(class=="anomaly",1,0))
colSums(is.na(nt))
View(nt)


sample_nt<-sample(2,nrow(nt),replace = TRUE,prob = c(0.8,0.2))
train_nt<-nt[sample_nt==1,]
test_nt<-nt[sample_nt==2,]
dim(nt)
dim(train_nt)
dim(test_nt)

library(dplyr)
model_nt<-glm(class~.,data = train_nt,family = binomial)
summary(model_nt)
pred_nt<-predict(model_nt,test_nt,type = "response")
pred_nt

pred_nt<-data.frame(pred_nt,test_nt$class)
pred_nt
library(dplyr)
pred_nt<-mutate(pred_nt,pred_val=ifelse(pred_nt>=0.5,1,0))
pred_nt
View(pred_nt)
colnames(pred_nt)[2]<-"Actual"
View(pred_nt)
# Confusion Matrix
tab1=table(pred_nt$pred_val,pred_nt$Actual)
tab1
tab2=table(pred_nt$Actual,pred_nt$pred_val)
tab2

Acc=sum(diag(tab1))*100/sum(tab1)
Acc
Tpr<-2151/(2151+144)
Tpr
Fpr<-98/(2542+98)
Fpr
###Code to draw ROC curve on Test Data
#install.packages("ROSE")

pred_nt<-predict(model_nt,test_nt,type = "response")
pred_nt
pred_actual <- data.frame(pred_nt,test_nt$class)
pred_actual
colnames(pred_actual)[2]<-"Actual"
pred_actual
library(ROSE)
roc.curve(pred_actual$Actual,pred_nt)

