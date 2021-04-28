###Logistic Regression  on Credit Risk###
##Problem Statement:
#1.Build a predictive model which can predict wether to approve or reject loan
#2.Run a marketing /sale campaign to identify(target) the good customer
cr<-read.csv("E:/datasets/CreditRisk.csv",na.strings = "")
cr<-cr[,c(2:13)]
View(cr)
head(cr)
colSums(is.na(cr))

table(cr$Gender)
cr$Gender[is.na(cr$Gender)]<-"Male"
colSums(is.na(cr))

table(cr$Married)
cr$Married[is.na(cr$Married)]<-"Yes"
colSums(is.na(cr))        

table(cr$Dependents)
cr$Dependents[is.na(cr$Dependents)]<- 0
colSums(is.na(cr))

table(cr$Self_Employed)
cr$Self_Employed[is.na(cr$Self_Employed)]<-"Yes"
colSums(is.na(cr))

mean(cr$LoanAmount,na.rm = TRUE)
cr$LoanAmount[is.na(cr$LoanAmount)]<-142.5115
colSums(is.na(cr))

mean(cr$Loan_Amount_Term,na.rm = TRUE)
cr$Loan_Amount_Term[is.na(cr$Loan_Amount_Term)]<-342.2019
colSums(is.na(cr))

table(cr$Credit_History)
cr$Credit_History[is.na(cr$Credit_History)]<-0
colSums(is.na(cr))

####Converting categorial data into numeric
cr$Gender <-as.numeric(as.factor(cr$Gender))
cr$Married <-as.numeric(as.factor(cr$Married))
cr$Education <-as.numeric(as.factor(cr$Education))
cr$Self_Employed<-as.numeric(as.factor(cr$Self_Employed))
cr$Property_Area <-as.numeric(as.factor(cr$Property_Area))

library(dplyr)
# Converting loan status into 0 & 1
cr<-mutate(cr,Loan_Status = ifelse(Loan_Status=="Y",1,0))
colSums(is.na(cr))
View(cr)

#Apply Sampling
sample_cr<- sample(2,nrow(cr),replace = TRUE,prob = c(0.8,0.2))
cr_train<-cr[sample_cr==1,]
cr_test<-cr[sample_cr==2,]
dim(cr_train)
dim(cr_test)
dim(cr)
####Apply Logistic Model
##Family indicate which probility we have to use .here we r using binomial
model_cr<-glm(Loan_Status~.,data=cr_train,family = binomial)
summary(model_cr)

##IN output AIC- Akaike information criterian value
### BIC -Bayesian information criterian
##type = " response" gives prediction probability
pred_cr<-predict(model_cr,cr_test,type = "response")
pred_cr

### To build the confusion matrix prediction has to be  in 0 and in 1
pred_actual <- data.frame(pred_cr,cr_test$Loan_Status)
pred_actual <- mutate(pred_actual,pred_Val=ifelse(pred_cr>=.5,1,0))
View(pred_actual)
colnames(pred_actual)[2]<-"Actual"
# Confusion Matrix
tab1=table(pred_actual$pred_Val,pred_actual$Actual)
tab1
tab2=table(pred_actual$Actual,pred_actual$pred_Val)
tab2
Acc=sum(diag(tab1))*100/sum(tab1)
Acc
Tpr<-146/(146+16)
Fpr<-26/(26+15)
####Target the high probability customer
##Do prediction on full data
pred_full<-predict(model_cr,cr,type = "response")
pred_full
customer_prob<-data.frame(cr$Loan_ID,pred_full)
View(customer_prob)
###Sort it so highest probability on Top
Sorted_record<-arrange(customer_prob,desc(pred_full))
View(Sorted_record)

###Code to draw ROC curve on Test Data
install.packages("ROSE")

pred_cr<-predict(model_cr,cr_test,type = "response")
pred_cr
pred_actual <- data.frame(pred_cr,cr_test$Loan_Status)
pred_actual
colnames(pred_actual)[2]<-"Actual"
pred_actual
library(ROSE)
roc.curve(pred_actual$Actual,pred_cr)


length(pred_test)
