getwd()
setwd("E:/")
Data_In<-read.csv("Retail_Lending.csv",header = T)
str(mydata)
library(plotROC)
library(pROC)
library(MASS)



##Reading and summarising the data

Data_In<-read.csv('Retail_Lending.csv', header = TRUE)
summary(Data_In)
str(Data_In)



##changing formats

# Date format Change

Data_In$end_effective_date<-as.Date(strptime(Data_In$end_effective_date,'%d-%b-%y'))

Data_In$maturity_date<-as.Date(strptime(Data_In$maturity_date,'%d-%b-%y'))

Data_In$NEXT_DUE_DATE<-as.Date(strptime(Data_In$NEXT_DUE_DATE,'%d-%b-%y'))

Data_In$pmt_next_due_date<-as.Date(strptime(Data_In$pmt_next_due_date,'%d-%b-%y'))

#Re-Converting factors

Data_In$hpi_uCLTV_since_orig2<-as.numeric(Data_In$hpi_uCLTV_since_orig2)

Data_In$Upd.Shaw.Fico<-as.numeric(Data_In$Upd.Shaw.Fico)

Data_In$DTI<-as.numeric(Data_In$DTI)

Data_In$tr_1_to_2<-as.factor(Data_In$tr_1_to_2)

#Splitting the data into train and test datasets
Data_In<-Data_In[-1,]
smp_size <- floor(0.50 * nrow(Data_In))
set.seed(123)
train_ind <- sample(seq_len(nrow(Data_In)), size = smp_size)
train <- Data_In[train_ind, ]
test <- Data_In[-train_ind, ]



#Creating the model for WHOLE DATASET 

fit3<-glm(tr_1_to_2~end_effective_date+Curr.Balance+maturity_date+hpi_uCLTV_since_orig2+PAYMENT_AMOUNT+NEXT_DUE_DATE+OPEN_OR_CLOSED+Upd.Shaw.Fico+no_of_pmts_rcd+num_of_missed_pmts+Delinquency+months_to_maturity+mly_unemp+DTI+hpi_life_change, family = 'binomial', data = Data_In)

#confidence interval
confint(fit3)

#predicting fit
preds3<-predict.glm(fit3)

#roc validation
roc3<-roc(Data_In$tr_1_to_2~preds3)
summary(fit3)
plot.roc(roc3)


#creating model for train dataset
fit1<-glm(tr_1_to_2~end_effective_date+Curr.Balance+maturity_date+hpi_uCLTV_since_orig2+PAYMENT_AMOUNT+NEXT_DUE_DATE+OPEN_OR_CLOSED+Upd.Shaw.Fico+no_of_pmts_rcd+num_of_missed_pmts+Delinquency+months_to_maturity+mly_unemp+DTI+hpi_life_change, family = 'binomial', data = train)

#confidence interval
confint(fit1)

#predicting fit
preds2<-predict.glm(fit1)

#roc validation
roc1<-roc(train$tr_1_to_2~preds2)
summary(fit1)
plot.roc(roc1)



#creating model foe test dataset
fit2<-glm(tr_1_to_2~end_effective_date+Curr.Balance+maturity_date+hpi_uCLTV_since_orig2+PAYMENT_AMOUNT+NEXT_DUE_DATE+OPEN_OR_CLOSED+Upd.Shaw.Fico+no_of_pmts_rcd+num_of_missed_pmts+Delinquency+months_to_maturity+mly_unemp+DTI+hpi_life_change, family = 'binomial', data = test)

#confidence interval
confint(fit2)

#predicting fit
preds1<-predict.glm(fit2)

#roc validation
roc2<-roc(test$tr_1_to_2~preds1)
summary(fit2)
summary(fit2)
plot.roc(roc2)


#Linear Discriminant Analysis
library(MASS)
ldafit<-lda(tr_1_to_2~end_effective_date+Curr.Balance+maturity_date+hpi_uCLTV_since_orig2+PAYMENT_AMOUNT+NEXT_DUE_DATE+OPEN_OR_CLOSED+Upd.Shaw.Fico+no_of_pmts_rcd+num_of_missed_pmts+Delinquency+months_to_maturity+mly_unemp+DTI+hpi_life_change, data = train)
ldafit
lda.pred<-predict(ldafit,data=test)
ldaclass<-lda.pred$class
table(ldaclass,test$tr_1_to_2)
table(ldaclass,train$tr_1_to_2)
