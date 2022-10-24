

1.
data<-read.csv("creditapproval.csv", header = TRUE, sep = ",",na.strings = "")
summary(data)
sum(is.na(data))#count NA values
#install.packages("imputeMissings")
library(imputeMissings)
?impute
data<-impute(data, method = "median/mode") #impute to NA values the median or mode
#of the corresponding variable
sum(is.na(data))
names(data)
data2<-data[,-c(1,4,5,6,7,9,10,12,13,14)]
str(data2)
2.
#distribution plot
hist(data2$A2,col='red',main="Age")
hist(data2$A3,col='green',main="Debt")
hist(data2$A8,col='pink',main="Years Employed")
hist(data2$A11,col='yellow',main="Credit Score")
hist(data2$A15,col='purple',main="Income")
summary(data2)

2.
ln_A2<-scale(log(data2$A2+1)) #log transformation of the variable age + standardization 
hist(ln_A2,col='red',main="Log Age")
ln_A3<-scale(log(data2$A3+1))
hist(ln_A3,col='green',main="Log Debt")
ln_A8<-scale(log(data2$A8+1))
hist(ln_A8,col='pink',main="Log Years Employed")
ln_A11<-scale(log(data2$A11+1))
hist(ln_A11,col='yellow',main="Log Credit Score")
ln_A15<-scale(log(data2$A15+1))
hist(ln_A15,col='purple',main="Log Income")
data3<-cbind(data2,ln_A2,ln_A3,ln_A8,ln_A11,ln_A15)
str(data3)
4.
#install.packages("caTools")
library(caTools)
set.seed(1234)
?sample.split
#divide dataset into training and test set
split<-sample.split(data3$class,SplitRatio=0.65)
Train<-subset(data3,split==TRUE)
str(Train)
Test<-subset(data3,split==FALSE)
str(Test)
table(Train$class)
table(Test$class)

5.
library(MASS)
?lda
#linear discriminant analysis
model_LDA<-lda(formula=class~ln_A2+ln_A3+ln_A8+ln_A11+ln_A15,data=Train)
model_LDA
6.
#install.packages("biotools")
library(biotools)
pred_train<-predict(model_LDA,data=Train)$class
?predict

#     Predicted class
#          No Yes
#True  No  TN FP
#Class Yes FN TP

#confusion matrix
ct_train_LDA<-confusionmatrix(Train$class, pred_train)

#Accuracy: (TP+TN)/total.
sum(diag(prop.table(ct_train_LDA)))
#78%


#Misclassification Rate:(FP+FN)/total
total<-sum(ct_train_LDA)
(ct_train_LDA[1,2]+ct_train_LDA[2,1])/total
#22%


#Sensitivity, True Positive Rate:TP/(True yes)
true_yes<-ct_train_LDA[2,1]+ct_train_LDA[2,2]
ct_train_LDA[2,2]/true_yes
#66%

#False Positive Rate: FP/(True no).
true_no<-ct_train_LDA[1,1]+ct_train_LDA[1,2]
ct_train_LDA[1,2]/true_no
#12%

#Specificity, True Negative Rate: TN/(True no).
ct_train_LDA[1,1]/true_no
#88%

#Precision: TP/(predicted yes).
predicted_yes<-ct_train_LDA[1,2]+ct_train_LDA[2,2]
ct_train_LDA[2,2]/predicted_yes
#81%

#Prevalence: (True yes)/total.
true_yes/total
#44%


7.
pred_test<-predict(model_LDA, Test)$class
ct_test_LDA<-confusionmatrix(Test$class, pred_test)

#accuracy
sum(diag(prop.table(ct_test_LDA)))
#75%

#specificity True Negative Rate: TN/(True no).
true_no<-ct_test_LDA[1,1]+ct_test_LDA[1,2]
ct_test_LDA[1,1]/true_no
#85%

#sensitivity True Positive Rate:TP/(True yes)
true_yes<-ct_test_LDA[2,1]+ct_test_LDA[2,2]
ct_test_LDA[2,2]/true_yes
#62%

8.
#quadratic discriminant analysis
model_QDA<-qda(formula=class~ln_A2+ln_A3+ln_A8+ln_A11+ln_A15,data=Train)

#TRAINING SET
pred_train_QDA<-predict(model_QDA,data=Train)$class
ct_train_QDA<-confusionmatrix(Train$class, pred_train_QDA)

#accuracy
sum(diag(prop.table(ct_train_QDA)))
#78%

#specificity True Negative Rate: TN/(True no).
true_no<-ct_train_QDA[1,1]+ct_train_QDA[1,2]
ct_train_QDA[1,1]/true_no
#87%
#sensitivity True Positive Rate:TP/(True yes)
true_yes<-ct_train_QDA[2,1]+ct_train_QDA[2,2]
ct_train_QDA[2,2]/true_yes
#67%

#TEST SET
pred_test_QDA<-predict(model_QDA, Test)$class
ct_test_QDA<-confusionmatrix(Test$class, pred_test_QDA)

#accuracy
sum(diag(prop.table(ct_test_QDA)))
#77%

#specificity True Negative Rate: TN/(True no).
true_no<-ct_test_QDA[1,1]+ct_test_QDA[1,2]
ct_test_QDA[1,1]/true_no
#86%

#sensitivity True Positive Rate:TP/(True yes).
true_yes<-ct_test_QDA[2,1]+ct_test_QDA[2,2]
ct_test_QDA[2,2]/true_yes
#66%

