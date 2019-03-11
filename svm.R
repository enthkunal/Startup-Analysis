## Read the datafile.

svm_data <- read.csv('Companiesnew.csv')


svm_data <- svm_data[,-1]
svm_data <- svm_data[,-10]
str(svm_data)
table(svm_data$status)
#now converting data to numeric variables
levels(svm_data$category_code)
svm_data$category_code <- factor(svm_data$category_code, labels  = c(1:42))
levels(svm_data$country_code)
svm_data$country_code <- factor(svm_data$country_code, labels =  c(1:12))
str(svm_data)
svm_data$state_code <- factor(svm_data$state_code,labels  = c(1:49))
svm_data$region <- factor(svm_data$region, labels  = c(1:299))
svm_data$city <- factor(svm_data$city, labels = c(1:891))
svm_data$status <-factor(svm_data$status,labels = c(1:4))
#levels(svm_data$status)

#svm_data$status <- ifelse(svm_data$status == 'acquired', 'acquired','not acquired')
#svm_data$status <- ifelse(svm_data$status=='acquired',1,0)
#histogram(svm_data$status)

svm_data <- svm_data[,c(3,1,2,4,5,6,7,8,9)] #rearrarranging column
table(svm_data$status)
str(svm_data)
svm_data$status <- as.numeric(svm_data$status)
svm_data$status <- as.factor(svm_data$status)
svm_data$category_code <- as.numeric(svm_data$category_code)
svm_data$country_code <- as.numeric(svm_data$country_code)
svm_data$region <- as.numeric(svm_data$region)
svm_data$state_code <-as.numeric(svm_data$state_code)
svm_data$city <- as.numeric(svm_data$city)

install.packages("tabplot")
library(tabplot)
tableplot(svm_data)
str(svm_data)
library(ggplot2)
ggplot(data=svm_data,aes(x=svm_data$status,y=svm_data$funding)) + geom_point(shape=1)
## Since the measurement of data is different it needs to be normalized

install.packages("tabplot")
library(tabplot)
tableplot(svm_data)

install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
chart.Correlation(svm_datan[,2:9],col=svm_data$f)


## for running support vector machine
## for this input is soil data which has 9 variable and 1 class variable
## SVM learner require all features to be numneric
## Generally we need to normalize the data but this svm package will perform this activity.
## Now we need to divide the data into testing and training phase

set.seed(1337)
n <- nrow(svm_data)
shuffled_data <- svm_data[sample(n),]

svm_data <- shuffled_data

#library(caret)
table(svm_data$status)

svm_data <- upSample(svm_data,svm_data$status)

sample <- createDataPartition(svm_data$status, p = .70, list = FALSE) 
svm_test <- svm_data[sample, ]
svm_train <- svm_data[-sample, ]


install.packages("kernlab")
install.packages("e1071")
library(e1071)
library(kernlab)

model<-ksvm(status ~.,data=svm_train, kernel="polydot")
model
status_predict<-predict(model,svm_test)
conf=table(status_predict,svm_test$status)
conf
#library(caret)
confusionMatrix(status_predict,svm_test$status)

roc_obj2 <- roc(svm_test$status, status_predict2)
auc(roc_obj2)
plot(roc_obj2)




Hyperbolic<-ksvm(status ~ .,data=svm_train, kernel="tanhdot")
Hyperbolic
status_predict1<-predict(Hyperbolic,svm_test)
conf=table(status_predict1,svm_test$status)
conf
confusionMatrix(status_predict1,svm_test$status)

roc_obj1 <- roc(as.numeric(svm_test$status),as.numeric(status_predict1))
auc(roc_obj1)
plot(roc_obj1)


Radial<-ksvm(status ~ .,data=svm_train, kernel="rbfdot")
Radial            
status_predict2<-predict(Radial,svm_test)
table(status_predict2,svm_test$status)
confusionMatrix(status_predict2,svm_test$status)
status_predict2


library(pROC)
roc_obj <- roc(as.numeric(svm_test$status), as.numeric(status_predict2))
auc(roc_obj)
plot(roc_obj)

nn.pred = prediction(predicted_class, ann_test$status)
pref <- performance(status_predict2, "tpr", "fpr")
plot(pref,colorize=T)
abline(a=0,b=1)
library(pROC)




