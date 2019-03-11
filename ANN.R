# what are different attributes of company dataset that are corelated with each other, to identify this implementing ANN algorithm


#reading the data source

ann_data<- read.csv('Companiesnew.csv')
str(ann_data)

#removing unnecessary columns 

ann_data <- ann_data[,-17]
ann_data <- ann_data[,-18]
#Dropping companies name
ann_data <- ann_data[,-1]
#now converting data to numeric variables
levels(ann_data$category_code)
ann_data$category_code <- factor(ann_data$category_code, labels  = c(1:42))
levels(ann_data$country_code)
ann_data$country_code <- factor(ann_data$country_code, labels =  c(1:12))
str(ann_data)
ann_data$state_code <- factor(ann_data$state_code,labels  = c(1:49))
ann_data$region <- factor(ann_data$region, labels  = c(1:299))
ann_data$city <- factor(ann_data$city, labels = c(1:891))
ann_data$status <- ifelse(ann_data$status == 'acquired', 'acquired','not acquired')
ann_data$status <- ifelse(ann_data$status=='acquired',1,0)
levels(ann_data$status)
ann_data$status <- as.numeric(ann_data$status)
ann_data[,1]=ann_data[,3]

svm_test <- ann_data[,c(3,1,2,4,5,6,7,8,9)] #rearrarranging column

ann_data <-svm_test
str(ann_data)
ann_data$category_code <- as.numeric(ann_data$category_code)
ann_data$country_code <- as.numeric(ann_data$country_code)
ann_data$region <- as.numeric(ann_data$region)
ann_data$state_code <-as.numeric(ann_data$state_code)
ann_data$city <- as.numeric(ann_data$city)
ann_data$status <- as.numeric(ann_data$status)
install.packages("tabplot")
library(tabplot)
tableplot(ann_data)
str(ann_data)

install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
chart.Correlation(ann_data[,c(2:9)],col=ann_data$status) # Here we can find correlation between differernt attributes of company data

pairs(ann_data[,c(2:9)])

# now data normazization needed for training and testing neural nets

normalize <- function(x)
{
  return ( (x-min(x) ) / ( max(x)-min(x) ) )
}


ann_data_n<-as.data.frame(lapply(ann_data,normalize))
str(ann_data_n)

## Now we need to divide the data into testing and training phase
set.seed(17108225)
test_index<-sample(nrow(ann_data),200)
ann_test<- ann_data[test_index, ]
ann_train<- ann_data[-test_index, ]

install.packages("neuralnet")
library("neuralnet")

ann_model<-neuralnet(status ~ category_code +  funding_total_usd + country_code + state_code + region + city + funding_rounds + founded_year , data = ann_train, hidden=1,stepmax =1e6)

plot(ann_model)

model_results<-compute(ann_model,ann_test[,c("category_code","funding_total_usd","country_code","state_code", "region" ,"city" ,"funding_rounds", "founded_year")])
str(model_results)
predicted_class<-model_results$net.result
cor(predicted_class,ann_test$status)
detach(package:neuralnet,unload = T)

library(ROCR)

nn.pred = prediction(predicted_class, ann_test$status)
pref <- performance(nn.pred, "tpr", "fpr")
plot(pref,colorize=T)
abline(a=0,b=1)
library(pROC)

## This will give the percentage of corret classified data in ANN.





