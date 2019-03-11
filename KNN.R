#Implementing KNN to classify data based on attribdutes i.e company status.


#reading the data
knn_data <- read.csv('cdata.csv')
summary(knn_data)
library(ggplot2)


summary(knn_data)

knn_data$level <- ifelse(knn_data$level == 'High', 'High','Med-High')
str(knn_data)
knn_data$level <- ifelse(knn_data$level=='High',1,0)
knn_data$category_code <- factor(knn_data$category_code, labels  = c(1:43))
knn_data$country_code <- factor(knn_data$country_code, labels =  c(1:10))
knn_data$status <- factor(knn_data$status,labels = c(1:4))
knn_data$state_code <- factor(knn_data$state_code,labels  = c(1:50))
knn_data$region <- factor(knn_data$region, labels  = c(1:327))
knn_data$city <- factor(knn_data$city, labels = c(1:890))
mytestcompanies <- subset(knn_data, select=c(2:7,9:10))
knn_data <-knn_data[,-9]

knn_data$category_code <- as.integer(knn_data$category_code)
knn_data$country_code <- as.integer(knn_data$country_code)
knn_data$region <- as.integer(knn_data$region)
knn_data$state_code <-as.integer(knn_data$state_code)
knn_data$city <- as.integer(knn_data$city)
knn_data$status <- as.integer(knn_data$status)
str(knn_data)


library(caret)
set.seed(1550)
sample <- createDataPartition(knn_data$status, p = .75, list = FALSE) 
train <- knn_data[sample, ]
test <- knn_data[-sample, ]


library(class)

normalize <- function(x) { return ((x - min(x)) / (max(x) - min(x))) }
#normalizing the data 
my_dataset_n <- subset(knn_data, select=c(1:4,7:9)) 
my_dataset_n <- as.data.frame(lapply(my_dataset_n, normalize)) #normalise
summary(my_dataset_n)#all our numericals are normalised, our categoricals are untouched


#re make train and test note we can retain the original distribution if we choose to

sample <- createDataPartition(shuffled_data$level, p = .75, list = FALSE) 
train_n <- shuffled_data[sample, ]
test_n <- shuffled_data[-sample, ]
set.seed(1337)


n <- nrow(my_dataset_n)
shuffled_data <- my_dataset_n[sample(n),]


k1 <- 5
k2 <- 7
k3 <- 3 
k4 <- 9


knn1 <- knn(train = train, test = test, cl = train$status, k=k1)
knn2 <- knn(train = train, test = test, cl = train$status, k=k2)
knn3 <- knn(train = train, test = test, cl = train$status, k=k3)
knn4 <- knn(train = train, test = test, cl = train$status, k=k9)



(knn1Acc <- 1- mean(knn1 != test$status))
(knn2Acc <- 1- mean(knn2 != test$status))
(knn3Acc <- 1- mean(knn3 != test$status))
(knn4Acc <- 1- mean(knn4 != test$status))

library(caret)
confusionMatrix(knn1,test$status)
histogram(status,data = knn_data)
