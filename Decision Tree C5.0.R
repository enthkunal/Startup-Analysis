getwd()

my_companies <- read.csv("cdata.csv", stringsAsFactors = T)
my_companies$level <- level
#my_companies$level <- cut(my_companies$funding_total_usd, breaks = c(5500000,8816918,17633837,2600000000),labels = c("Low","Med","High"))
str(my_companies)
# encoding the vaiables:
table(my_companies$level)
na.omit(my_companies)
summary(my_companies)
str(my_companies)
table(my_companies$status)
set.seed(1337) 
#Checking for missing values
sapply(my_companies,function(x) sum(is.na(x)))

#my_companies$status <- ifelse(my_companies$status == 'acquired', ) #conveting acquired as 0 and else other such as operating , IPO and closed to 0
levels(my_companies$status)
my_companies$category_code <- factor(my_companies$category_code, labels  = c(1:43))
my_companies$country_code <- factor(my_companies$country_code, labels =  c(1:10))
my_companies$state_code <- factor(my_companies$state_code,labels  = c(1:50))
my_companies$region <- factor(my_companies$region, labels  = c(1:327))
my_companies$city <- factor(my_companies$city, labels = c(1:890))
my_companies$status <-factor(my_companies$status,labels = c(1:4))

table(my_companies$level) # checking for class imbalanced
my_companies <- my_companies[,-1]

write.csv(file="cdata.csv", my_companies)



#str(my_companies) 
my_companies$category_code <- factor(my_companies$category_code)
my_companies$country_code <- factor(my_companies$country_code)
my_companies$state_code <- factor(my_companies$state_code)
my_companies$region <- factor(my_companies$region)
my_companies$city <- factor(my_companies$city)
my_companies$status <- factor(my_companies$status)

my_companies$status <- factor(my_companies$status,levels = c(1,0),labels = c('acquired','not acquired'))

att_data <-my_companies
# Load the rpart, rattle, rpart.plot and RColorBrewer package
library(rpart)
library(rpart.plot)
library(RColorBrewer)
att_data$status <-factor(att_data$status,labels  = c(0,1,2,3),levels =c("acquired","closed","ipo","operating"))
datt_data$category_code <- factor(att_data$category_code)
levels(att_data$status)
att_data$country_code <- factor(att_data$country_code)
att_data$state_code <- factor(att_data$state_code)
att_data$region <- factor(att_data$region)

att_data$funding_rounds <- factor(att_data$funding_rounds)
str(att_data)
my_companies <-my_companies[,-1]
att_data <- my_companies
#Shuffle the dataset, call the result shuffled

set.seed(1337)
n <- nrow(att_data)
shuffled_data <- att_data[sample(n),]

att_data <- shuffled_data

# Spliting data into training and testing dataset 

sample <- createDataPartition(att_data$level, p = .70, list = FALSE) 
train_data <- att_data[sample, ]
test_data <- att_data[-sample, ]



library(C50)

cFifty <- C5.0(status ~., data=train_data, trials=100)
cFiftyPrediction <- predict(cFifty, newdata = test_data$status)
(cFiftyAccuracy <- 1- mean(cFiftyPrediction != test_data$status))
plot(cFifty)
conf <- table(cFiftyPrediction,test_data$status)
conf
acc <- sum(diag(conf)/sum(conf))
acc

library(caret)
confusionMatrix(cFiftyPrediction, test_data$level)
plot(cFifty)







