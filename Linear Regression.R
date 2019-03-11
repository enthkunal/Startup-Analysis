#Reading the data 

lr_data <- read.csv('/Users/kunal/Documents/ADM Project/crunchbase-investments.csv')
str(lr_data)
cor(lr_data_n$funded_year,lr_data_n$raised_amount_usd)
#now from this data frame we need two column namely funded_year and raised amount USD 

lr_data_n <- lr_data[,c(19,20)]
str(lr_data_n)

lr_data$funding_round_type <-
  str(lr_data)

# To evaluate what will be the future prediction we need to add all amount raised yearly


lr_data_n <- aggregate(raised_amount_usd ~ funded_year, data = lr_data_n,sum, na.rm=TRUE)

#now we have all the raised amount during every avaible year in the dataset

#bulding linear model

lmm_model <- lm(raised_amount_usd ~ funded_year,data = lr_data_n)
summary(lmm_model)

plot(lr_data_n$funded_year,lr_data_n$raised_amount_usd)

plot(lr_data$funded_year,lr_data$raised_amount_usd)
