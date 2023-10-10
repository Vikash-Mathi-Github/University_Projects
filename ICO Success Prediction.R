#-----Filter distributed percentage & price....dsi<100% 
#1. Installing packages and library
rm(list=ls())

install.packages("tidyverse")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("mice")
install.packages("naniar")
install.packages("DescTools")
install.packages("lubridate")
install.packages("Amelia")
install.packages("corrplot")
install.packages("stringr")
install.packages("countrycode")
install.packages("VIM")
install.packages("tidytext")
install.packages("syuzhet")
install.packages("textdata")
install.packages("textblob")
install.packages("topicmodels")
install.packages("caret")
install.packages("fastDummies")
install.packages("corrgram")
install.packages("class")


library(dplyr)
library(tidyverse)
library(ggplot2)
library(mice)
library(naniar)
library(DescTools)
library(lubridate)
library(Amelia)
library(corrplot)
library(stringr)
library(countrycode)
library(VIM)
library(tidytext)
library(syuzhet)
library(textdata)
library(textblob)
library(topicmodels)
library(caret)
library(fastDummies)
library(corrgram)
library(class)

#2. Exploratory Data Analysis (EDA)

#Read the data
raw_data <- read.csv("C:/Users/Vikash/Desktop/Machine Learning/Assignment/LUBS5990M_courseworkData_202223.csv", 
                     stringsAsFactors = FALSE, encoding = 'UTF-8')

#Identifying data structure and format
str(raw_data)

#Summary of the data
summary(raw_data)

#Success field summary
frequency_table <- table(raw_data$success)
# Calculate the percentages
percentage_table <- prop.table(frequency_table) * 100

# Combine the frequency and percentage tables
summary_table <- cbind(frequency_table, percentage_table)

#Brand Slogan field
length(unique(raw_data$brandSlogan))
#Identifying missing values
sum(is.na(raw_data))

#Summary of Rating
summary(raw_data$rating)

#Column wise missing data summary
missing_values <-sapply(raw_data,function(x) sum(is.na(x) | x==" " |x=="" ))

complete.cases(raw_data)
!complete.cases(raw_data)

#Visual representation of missing values
png("missing_data_plot.png", width = 1200, height = 800, units = "px")
print(summary(aggr(raw_data,prop=FALSE, type="both",numbers=TRUE)))
dev.off()

#Summarising missing values
summary_missing <- data.frame(
  Variable= names(raw_data),
  Missing_Values = missing_values,
  Percent_missing = round((missing_values/ nrow(raw_data))*100,2),
  Type=sapply(raw_data, class)
)

#-------------------------------------------------------------------
#Outlier Treatment - Treating missing values in "priceUSD" field

#Box plot to view outliers before imputing NA with mean
boxplot(raw_data$priceUSD)$out
par(mar=c(4, 4, 2, 2))
hist(raw_data$priceUSD)
range(raw_data$priceUSD,na.rm = TRUE)
summary(raw_data$priceUSD)

#Missing value treatment - Replacing max value of price 
#column with previous max value as there 
#is only one outlier (39384)

# calculate the maximum value
max_value <- max(raw_data$priceUSD,na.rm = TRUE)

# create a logical vector indicating max value outliers
outliers <- ifelse(raw_data$priceUSD == max_value, TRUE, FALSE)

# impute max value outliers with previous max value
raw_data$priceUSD <- ifelse(outliers, lag(raw_data$priceUSD)[outliers], raw_data$priceUSD)
summary(raw_data)

# print the result
max(raw_data$priceUSD,na.rm = TRUE)

#Replace NA with mean price value for respective month and year

#Creating Month and Year columns for data cleaning
raw_data$year_column <- substr(raw_data$startDate, nchar(raw_data$startDate)-3, nchar(raw_data$startDate))

raw_data$month_column <- substr(raw_data$startDate, nchar(raw_data$startDate)-6, nchar(raw_data$startDate)-5)
head(raw_data, n = 10)
unique(raw_data$year)
str(raw_data)

#Group by Year and Month for imputing missing price values at 
#a month and year level

summary(raw_data)

raw_data <- raw_data %>% 
  group_by(year_column,month_column) %>% 
  mutate(mean_price = mean(priceUSD, na.rm = TRUE))

#Replacing NA in Price column with mean values for the respective year and month

raw_data$priceUSD[is.na(raw_data$priceUSD)] <- raw_data$mean_price[is.na(raw_data$priceUSD)]

sum(is.na(raw_data$priceUSD))
summary(raw_data)

#-------------------------------------------------------------------
#Missing value - treatment in Country column 

# Use the countrycode() function to convert country names to continent codes

# Treating misspelled words in country column
# Words to replace
words_to_replace_ctry <- c("México")

# use gsub function to replace the words

raw_data$countryRegion <- gsub(paste(words_to_replace_ctry, collapse = "|"), "Mexico",raw_data$countryRegion,ignore.case = FALSE)
unique(raw_data$countryRegion)

#Adding continent values to respective countries
raw_data$continent <- countrycode(raw_data$countryRegion, "country.name", "continent")
unique(raw_data$continent)

#Replacing NA values in continent column
raw_data$continent <- ifelse(is.na(raw_data$continent), "Unknown", raw_data$continent)
sum(is.na(raw_data$continent))
unique(raw_data$continent)

#-------------------------------------------------------------------
#Missing Value - Treatment for "teamSize" column

summary(raw_data$teamSize)
#Identifying which column is highly corelated to team size to
#figure out which columns to fit the regression line
par(mar = c(5,5,2,2))
corrplot(cor(raw_data[,c("hasVideo","rating","priceUSD","teamSize","hasGithub","hasReddit","coinNum","minInvestment","distributedPercentage"
)]))

# Use the "mice" package to impute missing values with 

missdata <-raw_data
missdata$missing <-as.numeric(!complete.cases(raw_data)) 
corrgram(missdata)
filter(missdata,missing==1)


imi <-mice(raw_data, m = 5, maxit = 10,method = "cart")
raw_data <-complete(imi)
summary(raw_data)
summary(raw_data)

# Plot the imputed values against the observed values
imputed_values <- complete(raw_data, 1)
ggplot(raw_data, aes(x = my_variable, y = my_other_variable)) + 
  geom_point(color = "blue", alpha = 0.5) + 
  geom_point(data = raw_data, color = "red", alpha = 0.5) + 
  labs(title = "Imputed vs Observed Values")

#-------------------------------------------------------------------
#Missing Value & Data Correction - Treatment for "platform" column

(unique(raw_data$platform))

#Data correction in platform column for Ethereum

# specify the list of words to replace
words_to_replace <- c("Ethereum ","ETH"," Ethereum","Ethererum","Ethereum     ",
                      "Ethereum    ","Ethereum   ","Ethereum  ","Ethereum, Waves",
                      "Etherum","ERC20")

# use gsub function to replace the words

raw_data$platform <- gsub(paste(words_to_replace, collapse = "|"), "Ethereum", raw_data$platform, ignore.case = FALSE)
unique(raw_data$platform)


#Data correction in platform column for BIT Coin

# specify the list of words to replace
words_to_replace_btc <- c("Bitcoin","BTC")

# use gsub function to replace the words

raw_data$platform <- gsub(paste(words_to_replace_btc, collapse = "|"), "Bitcoin", raw_data$platform, ignore.case = FALSE)
unique(raw_data$platform)


#Data correction in platform column for Separate Blockchain Coin

# specify the list of words to replace
words_to_replace_sb <- c("Separate blockchain","Separate Blockchain ",
                         "Separate Blockchain  ")

# use gsub function to replace the words

raw_data$platform <- gsub(paste(words_to_replace_sb, collapse = "|"), "Separate Blockchain", raw_data$platform, ignore.case = FALSE)
unique(raw_data$platform)

#Data correction in platform column for POW/POS Coin

# specify the list of words to replace
words_to_replace_pos <- c("POS + POW","POS + POW","POS + POW",
                          "POS,POW","PoW/PoS","PoW/PoS","pow/pos","POS/POW","POS + POW" )

# use gsub function to replace the words
raw_data$platform<- gsub("POS[ ,/\\+]+POW|pow/pos", "POS/POW", raw_data$platform, ignore.case = TRUE)
unique(raw_data$platform)

#Data correction in platform column for Tron

# specify the list of words to replace
words_to_replace_tr <- c("Tron   ","Tron","Tron")

# use gsub function to replace the words

raw_data$platform <- gsub(paste(words_to_replace_tr, collapse = "|"), "Tron", raw_data$platform, ignore.case = FALSE)
unique(raw_data$platform)

#Replacing NA values in continent column
raw_data<- raw_data %>% replace_with_na(replace = list(platform= "unknown"))
sum(is.na(raw_data$platform))
raw_data$platform <- ifelse(raw_data$platform == " ", "unknown", raw_data$platform)
sum(is.na(raw_data$platform))
unique(raw_data$platform)

#-------------------------------------------------------------------
#Coin Num column
summary(raw_data$coinNum)

#-------------------------------------------------------------------
#Data correction for "distributedPercentage" as it 
#can not be greater than 100%

#cap values at 100%
print(raw_data$distributedPercentage>1)
#raw_data$distributedPercentage <- ifelse(raw_data$distributedPercentage > 1, 1, raw_data$distributedPercentage)
raw_data <- filter(raw_data, distributedPercentage<1)
summary(raw_data$distributedPercentage)
summary(raw_data)

#-------------------------------------------------------------------
#Calculating duration in days

#Converting Start and End date from string to date format
raw_data$endDate<-as.Date(raw_data$endDate, format ="%d/%m/%Y") 
raw_data$startDate<-as.Date(raw_data$startDate, format ="%d/%m/%Y") 

#Calculating the difference and fixing negative values with mod function
raw_data$days <- as.numeric(abs(raw_data$endDate-raw_data$startDate))
unique(raw_data$days)


#Detecting outliers
boxplot(raw_data$days)$out

#Display top 10 max values
head(sort(raw_data$days, decreasing = TRUE), n = 10)
nrow(raw_data)
#Filter
raw_data_flt <- filter(raw_data,raw_data$days > 0 & raw_data$days < 3722)
nrow(raw_data_flt)
summary(raw_data)
str(raw_data)

#-------------------------------------------------------------------
#Creating a column for total amount distributed
#Coins offered column
raw_data$total_money_offered <-raw_data$priceUSD*raw_data$coinNum*raw_data$distributedPercentage

#Filtering amount > 1000
raw_data <- filter(raw_data,raw_data$total_money_offered >=1000)

summary(raw_data$total_money_offered)



#-------------------------------------------------------------------

#Converting continent columns to Binary

#continent variable
raw_data<-raw_data%>%
  mutate(Continent_Asia=ifelse(continent=="Asia",1,0),
         Continent_Europe=ifelse(continent=="Europe",1,0),
         Continent_Americas=ifelse(continent=="Americas",1,0),
         Continent_Oceania=ifelse(continent=="Oceania",1,0),
         Continent_Africa=ifelse(continent=="Africa",1,0),
         Continent_Others=ifelse((continent!='Asia'& continent!='Europe'
                                  & continent!='Oceania'& continent!='Americas'
                                  & continent!='Africa'),1,0))

#-------------------------------------------------------------------

#Identifying major proportion of success rate for coin platform 
#raw_data <- raw_data %>% 
 # group_by(platform) %>% 
  #(success_rate = (count(raw_data$success=="Y",name = "count_if")/count(raw_data$success))*100)

print(count(raw_data))
print(sum(raw_data$success == 'N'))
         mean(priceUSD, na.rm = TRUE)



#-------------------------------------------------------------------

#Converting coin platform columns to Binary

#coin variable
raw_data<-raw_data%>%
  mutate(Coin_Ethereum=ifelse(platform=="Ethereum",1,0),
         Coin_Waves=ifelse(platform=="Waves",1,0),
         Coin_Stellar=ifelse(platform=="Stellar",1,0),
         Coin_Separate_Blockchain=ifelse(platform=="Separate Blockchain",1,0),
         Coin_NEO=ifelse(platform=="NEO",1,0),
         Coin_Bitcoin=ifelse(platform=="Bitcoin",1,0),
         Coin_Others=ifelse((platform!='Ethereum'& platform!='Waves'
                                  & platform!='Stellar'& platform!='Separate Blockchain'
                                  & platform!='NEO'& platform!='Bitcoin'),1,0))

summary(raw_data)

#-------------------------------------------------------------------

#Calculate the overall sentiment score for each text by summing up the sentiment scores:

raw_data$slogan_score<-(get_sentiment(raw_data[,"brandSlogan"]))
summary(raw_data$slogan_score)                  
?get_sentiment

#-------------------------------------------------------------------

#Removing duplicates from Brand Slogan column

raw_data <- raw_data[!duplicated(raw_data$brandSlogan), ]
count(raw_data)
summary(raw_data)
#duplicate_records <- raw_data[duplicated(raw_data$brandSlogan), ]
#duplicate_records <- df[duplicated(raw_data$brandSlogan) | duplicated(raw_data$brandSlogan, fromLast = TRUE), ]
#duplicate_counts <- table(raw_data$brandSlogan)
#duplicate_records <- subset(raw_data, raw_data$brandSlogan %in% names(duplicate_counts[duplicate_counts > 1]))
#print(duplicate_records)
#-------------------------------------------------------------------

#Converting success as binary

#raw_data$success <- ifelse(raw_data$success == "Y", 1, 0)
raw_data$success <- factor(raw_data$success,levels = c("Y", "N"), 
                    labels = c("YES", "NO"))
summary(raw_data)
cleaned_data<-raw_data

#Writing the final dataset after cleaning to present visuals
write.table(raw_data, file = "ML Coursework EDA Data.csv", 
           row.names=F,sep = ",")
#-------------------------------------------------------------------
#Removing unnecessary columns for further processing 
cleaned_data <- cleaned_data[, -c(1,3,7,8,9,13,17,18,19,20)]
str(cleaned_data)

#Writing the final dataset after cleaning to summarise
write.table(cleaned_data, file = "ML Final Dataset.csv", 
            row.names=F,sep = ",")
str(cleaned_data)
#-------------------------------------------------------------------

#Importance of each features
set.seed(7)
control <- trainControl(method="repeatedcv", number=10, repeats=3)
model <- train(success~., data=cleaned_data, method="lvq", preProcess="scale", trControl=control)
importance <- varImp(model, scale=FALSE)
print(importance)
plot(importance)

#Correlation
dataC<-cleaned_data
dataC <- dummy_cols(cleaned_data, select_columns = 'success')
dataC<-dataC[-1]
corrgram<-data.frame(corrgram(dataC))
cor(dataC)
cor<-data.frame(cor(dataC))



#-------------------------------------------------------------------
#Algorithm Section
##K-NN

data_knn<- read.csv("ML Final Dataset.csv", stringsAsFactors = TRUE)
str(data_knn)


table(data_knn$success) 
round(prop.table(table(data_knn$success)) * 100, digits = 1) 


#Normalization 
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

data_knn_norm <- as.data.frame(lapply(data_knn[2:26], normalize))
data_knn_norm$success<-data_knn$success

K = 10 
set.seed(0599)
folds <- createFolds(data_knn_norm$success, k = K)
str(folds)  

accuracy_list_knn_k3<-as.numeric() 
sensitivity_list_knn_k3<-as.numeric() 
specificity_list_knn_k3<-as.numeric()
precision_list_knn_k3<-as.numeric()
recall_list_knn_k3<-as.numeric()
f1_list_knn_k3<-as.numeric()
appendedDf_knn_k3<-data.frame()

for(i in 1:K){ 
  knn_test <- data_knn_norm[folds[[i]],] 
  knn_train <- data_knn_norm[-folds[[i]],]
  
  set.seed(2311)
  knn_pred_k3<- knn(train = knn_train[,-26], test = knn_test[,-26], 
                    cl = knn_train$success, k=29) 
  
  cm_knn_k3<- confusionMatrix(knn_pred_k3, knn_test$success, positive = "YES") 
  
  accuracy_knn_k3 <- cm_knn_k3$overall['Accuracy']
  sensitivity_knn_k3 <- cm_knn_k3$byClass['Sensitivity'] 
  specificity_knn_k3 <- cm_knn_k3$byClass['Specificity']
  precision_knn_k3 <- cm_knn_k3$byClass['Precision'] 
  recall_knn_k3 <- cm_knn_k3$byClass['Recall']
  f1_knn_k3 <- cm_knn_k3$byClass['F1']
  
  accuracy_list_knn_k3<- append(accuracy_list_knn_k3,accuracy_knn_k3)
  sensitivity_list_knn_k3<- append(sensitivity_list_knn_k3,sensitivity_knn_k3)
  specificity_list_knn_k3<- append(specificity_list_knn_k3,specificity_knn_k3)
  precision_list_knn_k3<- append(precision_list_knn_k3,precision_knn_k3)
  recall_list_knn_k3<- append(recall_list_knn_k3,recall_knn_k3)
  f1_list_knn_k3<- append(f1_list_knn_k3,f1_knn_k3)
  
  predict_class_knn_prob_k3 <- predict(caret::knn3(knn_train[,-26], knn_train$success, k = 29), knn_test[,-26])
  
  k3 <- data.frame(actual_type = knn_test[,26],
                   predict_type = knn_pred_k3,
                   prob_Yes = round(predict_class_knn_prob_k3 [ , 2], 5),
                   prob_No = round(predict_class_knn_prob_k3 [ , 1], 5))
  
  appendedDf_knn_k3 <- rbind(appendedDf_knn_k3, k3)
  
}
install.packages("gmodels")

library(gmodels)
CrossTable(knn_pred_k3, knn_test$success, 
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('predicted default', 'actual default' ))


#-------------------------------------------------------------------
install.packages("caret")
library(caret)
install.packages("ROCR")
library(ROCR)
?prediction()

##AUC and ROC curve
pred_object_knn_k3 <- prediction(appendedDf_knn_k3$prob_Yes, appendedDf_knn_k3$actual_type) 
roc_knn_k3<- performance(pred_object_knn_k3, measure = "tpr", x.measure = "fpr")
plot(roc_knn_k3, main = "KNN ROC curve for ICO", col = "blue", lwd = 2) 
abline(a = 0, b = 1, lwd = 2, lty = 2,col = "red") 

auc_object_knn_k3 <- performance(pred_object_knn_k3, measure = "auc")
auc_knn_k3 <- auc_object_knn_k3@y.values[[1]]
auc_knn_k3 

#Measures accuracy
accuracy_list_knn_k3 
accuracy_average_knn_k3 <- mean(accuracy_list_knn_k3)
accuracy_average_knn_k3 
sd(accuracy_list_knn_k3) 

sensitivity_list_knn_k3
sensitivity_average_knn_k3 <- mean(sensitivity_list_knn_k3)
sensitivity_average_knn_k3 
sd(sensitivity_list_knn_k3) 

specificity_list_knn_k3
specificity_average_knn_k3 <- mean(specificity_list_knn_k3)
specificity_average_knn_k3 
sd(specificity_list_knn_k3) 

precision_list_knn_k3
precision_average_knn_k3 <- mean(precision_list_knn_k3)
precision_average_knn_k3 
sd(precision_list_knn_k3) 

recall_list_knn_k3
recall_average_knn_k3 <- mean(recall_list_knn_k3)
recall_average_knn_k3 
sd(recall_list_knn_k3) 

f1_list_knn_k3
f1_average_knn_k3 <- mean(f1_list_knn_k3)
f1_average_knn_k3 
sd(f1_list_knn_k3)


#-------------------------------------------------------------------

##Decision Tree 

data_DT1<- read.csv("ML Final Dataset.csv", stringsAsFactors = TRUE)


data_DT <- as.data.frame(lapply(data_DT1[2:26], normalize))
data_DT$success<-data_DT1$success

#Decision tree k fold cross validation
#____________________________________________________

K = 10 
set.seed(0599)
folds <- createFolds(data_DT$success, k = K)
str(folds)  

accuracy_list_DT<-as.numeric() 
sensitivity_list_DT<-as.numeric() 
specificity_list_DT<-as.numeric()
precision_list_DT<-as.numeric()
recall_list_DT<-as.numeric()
f1_list_DT<-as.numeric()
appendedDf_DT<-data.frame()

library(C50)
install.packages("C50")
library(tidyverse)

for(i in 1:K){ 
  DT_test <- data_DT[folds[[i]],] 
  DT_train <- data_DT[-folds[[i]],]
  
  set.seed(2311)
  model_DT <- C5.0(select(DT_train, -success), DT_train$success) 
  
  predict_class_DT <- predict(model_DT, DT_test, type='class')
  cm_DT <- confusionMatrix(predict_class_DT, DT_test$success, positive = "YES") 
  
  accuracy_DT <- cm_DT$overall['Accuracy']
  sensitivity_DT <- cm_DT$byClass['Sensitivity'] 
  specificity_DT <- cm_DT$byClass['Specificity']
  precision_DT <- cm_DT$byClass['Precision'] 
  recall_DT <- cm_DT$byClass['Recall']
  f1_DT <- cm_DT$byClass['F1']
  
  accuracy_list_DT<- append(accuracy_list_DT,accuracy_DT)
  sensitivity_list_DT<- append(sensitivity_list_DT,sensitivity_DT)
  specificity_list_DT<- append(specificity_list_DT,specificity_DT)
  precision_list_DT<- append(precision_list_DT,precision_DT)
  recall_list_DT<- append(recall_list_DT,recall_DT)
  f1_list_DT<- append(f1_list_DT,f1_DT)
  
  
  predict_class_DT_prob <- predict(model_DT, DT_test, type='prob')
  predict_class_list_DT <- data.frame(actual_type = DT_test$success,
                                      predict_type = predict_class_DT,
                                      prob_Yes = round(predict_class_DT_prob[ , 2], 5),
                                      prob_No = round(predict_class_DT_prob[ , 1], 5)) 
  
  appendedDf_DT <- rbind(appendedDf_DT, predict_class_list_DT)
  
}

library(gmodels)
CrossTable(predict_class_DT, DT_test$success, 
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('predicted default', 'actual default' ))


##AUC and ROC curve
# the parameters are the probability for prediction of the positive class and the actual class
pred_object_DT <- prediction(appendedDf_DT$prob_Yes, appendedDf_DT$actual_type) 
roc_DT<- performance(pred_object_DT, measure = "tpr", x.measure = "fpr")
plot(roc_DT, main = "DT ROC curve for ICO", col = "blue", lwd = 2) 
abline(a = 0, b = 1, lwd = 2, lty = 2,col = "red") 

auc_object_DT <- performance(pred_object_DT, measure = "auc")
auc_DT <- auc_object_DT@y.values[[1]]
auc_DT 

#Measures accuracy
accuracy_list_DT 
accuracy_average_DT <- mean(accuracy_list_DT)
accuracy_average_DT 
sd(accuracy_list_DT) 

sensitivity_list_DT
sensitivity_average_DT <- mean(sensitivity_list_DT)
sensitivity_average_DT 
sd(sensitivity_list_DT) 

specificity_list_DT
specificity_average_DT <- mean(specificity_list_DT)
specificity_average_DT 
sd(specificity_list_DT) 

precision_list_DT
precision_average_DT <- mean(precision_list_DT)
precision_average_DT 
sd(precision_list_DT) 

recall_list_DT
recall_average_DT <- mean(recall_list_DT)
recall_average_DT 
sd(recall_list_DT) 

f1_list_DT
f1_average_DT <- mean(f1_list_DT)
f1_average_DT 
sd(f1_list_DT) 


#_______________________________________________________________________________________

##Adaboost 

K = 10 
set.seed(0599)
folds <- createFolds(data_DT$success, k = K)
str(folds)  

accuracy_list_boost<-as.numeric() 
sensitivity_list_boost<-as.numeric() 
specificity_list_boost<-as.numeric()
precision_list_boost<-as.numeric()
recall_list_boost<-as.numeric()
f1_list_boost<-as.numeric()
appendedDf_boost<-data.frame()

for(i in 1:K){ 
  DT_test <- data_DT[folds[[i]],] 
  DT_train <- data_DT[-folds[[i]],]
  
  set.seed(2311)
  DT_boost<- C5.0(select(DT_train, -success), DT_train$success, 
                  trials = 10) 
  predict_class_boost <- predict(DT_boost, DT_test, type='class')
  
  cm_DT_boost <- confusionMatrix(predict_class_boost, DT_test$success, positive = "YES") 
  
  accuracy_DT_boost <- cm_DT_boost$overall['Accuracy']
  sensitivity_DT_boost <- cm_DT_boost$byClass['Sensitivity'] 
  specificity_DT_boost <- cm_DT_boost$byClass['Specificity']
  precision_DT_boost <- cm_DT_boost$byClass['Precision'] 
  recall_DT_boost <- cm_DT_boost$byClass['Recall']
  f1_DT_boost <- cm_DT_boost$byClass['F1']
  
  accuracy_list_boost<- append(accuracy_list_boost,accuracy_DT_boost)
  sensitivity_list_boost<- append(sensitivity_list_boost,sensitivity_DT_boost)
  specificity_list_boost<- append(specificity_list_boost,specificity_DT_boost)
  precision_list_boost<- append(precision_list_boost,precision_DT_boost)
  recall_list_boost<- append(recall_list_boost,recall_DT_boost)
  f1_list_boost<- append(f1_list_boost,f1_DT_boost)
  
  
  predict_class_DT_prob_boost <- predict(DT_boost, DT_test, type='prob')
  predict_class_list_boost <- data.frame(actual_type = DT_test$success,
                                         predict_type = predict_class_boost,
                                         prob_Yes = round( predict_class_DT_prob_boost[ , 2], 5),
                                         prob_No = round( predict_class_DT_prob_boost[ , 1], 5)) 
  
  appendedDf_boost <- rbind(appendedDf_boost, predict_class_list_boost)
  
}

CrossTable(predict_class_boost, DT_test$success, 
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('predicted default', 'actual default' ))

##AUC and ROC curve
# the parameters are the probability for prediction of the positive class and the actual class
pred_object_DT_boost <- prediction(appendedDf_boost$prob_Yes, appendedDf_boost$actual_type) 
roc_DT_boost<- performance(pred_object_DT_boost, measure = "tpr", x.measure = "fpr")
plot(roc_DT_boost, main = "AdaBoost ROC curve for ICO", col = "blue", lwd = 2) 
abline(a = 0, b = 1, lwd = 2, lty = 2, col="red") 

auc_object_DT_boost <- performance(pred_object_DT_boost, measure = "auc")
auc_DT_boost <- auc_object_DT_boost@y.values[[1]]
auc_DT_boost 

#Measures accuracy
accuracy_list_boost 
accuracy_average_DT_boost <- mean(accuracy_list_boost)
accuracy_average_DT_boost 
sd(accuracy_list_boost) 

sensitivity_list_boost
sensitivity_average_DT_boost <- mean(sensitivity_list_boost)
sensitivity_average_DT_boost 
sd(sensitivity_list_boost) 

specificity_list_boost
specificity_average_DT_boost <- mean(specificity_list_boost)
specificity_average_DT_boost 
sd(specificity_list_boost) 

precision_list_boost
precision_average_DT_boost <- mean(precision_list_boost)
precision_average_DT_boost 
sd(precision_list_boost) 

recall_list_boost
recall_average_DT_boost <- mean(recall_list_boost)
recall_average_DT_boost 
sd(recall_list_boost) 

f1_list_boost
f1_average_DT_boost <- mean(f1_list_boost)
f1_average_DT_boost 
sd(f1_list_boost)


#_________________________________________________________________________________________

#SVM

data_svm1 <- read.csv("ML Final Dataset.csv", stringsAsFactors = TRUE)
str(data_svm1)

data_svm <- as.data.frame(lapply(data_svm1[2:26], normalize))
data_svm$success<-data_svm1$success

K = 10 
set.seed(0599)
folds <- createFolds(data_svm$success, k = K)
str(folds)  

accuracy_list_svm<-as.numeric() 
sensitivity_list_svm<-as.numeric() 
specificity_list_svm<-as.numeric()
precision_list_svm<-as.numeric()
recall_list_svm<-as.numeric()
f1_list_svm<-as.numeric()
appendedDf_svm<-data.frame()


install.packages("kernlab")
library(kernlab)

for(i in 1:K){ 
  svm_test <- data_svm[folds[[i]],] 
  svm_train <- data_svm[-folds[[i]],]
  
  set.seed(2311)
  model_svm <- ksvm(success ~ ., data = svm_train, kernel = "vanilladot") 
  
  
  predict_class_svm<- predict(model_svm, select(svm_test, -success), type='response')
  
  cm_svm <- confusionMatrix(predict_class_svm, svm_test$success, positive = "YES") 
  
  accuracy_svm <- cm_svm$overall['Accuracy']
  sensitivity_svm <- cm_svm$byClass['Sensitivity'] 
  specificity_svm <- cm_svm$byClass['Specificity']
  precision_svm <- cm_svm$byClass['Precision'] 
  recall_svm <- cm_svm$byClass['Recall']
  f1_svm <- cm_svm$byClass['F1']
  
  accuracy_list_svm<- append(accuracy_list_svm,accuracy_svm)
  sensitivity_list_svm<- append(sensitivity_list_svm,sensitivity_svm)
  specificity_list_svm<- append(specificity_list_svm,specificity_svm)
  precision_list_svm<- append(precision_list_svm,precision_svm)
  recall_list_svm<- append(recall_list_svm,recall_svm)
  f1_list_svm<- append(f1_list_svm,f1_svm)
  
  model_svm_prob <- ksvm(success ~ ., data = svm_train, kernel = "vanilladot",prob.model=TRUE)
  predict_class_svm_prob <- predict(model_svm_prob, select(svm_test,-success), type='prob')
  predict_class_list_svm <- data.frame(actual_type = svm_test$success,
                                       predict_type = predict_class_svm,
                                       prob_Yes = round(predict_class_svm_prob[ , 2], 5),
                                       prob_No = round(predict_class_svm_prob[ , 1], 5))
  
  appendedDf_svm <- rbind(appendedDf_svm, predict_class_list_svm)
}

CrossTable(predict_class_svm, svm_test$success, 
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('predicted default', 'actual default' ))

##AUC and ROC curve
pred_object_svm <- prediction(appendedDf_svm$prob_Yes, appendedDf_svm$actual_type) 
roc_svm<- performance(pred_object_svm, measure = "tpr", x.measure = "fpr")
plot(roc_svm, main = "SVM Model ROC curve for ICO", col = "blue", lwd = 2) 
abline(a = 0, b = 1, lwd = 2, lty = 2, col = "red") 

auc_object_svm <- performance(pred_object_svm, measure = "auc")
auc_svm <- auc_object_svm@y.values[[1]]
auc_svm 

#Measures accuracy
accuracy_list_svm 
accuracy_average_svm <- mean(accuracy_list_svm)
accuracy_average_svm 
sd(accuracy_list_svm) 

sensitivity_list_svm
sensitivity_average_svm <- mean(sensitivity_list_svm)
sensitivity_average_svm 
sd(sensitivity_list_svm) 

specificity_list_svm
specificity_average_svm <- mean(specificity_list_svm)
specificity_average_svm 
sd(specificity_list_svm) 

precision_list_svm
precision_average_svm <- mean(precision_list_svm)
precision_average_svm 
sd(precision_list_svm) 

recall_list_svm
recall_average_svm <- mean(recall_list_svm)
recall_average_svm 
sd(recall_list_svm) 

f1_list_svm
f1_average_svm<- mean(f1_list_svm)
f1_average_svm 
sd(f1_list_svm) 

#Combined ROC Curve
plot(roc_knn_k3, main = "ROC Curve", col = "red", lwd = 2) 
plot(roc_DT, add=TRUE, col='blue')
plot(roc_DT_boost, add=TRUE, col='green')
plot(roc_svm, add=TRUE, col='red')
abline(a = 0, b = 1, lwd = 2, lty = 2,col = "black") 
legend(0.6,0.4,legend = c("K-NN","DT","AdaBoost","SVM"), col = c("red","blue","green","orange"),lty=1:1, cex=0.8)

#_________________________________________________________________________________________
#Random Forrest

library(randomForest)

data_rf1 <- read.csv("ML Final Dataset.csv", stringsAsFactors = TRUE)
str(data_rf)

data_rf <- as.data.frame(lapply(data_rf1[2:26], normalize))
data_rf$success<-data_rf1$success

K = 10 
set.seed(0599)
folds <- createFolds(data_rf$success, k = K)
str(folds)  

accuracy_list_rf<-as.numeric() 
sensitivity_list_rf<-as.numeric() 
specificity_list_rf<-as.numeric()
precision_list_rf<-as.numeric()
recall_list_rf<-as.numeric()
f1_list_rf<-as.numeric()
appendedDf_rf<-data.frame()

install.packages("randomForest")
library(randomForest)

for (i in 1:10){
  test_data <- data_rf[folds[[i]],]
  train_data<- data_rf[-folds[[i]],]
  model <- randomForest(success~., data = train_data, ntree = 5, ntry = 10) 
  pred<- predict(model, test_data)
  con_matrix<- confusionMatrix(pred, test_data$success, positive = "YES")
  accuracy_list_rf <- append(accuracy_list_rf, con_matrix$overall['Accuracy'])
  precision_list_rf <- append(precision_list_rf, con_matrix$byClass['Precision'])
  sensitivity_list_rf<- append(sensitivity_list_rf, con_matrix$byClass['Sensitivity'])
  specificity_list_rf<- append(specificity_list_rf, con_matrix$byClass['Specificity'])
  recall_list_rf<- append(recall_list_rf, con_matrix$byClass['Recall'])
  f1_list_rf<- append(f1_list_rf, con_matrix$byClass['F1'])
  
  predict_class_rf_prob <- predict(model, test_data, type='prob')
  predict_class_list_rf <- data.frame(actual_type = test_data$success,
                                         predict_type = pred,
                                         prob_Yes = round( predict_class_rf_prob[ , 2], 5),
                                         prob_No = round( predict_class_rf_prob[ , 1], 5)) 
  
  appendedDf_rf <- rbind(appendedDf_rf, predict_class_list_rf)
  
}

CrossTable(predict_class_svm, svm_test$success, 
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('predicted default', 'actual default' ))

##AUC and ROC curve
pred_object_RF <- prediction(appendedDf_rf$prob_Yes, appendedDf_rf$actual_type) # the parameters are the probability for prediction of the positive class and the actual class
roc_RF<- performance(pred_object_RF, measure = "tpr", x.measure = "fpr")
plot(roc_RF, main = "RF ROC curve for ICO", col = "blue", lwd = 2) 
abline(a = 0, b = 1, lwd = 2, lty = 2,col = "red") 

auc_object_RF <- performance(pred_object_RF, measure = "auc")
auc_RF <- auc_object_RF@y.values[[1]]
auc_RF 

#Measures accuracy
accuracy_list_rf 
accuracy_average_rf <- mean(accuracy_list_rf)
accuracy_average_rf 
sd(accuracy_list_rf) 

sensitivity_list_rf
sensitivity_average_rf <- mean(sensitivity_list_rf)
sensitivity_average_rf 
sd(sensitivity_list_rf) 

specificity_list_rf
specificity_average_rf <- mean(specificity_list_rf)
specificity_average_rf 
sd(specificity_list_rf) 

precision_list_rf
precision_average_rf <- mean(precision_list_rf)
precision_average_rf 
sd(precision_list_rf) 

recall_list_rf
recall_average_rf <- mean(recall_list_rf)
recall_average_rf 
sd(recall_list_rf) 

f1_list_rf
f1_average_rf <- mean(f1_list_rf)
f1_average_rf 
sd(f1_list_rf) 


