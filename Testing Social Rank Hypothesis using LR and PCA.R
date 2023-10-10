#-----Filter distributed percentage & price....dsi<100% 
#1. Installing packages and library

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
install.packages("knitr")
install.packages("ineq")
install.packages("stats")
install.packages("summarytools")


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
library(knitr)
library(stats)
library(ineq)
library(data.table)
library(statar)
library(summarytools)

#_______________________________________________________________________________________________

#Exploratory Data Analysis (EDA)

#Read the data
raw_data <- read.csv("C:/Users/Vikash/Desktop/Dissertation/Dissertation P/2017,2018,2019 PSID Data.csv", 
                     stringsAsFactors = FALSE, encoding = 'UTF-8')

gini_data <- read.csv("C:/Users/Vikash/Desktop/Dissertation/Dissertation P/Gini Index All Years.csv", 
                      stringsAsFactors = FALSE, encoding = 'UTF-8')

#_______________________________________________________________________________________________
#Joining Gini Index with PS data
PS_data<-left_join(raw_data, gini_data, by = c('Year','Residence_Code_FIPS'))
summary(PS_data$State)
table(PS_data$State)
nrow(PS_data)
sum(is.na(PS_data$State))
sum(is.na(PS_data$Gini_Index))  
sum(is.na(PS_data$Residence_Code_FIPS)) 

#_______________________________________________________________________________________________
#Omitting rows with no state code
PS_data <- PS_data[complete.cases(PS_data$Gini_Index), ]
sum(is.na(PS_data$State))
sum(is.na(PS_data$Gini_Index)) 
nrow(PS_data)

#Multiplying gini index with 100
PS_data$Gini_Index<-PS_data$Gini_Index*100

#_______________________________________________________________________________________________
#Omitting rows for Age field with '999' and replacing age>100 with 100 for capping
PS_data<-PS_data[PS_data$Age != 999, ]
PS_data$Age <- ifelse(PS_data$Age > 100, 100, PS_data$Age)
nrow(PS_data)

#Summary Table
summary(PS_data$Age)

#_______________________________________________________________________________________________

#Omitting rows for Marital status field with '9' 
PS_data<-PS_data[PS_data$Marital_Status != 9, ]

nrow(PS_data)

#Summary Table
summary(PS_data$Marital_Status)
table(PS_data$Marital_Status)
table(PS_data$Sex)

#_______________________________________________________________________________________________

#Omitting rows for Employment Status field with '99' 
PS_data<-PS_data[PS_data$Employment_Status != 99, ]

nrow(PS_data)

#Summary Table
summary(PS_data$Employment_Status)
table(PS_data$Employment_Status)

#_______________________________________________________________________________________________

#Omitting rows for Health status field with '9' 
PS_data<-PS_data[PS_data$Health_Status != 9 & PS_data$Health_Status != 8 , ]

nrow(PS_data)

#Summary Table
summary(PS_data$Health_Status)

#_______________________________________________________________________________________________

#Omitting rows for Unemployment Status field with '9' 
PS_data<-PS_data[PS_data$Unemployment_Status != 9, ]

nrow(PS_data)

#Summary Table
summary(PS_data$Unemployment_Status)

#_______________________________________________________________________________________________

#Omitting rows for Mental Health Score field with '99' 
PS_data<-PS_data[PS_data$Mental_Health_Score != 99, ]

nrow(PS_data)

#Summary Table
summary(PS_data$Mental_Health_Score)
summary(PS_data)

#_______________________________________________________________________________________________
#Removing computing expense
PS_data<-select(PS_data, -Computing_Expenditure)
PS_data<-select(PS_data, -Food_Expenditure)

#_______________________________________________________________________________________________
#Calculating Total Expenditure
PS_data$Total_Expenditure <- PS_data$Food_At_Home_Expenditure+PS_data$Food_Away_From_Home_Expenditure+PS_data$Food_Delivered_Expenditure+PS_data$Housing_Expenditure+PS_data$Rent_Expenditure+PS_data$Property_Tax_Expenditure+PS_data$Home_Insurance_Expenditure+PS_data$Utility_Expenditure+PS_data$Telephone_Internet_Expenditure+PS_data$Transportation_Expenditure+PS_data$Education_Expenditure+PS_data$Childcare_Expenditure+PS_data$Health_Care_Expenditure+PS_data$Health_Insurance_Expenditure+PS_data$Household_Repairs_Expenditure+PS_data$Household_Furnishing_Expenditure+PS_data$Clothing_Expenditure+PS_data$Trips_Expenditure+PS_data$Other_Recreational_Expenditure
summary(PS_data)

#_______________________________________________________________________________________________
# Function to count negative values in a vector
count_negatives <- function(x) {
  sum(x < 0, na.rm = TRUE)
}

# Apply the function to each column using sapply()
negative_counts <- sapply(PS_data, count_negatives)

# Print the summary
print(negative_counts)

#_______________________________________________________________________________________________

#Visual representation of missing values
png("missing_data_plot.png", width = 1200, height = 800, units = "px")
print(summary(aggr(PS_data,prop=FALSE, type="both",numbers=TRUE)))
dev.off()

#Column wise missing data summary
missing_values <-sapply(PS_data,function(x) sum(is.na(x) | x==" " |x=="" ))

complete.cases(PS_data)
!complete.cases(PS_data)

#Summarising missing values
summary_missing <- data.frame(
  Variable= names(PS_data),
  Missing_Values = missing_values,
  Percent_missing = round((missing_values/ nrow(PS_data))*100,2),
  Type=sapply(PS_data, class)
)


#_______________________________________________________________________________________________
#Removing top and bottom 5% of data as a part of outlier treatment
PS_data_tf<-PS_data

#Summary
summary(PS_data)
nrow(PS_data_tf)

# Specify the columns in which you want to remove as outliers
columns_to_convert <- c("Total_Family_Income", "Food_At_Home_Expenditure","Food_Away_From_Home_Expenditure","Food_Delivered_Expenditure", "Housing_Expenditure","Rent_Expenditure",
                        "Property_Tax_Expenditure","Home_Insurance_Expenditure","Utility_Expenditure","Telephone_Internet_Expenditure","Transportation_Expenditure","Education_Expenditure","Health_Care_Expenditure","Childcare_Expenditure","Clothing_Expenditure",
                        "Household_Furnishing_Expenditure","Trips_Expenditure","Health_Insurance_Expenditure","Household_Repairs_Expenditure","Other_Recreational_Expenditure","Total_Expenditure","Bus_Train_Expenditure","Taxi_Cab_Expenditure","Other_Transportation_Expenditure")


# Calculate the lower and upper quantile boundaries
lower_bound <- quantile(PS_data_tf[, columns_to_convert], 0.01, na.rm = TRUE)
upper_bound <- quantile(PS_data_tf[, columns_to_convert], 0.99, na.rm = TRUE)

# Filter the data to exclude values below the lower quantile and above the upper quantile
PS_data_tf <- PS_data_tf %>%
  filter(across(all_of(columns_to_convert), ~ . >= lower_bound & . <= upper_bound))

summary(PS_data_tf)
nrow(PS_data_tf)



# writing it to view the trimmed data frame

write.table(PS_data_tf, file = "Data check for outlier removal.csv", 
            row.names=F,sep = ",")

#_______________________________________________________________________________________________


# Create a tabular summary
df_summary <- PS_data_tf %>%
  descr(summary = TRUE)

# Create a data frame with the top 10 expenditure columns and their means
top_10_expenditures <- data.frame(
  Expenditure_Category = c("Housing_Expenditure", "Transportation_Expenditure", "Health_Care_Expenditure", 
                           "Food_Away_From_Home_Expenditure", "Food_At_Home_Expenditure", 
                           "Utilities_Expenditure", "Household_Repairs_Expenditure", 
                           "Clothing_Expenditure", "Education_Expenditure", "Trips_Expenditure"),
  Mean_Expenditure = c(14586, 7373, 2648, 2015, 5080, 2576, 795.6, 1042, 671.7, 1042)
)

# Create a bar plot using ggplot2
ggplot(top_10_expenditures, aes(x = reorder(Expenditure_Category, Mean_Expenditure), 
                                y = Mean_Expenditure, fill = Expenditure_Category)) +
  geom_bar(stat = "identity") +
  labs(title = "Top 10 Mean Expenditures by Category", 
       x = "Expenditure Category", y = "Mean Expenditure") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Male %
Sex<-table(PS_data_tf$Sex)
# Calculate the total number of observations
total_observations <- length(PS_data_tf$Sex)

# Calculate the percentage frequency for each category
percentage_freq <- (Sex / total_observations) * 100
print(percentage_freq)

#Marital status summary
table(PS_data_tf$Marital_Status)

#Histogram
hist(PS_data$Health_Care_Expenditure)


# Apply the function to each column using sapply()
negative_counts <- sapply(PS_data_tf, count_negatives)

# Print the summary
print(negative_counts)
summary(PS_data_tf)
#_______________________________________________________________________________________________
#Summary statistics

# Income Inequality Measures
Gini(PS_data_tf$Total_Family_Income)
Theil(PS_data_tf$Total_Family_Income)

# Distributional Analysis
lorenz_curve <- Lc(PS_data_tf$Total_Family_Income)
plot(lorenz_curve)

# Percentile Ratios
percentile_ratio <- quantile(PS_data_tf$Total_Family_Income, probs = c(0.9, 0.1)) / quantile(PS_data_tf$Total_Family_Income, probs = c(0.1, 0.9))
print(percentile_ratio)

# Cross-tabulation of Marital_Status and Employment_Status
cross_tab <- table(PS_data_tf$Marital_Status, PS_data_tf$Employment_Status)
print(cross_tab)

summary(PS_data_tf)
#_______________________________________________________________________________________________

#Age squared and cubed

PS_data_tf$Age_sq <- PS_data_tf$Age^2
PS_data_tf$Age_cb <- PS_data_tf$Age^3

summary(PS_data_tf$Age_sq)
PS_data_PCA<-PS_data_tf
#_______________________________________________________________________________________________

#Selecting columns for corelation
selected_cols_cor <- c("Gini_Index","Total_Family_Income", "Food_At_Home_Expenditure","Food_Away_From_Home_Expenditure","Food_Delivered_Expenditure", "Housing_Expenditure","Rent_Expenditure",
                       "Property_Tax_Expenditure","Home_Insurance_Expenditure","Utility_Expenditure","Telephone_Internet_Expenditure","Transportation_Expenditure","Education_Expenditure","Health_Care_Expenditure","Childcare_Expenditure","Clothing_Expenditure",
                       "Trips_Expenditure","Health_Insurance_Expenditure","Household_Repairs_Expenditure","Other_Recreational_Expenditure")  
# Calculate the correlation matrix
correlation_matrix <- cor(PS_data_tf[, selected_cols_cor])

# Print the correlation matrix
print(correlation_matrix)
summary(PS_data_tf)

#Corelation
par(mar = c(5,5,4,4))
corrplot(cor(PS_data_tf))

# Example correlation matrix (replace this with your own data)
correlation_matrix <- cor(PS_data_tf)

# Plot the correlation matrix
corrplot(correlation_matrix, method = "color", type = "upper", order = "hclust", tl.cex = 0.5)
cor_table <- kable(correlation_matrix, format = "html", digits = 2)
cor_df <- as.data.frame(correlation_matrix)

#_______________________________________________________________________________________________

#Columns for CC and NCC
Conspicuous_consumption_cols<-c("Food_Away_From_Home_Expenditure","Food_Delivered_Expenditure","Clothing_Expenditure","Trips_Expenditure","Telephone_Internet_Expenditure","Other_Recreational_Expenditure")

Non_Conspicuous_consumption_cols<-c("Food_At_Home_Expenditure","Housing_Expenditure","Rent_Expenditure", "Property_Tax_Expenditure","Home_Insurance_Expenditure","Utility_Expenditure","Transportation_Expenditure","Education_Expenditure","Health_Care_Expenditure","Childcare_Expenditure",
                                    "Health_Insurance_Expenditure","Household_Repairs_Expenditure")

#Calculating Conspicuous Expenditure
PS_data_tf$Conspicuous_Expenditure <- PS_data_tf$Food_Away_From_Home_Expenditure+PS_data_tf$Food_Delivered_Expenditure+PS_data_tf$Telephone_Internet_Expenditur+PS_data_tf$Clothing_Expenditure+PS_data_tf$Trips_Expenditure+PS_data_tf$Other_Recreational_Expenditure
summary(PS_data_tf$Conspicuous_Expenditure)

PS_data_tf$Non_Conspicuous_Expenditure <-PS_data_tf$Food_At_Home_Expenditure+PS_data_tf$Housing_Expenditure+PS_data_tf$Rent_Expenditure+PS_data_tf$Property_Tax_Expenditure+PS_data_tf$Home_Insurance_Expenditure+PS_data_tf$Utility_Expenditure+PS_data_tf$Transportation_Expenditure+PS_data_tf$Education_Expenditure+PS_data_tf$Childcare_Expenditure+PS_data_tf$Health_Care_Expenditure+PS_data_tf$Health_Insurance_Expenditure+PS_data_tf$Household_Repairs_Expenditure+PS_data_tf$Household_Furnishing_Expenditure
summary(PS_data_tf$Non_Conspicuous_Expenditure)

#_______________________________________________________________________________________________

#Removing Family ID for processing
PS_data_tf<-select(PS_data_tf, -Family_ID)

#Separate data frames for CC
PS_data_CC<-subset(PS_data_tf, select = c("Age","Age_sq","Age_cb","Sex","No_of_Children",
                                          "Marital_Status","Life_Satisfaction","Employment_Status","Health_Status",
                                          "Unemployment_Status","Total_Family_Income","Gini_Index","Conspicuous_Expenditure"))
str(PS_data_CC)

#_______________________________________________________________________________________________
#Separate data frames for NCC
PS_data_NCC<-subset(PS_data_tf, select = c("Age","Age_sq","Age_cb","Sex","No_of_Children",
                                           "Marital_Status","Life_Satisfaction","Employment_Status","Health_Status",
                                           "Unemployment_Status","Total_Family_Income","Gini_Index","Non_Conspicuous_Expenditure"))
str(PS_data_NCC)

#_______________________________________________________________________________________________

#Converting to Vector for Conspicuous Consumption
col_names_CC_ft  <- c( "Sex","Marital_Status","Life_Satisfaction","Employment_Status","Health_Status","Unemployment_Status")
PS_data_CC[,col_names_CC_ft] <- lapply(PS_data_CC[,col_names_CC_ft] , factor)
str(PS_data_CC$Employment_Status)

#Converting to Vector for Non Conspicuous Consumption
col_names_NCC_ft <- c( "Sex",
                       "Marital_Status","Life_Satisfaction","Employment_Status","Health_Status","Unemployment_Status")
PS_data_NCC[,col_names_NCC_ft] <- lapply(PS_data_NCC[,col_names_NCC_ft] , factor)
str(PS_data_NCC)

sum(is.na(PS_data_NCC)) 
#_______________________________________________________________________________________________

#Missing data checks
#Column wise missing data summary
missing_values <-sapply(PS_data_CC,function(x) sum(is.na(x) | x==" " |x=="" ))

complete.cases(PS_data_CC)
!complete.cases(PS_data_CC)
!complete.cases(PS_data_NCC)

#Summarising missing values
summary_missing <- data.frame(
  Variable= names(PS_data_CC),
  Missing_Values = missing_values,
  Percent_missing = round((missing_values/ nrow(PS_data_CC))*100,2),
  Type=sapply(PS_data_CC, class)
)
#_______________________________________________________________________________________________

#CC Regression
str(PS_data_CC)
summary(PS_data_CC)
PS_data_CC[is.na(PS_data_CC) | PS_data_CC == "Inf"| PS_data_CC == "NaN"| PS_data_CC == "NA"] <- NA
write.table(PS_data_CC, file = "CC_data.csv", 
            row.names=F,sep = ",")
PS_data_CC$Conspicuous_Expenditure<-ifelse(PS_data_CC$Conspicuous_Expenditure == 0, 1, PS_data_CC$Conspicuous_Expenditure)
PS_data_CC$Total_Family_Income<-ifelse(PS_data_CC$Total_Family_Income == 0, 1, PS_data_CC$Total_Family_Income)

# Fit the linear regression model
model_CC <- lm(log(Conspicuous_Expenditure) ~ Gini_Index + log(Total_Family_Income) +Age + Age_sq +Age_cb+ Sex + No_of_Children + Marital_Status + Life_Satisfaction + Employment_Status + Health_Status + Unemployment_Status, data = PS_data_CC)

# Print the coefficients
print(coefficients(model_CC))
summary(model_CC)

#_______________________________________________________________________________________________

PS_data_NCC$Total_Family_Income <- ifelse(PS_data_NCC$Total_Family_Income == 0, 1, PS_data_NCC$Total_Family_Income)
PS_data_NCC$Non_Conspicuous_Expenditure<-ifelse(PS_data_NCC$Non_Conspicuous_Expenditure == 0, 1, PS_data_NCC$Non_Conspicuous_Expenditure)


# Fit the linear regression model for NCC
model_NCC <- lm(log(Non_Conspicuous_Expenditure) ~ Gini_Index +log(Total_Family_Income)+Age + Age_sq + Age_cb + Sex + No_of_Children + Marital_Status + Life_Satisfaction + Employment_Status + Health_Status + Unemployment_Status, data = PS_data_NCC)

# Print the coefficients
print(coefficients(model_NCC))
summary(model_NCC)
summary(PS_data_NCC$Total_Family_Income)
summary(PS_data_NCC$Non_Conspicuous_Expenditure)
summary(PS_data_CC)


#_______________________________________________________________________________________________
#PCA
#Subsetting columns
PS_data_PCA <- subset(PS_data_tf, select = c("Food_Away_From_Home_Expenditure","Food_Delivered_Expenditure","Clothing_Expenditure","Trips_Expenditure","Telephone_Internet_Expenditure","Other_Recreational_Expenditure","Food_At_Home_Expenditure","Housing_Expenditure","Rent_Expenditure", "Property_Tax_Expenditure","Home_Insurance_Expenditure","Utility_Expenditure","Transportation_Expenditure","Education_Expenditure","Health_Care_Expenditure","Childcare_Expenditure",
                                             "Health_Insurance_Expenditure","Household_Repairs_Expenditure","Bus_Train_Expenditure","Taxi_Cab_Expenditure","Other_Transportation_Expenditure"))
str(PS_data_PCA)


#Converting to matrix
PS_data_PCA_matrix <- as.matrix(PS_data_PCA)

#PCA using PRCOMP
library(stats)
pca_result <- prcomp(PS_data_PCA_matrix, center= TRUE, scale. = TRUE)

summary(pca_result)

# Biplot 
biplot(pca_result, scale = 0)
# Scree plot
plot(pca_result, type = "l")

#Varimax rotation
varimax7 <- varimax(pca_result$rotation[,1:7])
varimax7 <- as.matrix(varimax7)

# Extract principal components
pca_result$rotation

# Extract standard deviations (square roots of eigenvalues)
pca_result$sdev

# Extract variance explained by each principal component
pca_result$sdev^2 / sum(pca_result$sdev^2)

varimax7$sdev^2 / sum(varimax7$sdev^2)

sum_var<- sum(pca_result$sdev^2 / sum(pca_result$sdev^2))


