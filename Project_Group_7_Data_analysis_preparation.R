######## libraries ###
install.packages("tidyverse")
library(tidyverse)
install.packages("scales")
library(dplyr)
install.packages("e1071")  
library(e1071)
install.packages("corrplot")
library(corrplot)

###### Initital analysis ####
Credit_data <- Par_Data_for_Logistic_Regression_Cleaned
str(Credit_data)
class(Credit_data)
summary(Credit_data)
glimpse(Credit_data)

###### Missing values check ####
sum(is.na(Credit_data))
Credit_data%>%summarise(count = sum(is.na(Credit_data)))

####### duplicate vaues check ####
sum(duplicated(Credit_data))
sum(duplicated(Credit_data$Customer_ID))

###### skewness check ####
skewness(Credit_data$Default_On_Payment)

####### correlation plot #####
Credit_data$Status_Checking_Acc <- factor(Credit_data$Status_Checking_Acc)
Credit_data$Credit_History <- factor(Credit_data$Credit_History)
Credit_data$Purposre_Credit_Taken <- factor(Credit_data$Purposre_Credit_Taken)
Credit_data$Savings_Acc <- factor(Credit_data$Savings_Acc)
Credit_data$Years_At_Present_Employment <- factor(Credit_data$Years_At_Present_Employment)
Credit_data$Marital_Status_Gender <- factor(Credit_data$Marital_Status_Gender)
Credit_data$Other_Debtors_Guarantors <- factor(Credit_data$Other_Debtors_Guarantors)
Credit_data$Property <- factor(Credit_data$Property)
Credit_data$Other_Inst_Plans <- factor(Credit_data$Other_Inst_Plans)
Credit_data$Housing <- factor(Credit_data$Housing)
Credit_data$Job <- factor(Credit_data$Job)
Credit_data$Telephone <- factor (Credit_data$Telephone)
Credit_data$Foreign_Worker <- factor (Credit_data$Foreign_Worker)

Credit_data2$Status_Checking_Acc <- as.numeric(Credit_data$Status_Checking_Acc)
Credit_data2$Credit_History <- as.numeric(Credit_data$Credit_History)
Credit_data2$Purposre_Credit_Taken <- as.numeric(Credit_data$Purposre_Credit_Taken)
Credit_data2$Savings_Acc <- as.numeric(Credit_data$Savings_Acc)
Credit_data2$Years_At_Present_Employment <- as.numeric(Credit_data$Years_At_Present_Employment)
Credit_data2$Marital_Status_Gender <- as.numeric(Credit_data$Marital_Status_Gender)
Credit_data2$Other_Debtors_Guarantors <- as.numeric(Credit_data$Other_Debtors_Guarantors)
Credit_data2$Property <- as.numeric(Credit_data$Property)
Credit_data2$Other_Inst_Plans <- as.numeric(Credit_data$Other_Inst_Plans)
Credit_data2$Housing <- as.numeric(Credit_data$Housing)
Credit_data2$Job <- as.numeric(Credit_data$Job)
Credit_data2$Telephone <- as.numeric (Credit_data$Telephone)
Credit_data2$Foreign_Worker <- as.numeric (Credit_data$Foreign_Worker)

Credit_data2 <- Credit_data
Credit_data2_cor <- cor(Credit_data2[, 2:22])
Credit_data2_cor <- as.matrix(Credit_data2_cor)
corrplot(cor(Credit_data2[, 2:22]), order="hclust")

##### Oultier analysis ####
Credit_data_Duration_in_months_outlier<-Credit_data[which(Credit_data$Duration_in_Months>40),]
str(Credit_data_Duration_in_months_outlier)


boxplot(Credit_data$Credit_Amount)
Credit_data_Credit_Amount_outlier<-Credit_data[which(Credit_data$Credit_Amount>5500),]
str(Credit_data_Credit_Amount_outlier)

boxplot(Credit_data$Age)
Credit_data_Age_outlier<-Credit_data[which(Credit_data$Age>65),]
str(Credit_data_Age_outlier)

boxplot(Credit_data$Num_CC)
Credit_data_Num_CC_outlier<-Credit_data[which(Credit_data$Num_CC>3),]
str(Credit_data_Num_CC_outlier)



