library(caret)
library(e1071)
library(arules)
library(rpart)
library(rpart.plot)
library(pROC)
library(gplots)
library(gains)
install.packages("rlang")
library(ggplot2)
install.packages("performanceEstimation")
install.packages("smotefamily")
library(performanceEstimation)
library(smotefamily)
install.packages("nnet")
library(nnet)
install.packages("e1071")
install.packages("MASS")
install.packages("DMwR2")
library(DMwR2)
install.packages("dplyr")
library(dplyr)

FraudTrans.df <- read.csv("C:/Users/alexv/OneDrive/Desktop/MIS/cust_transaction_details (1).csv")
CustomerTrans.df<- read.csv("C:/Users/alexv/OneDrive/Desktop/MIS/Customer_DF updated.csv")

View(FraudTrans.df)
View(CustomerTrans.df)

# Testing for missing Vlaues
sum(which(is.na(FraudTrans.df))) #Number of Missing Values in the data set
sum(which(is.na(CustomerTrans.df)))#Number of Missing Values in the data set

#Summary Statistics of a few variables
summary(CustomerTrans.df$No_Transactions)
summary(CustomerTrans.df$No_Orders)
summary(CustomerTrans.df$No_Payments)
summary(CustomerTrans.df$Fraud)

#Viewing the fraud variable
table(CustomerTrans.df$Fraud)

#Creating groups for low, medium, and high number of payments
CustomerTrans.df$PaymentCategory <- cut(CustomerTrans.df$No_Payments,
                                        breaks = c(-Inf, 5, 10, Inf),
                                        labels = c("Low", "Medium", "High"))

# View of the results of the change to the payment category 
table(CustomerTrans.df$PaymentCategory)

# Testing predicted frauds and correlation
CustomerTrans.df$PredictedFraud <- ifelse(CustomerTrans.df$No_Payments > 5, "True", "False")
table(Predicted = CustomerTrans.df$PredictedFraud, Actual = CustomerTrans.df$Fraud)
cor(as.numeric(CustomerTrans.df$Fraud), CustomerTrans.df$No_Payments)
table(CustomerTrans.df$PaymentCategory,CustomerTrans.df$Fraud)
CustomerTrans_Fraud_Factor <- as.factor(CustomerTrans.df$Fraud)
CustomerTrans.df$Fraud <- as.numeric(CustomerTrans.df$Fraud)
CustomerTrans.df[] <- lapply(CustomerTrans.df, function(x) {
  if (is.factor(x)) {
    return(as.numeric(x))
  } else if (is.character(x)) {
    return(as.numeric(factor(x)))
  } else {
    return(x)
  }
})
smote_result <- SMOTE(X = CustomerTrans.df[, -5], target = CustomerTrans.df$Fraud, dup_size = 1)
table(CustomerTrans.df$Fraud)
table(smote_result$data$Fraud)

# Before SMOTE
ggplot(CustomerTrans.df, aes(x = as.factor(Fraud))) +
  geom_bar() +
  ggtitle("Class Distribution Before SMOTE")

# After SMOTE
ggplot(smote_result$data, aes(x = as.factor(Fraud))) +
  geom_bar() +
  ggtitle("Class Distribution After SMOTE")

# Training data
set.seed(150)
trainIndex <- sample(seq_len(nrow(smote_result$data)), size = 0.7*nrow(smote_result$data))
trainData <- smote_result$data[trainIndex, ]
testData <- smote_result$data[-trainIndex, ]
# Select only Fraud and No_Payments columns
selected_data <- trainData[, c("Fraud", "No_Payments")]

# Convert 'Fraud' to numeric for correlation
selected_data$Fraud <- as.numeric(selected_data$Fraud)

# Calculate the correlation matrix
cor_matrix <- cor(selected_data)

# Print the correlation matrix and roeoaring model
cor_matrix

model <- glm(Fraud ~ No_Payments, data = trainData, family = "binomial")
summary(model)

resampled_data <- smote_result$data
resampled_data$PaymentCategory <- cut(
  resampled_data$No_Payments,
  breaks = c(-Inf, 5, 10, Inf),
  labels = c("Low", "Medium", "High")
)
# running Chi-Square and Fisher tests
chisq_test_result <- chisq.test(table(resampled_data$PaymentCategory, resampled_data$Fraud))
chisq_test_result
fisher_test <-fisher.test(table(resampled_data$PaymentCategory, resampled_data$Fraud))
cor(as.numeric(resampled_data$Fraud), resampled_data$No_Payments)
fisher_test

# Calculate predicted probabilities for each point in the data set
predicted_prob <- predict(model, newdata = trainData, type = "response")
# Ensure the 'Fraud' column from the smote_result$data is used
smote_data <- smote_result$data
predicted_prob <- predict(model, newdata = smote_data, type = "response")
# Plot the data points and logistic regression curve
ggplot(smote_data, aes(x = No_Payments, y = as.numeric(Fraud))) +
  geom_point(alpha = 0.5, color = "blue") +  # Actual data points (Fraud vs No_Payments)
  geom_line(aes(x = No_Payments, y = predicted_prob), color = "red") +  # Logistic regression curve
  labs(title = "Logistic Regression: Probability of Fraud vs No_Payments",
       x = "No_Payments", y = "Probability of Fraud") +
  theme_minimal()

table(FraudTrans.df$paymentMethodType,FraudTrans.df$transactionFailed )
# Updating to factors
FraudTrans.df[] <- lapply(FraudTrans.df, function(x) {
  if (is.factor(x)) {
    return(as.numeric(x))
  } else if (is.character(x)) {
    return(as.numeric(factor(x)))
  } else {
    return(x)
  }
})

class(FraudTrans.df$paymentMethodType)
FraudTrans.df$paymentMethodType <- as.numeric(factor(FraudTrans.df$paymentMethodType))


#Correlation Tests For FraudTrans
cor(as.numeric(FraudTrans.df$paymentMethodType), FraudTrans.df$transactionFailed)
cor_matrix <- cor(FraudTrans.df[, c("paymentMethodType", "transactionFailed")], use = "complete.obs")
cor_matrix
fisher_test <-fisher.test(table(FraudTrans.df$paymentMethodType,FraudTrans.df$transactionFailed ))
fisher_test
table(FraudTrans.df$paymentMethodType,FraudTrans.df$transactionFailed )



