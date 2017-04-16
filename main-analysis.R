#Include packages
library(ggplot2)
library(caret)

#Setting file name
filename <- "VoterPref.csv"

#Reading csv into dataframe and create success class
df <- read.csv(filename, header = TRUE)
SUCCESS <- ifelse((df$PREFERENCE == 'Against'), 1, 0)
df <- cbind(df, SUCCESS)
attach(df)
head(df)

#Setting seed value and sampling
set.seed(123457)
train <- sample(nrow(df), 0.7*nrow(df))
df_train <- df[train,]
df_val <- df[-train,]
str(df_train)
str(df_val)

#Boxplot
ggplot(data = df_train, mapping = aes(x = PREFERENCE, y = INCOME)) + geom_boxplot()
ggplot(data = df_train, mapping = aes(x = PREFERENCE, y = AGE)) + geom_boxplot()

#Stats for FOR and AGAINST
count <- table(df_train$PREFERENCE)
proportions <- c(round(count["Against"]/nrow(df_train), digits = 4), 
                  round(count["For"]/nrow(df_train), digits = 4))
proportions

table(df_train$PREFERENCE, df_train$GENDER)

#Regression and metrics
fit1 <- lm(SUCCESS ~ AGE + INCOME + GENDER, data = df_train)

df_train$PREDICTED_PROBABILITY <- predict(fit1, df_train)
Metrics_Train <- c("AVG ERROR","RMSE", "MAE")
avg_err_train <- mean(df_train$SUCCESS - df_train$PREDICTED_PROBABILITY)
rmse_train <- sqrt(mean((df_train$SUCCESS - df_train$PREDICTED_PROBABILITY)^2))
mae_train <- mean(abs(df_train$SUCCESS - df_train$PREDICTED_PROBABILITY))
Values_Train <- c(avg_err_train, rmse_train, mae_train)
metrics_table_train <- data.frame(Metrics_Train, Values_Train)
metrics_table_train

df_val$PREDICTED_PROBABILITY <- predict(fit1, df_val)
Metrics_Validation <- c("AVG ERROR", "RMSE", "MAE")
avg_err_val <- mean(df_val$SUCCESS - df_val$PREDICTED_PROBABILITY)
rmse_val <- sqrt(mean((df_val$SUCCESS - df_val$PREDICTED_PROBABILITY)^2))
mae_val <- mean(abs(df_val$SUCCESS - df_val$PREDICTED_PROBABILITY))
Values_Validation <- c(avg_err_val, rmse_val, mae_val)
metrics_table_val <- data.frame(Metrics_Validation, Values_Validation)
metrics_table_val

#Classification and confusion matrix
df_train$PREDICTED_OUTCOME <- ifelse((df_train$PREDICTED_PROBABILITY > 0.5), 1, 0)
df_val$PREDICTED_OUTCOME <- ifelse((df_val$PREDICTED_PROBABILITY > 0.5), 1, 0)
head(df_train)
head(df_val)
conf_matrix_train <- confusionMatrix(df_train$PREDICTED_OUTCOME, df_train$SUCCESS)
conf_matrix_val <- confusionMatrix(df_val$PREDICTED_OUTCOME, df_val$SUCCESS)
conf_matrix_train
conf_matrix_val

#Logistic regression
fit2 <- glm(SUCCESS ~ AGE + INCOME + GENDER, data = df_train, family = "binomial")
summary(fit2)

#Confusion matrix
df_train$PREDICTED_PROBABILITY_GLM <- predict(fit2, df_train, type = "response")
df_train$PREDICTED_OUTCOME_GLM <- ifelse((df_train$PREDICTED_PROBABILITY_GLM > 0.5), 1, 0)

df_val$PREDICTED_PROBABILITY_GLM <- predict(fit2, df_val, type = "response")
df_val$PREDICTED_OUTCOME_GLM <- ifelse((df_val$PREDICTED_PROBABILITY_GLM > 0.5), 1, 0)

head(df_train)
head(df_val)

conf_matrix_train_glm <- confusionMatrix(df_train$PREDICTED_OUTCOME_GLM, df_train$SUCCESS)
conf_matrix_val_glm <- confusionMatrix(df_val$PREDICTED_OUTCOME_GLM, df_val$SUCCESS)
conf_matrix_train_glm
conf_matrix_val_glm

#Predicting on new data
headers <- c("GENDER", "AGE", "INCOME")
df_newdata <- data.frame('F', 36, 70)
colnames(df_newdata) <- headers

df_newdata$PREDICTED_PROBABILITY_GLM <- predict(fit2, df_newdata, type = "response")
df_newdata$PREDICTED_OUTCOME_GLM <- ifelse((df_newdata$PREDICTED_PROBABILITY_GLM > 0.5), 1, 0)

df_newdata


