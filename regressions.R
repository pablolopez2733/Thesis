# RIDGE REGRESSION --------------------------------
# We must first run the matrix_functions script in order to have the 
# necessary functions.

# Libraries--------------------------------------------------------------------
library(tidyverse)
library(dplyr)
library(readr)
library(glmnet)
library(caret)

####################################################
# Compute R^2 from true and predicted values
eval_results <- function(true, predicted, df) {
  SSE <- sum((predicted - true)^2)
  SST <- sum((true - mean(true))^2)
  R_square <- 1 - SSE / SST
  RMSE = sqrt(SSE/nrow(df))
  
  
  # Model performance metrics
  data.frame(
    RMSE = RMSE,
    Rsquare = R_square
  )
  
}


# Ridge Regression Test with Cleveland:--------------------------------------

# Set matrices:
t <- "CLE"
x_train = log(x_matrix(t)+1)
y_train = y_vector(t)
x_test = log(x_matrix_playoffs(t)+1)
y_test = y_vector_playoffs(t)

# Fit model:
lambdas <- 10^seq(3, -3, by = -.1)
fit <- glmnet(x_train, y_train, nlambda = 25, alpha = 0, lambda = lambdas)

#Get optimal lambdas:
cv_fit <- cv.glmnet(x_train, y_train, alpha = 0, lambda = lambdas)
opt_lambda <- cv_fit$lambda.min


# Predict on training(regular season)
y_predicted <- predict(fit, s = opt_lambda, newx = x_train)
sst <- sum((y_train - mean(y_train))^2)
sse <- sum((y_predicted - y_train)^2)
rsq <- 1 - (sse / sst)

#Predict on test (playoffs)
y_predicted_test <- predict(fit, s = opt_lambda, newx = x_test)
sst_test <- sum((y_test - mean(y_test))^2)
sse_test <- sum((y_predicted_test - y_test)^2)
rsq_test <- 1 - (sse_test / sst_test)






##############################################################
# Elastic Net
##############################################################
train <- as.data.frame(cbind(x_train,y_train)) 


# Set training control
train_cont <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 5,
                           search = "random",
                           verboseIter = TRUE)

# Train the model
elastic_reg <- train(y_train ~ .,
                     data = train,
                     method = "glmnet",
                     preProcess = c("center", "scale"),
                     tuneLength = 10,
                     trControl = train_cont)


# Best tuning parameter
best_e <- elastic_reg$bestTune

# Make predictions on training set
predictions_train <- predict(elastic_reg, x_train)
eval_results(y_train, predictions_train, train) 

# Make predictions on test set
predictions_test <- predict(elastic_reg, x_test)
eval_results(y_test, predictions_test, test)




