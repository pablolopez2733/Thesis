# RIDGE REGRESSION --------------------------------
# We must first run the matrix_functions script in order to have the 
# necessary functions.

# Libraries--------------------------------------------------------------------
library(tidyverse)
library(dplyr)
library(readr)
library(glmnet)

# Ridge Regression Test with Cleveland:--------------------------------------

# Set matrices:
x_train = x_matrix("CLE")
y_train = y_vector("CLE")
x_test = x_matrix_playoffs("CLE")
y_test = y_vector_playoffs("CLE")

# Fit model:
lambdas <- 10^seq(2, -3, by = -.1)
fit <- glmnet(x_train, y_train, nlambda = 25, alpha = 0, lambda = lambdas)
fit_test <- glmnet(x_test, y_test, nlambda = 25, alpha = 0, lambda = lambdas)

#Get optimal lambdas:
cv_fit <- cv.glmnet(x_train, y_train, alpha = 0, lambda = lambdas)
opt_lambda <- cv_fit$lambda.min
cv_fit_test <- cv.glmnet(x_test, y_test, alpha = 0, lambda = lambdas)
opt_lambda_test <- cv_fit_test$lambda.min

# Predict on training(regular season)
y_predicted <- predict(fit, s = opt_lambda, newx = x_train)
sst <- sum((y_train - mean(y_train))^2)
sse <- sum((y_predicted - y_train)^2)
rsq <- 1 - (sse / sst)

#Predict on test (playoffs)
y_predicted_test <- predict(fit_test, s = opt_lambda_test, newx = x_test)
sst_test <- sum((y_test - mean(y_test))^2)
sse_test <- sum((y_predicted_test - y_test)^2)
rsq <- 1 - sse_test / sst_test









# Trying to create a function which returns the regression results for each team
# (INCOMPLETE)

ridge_regression <- function (team){
  
  x_season <- x_matrix(team)
  y_season <- y_vector(team)
  x_test <-x_matrix_playoffs(team)
  y_test <- y_vector_playoffs(team)
  
  # Fit model:
  lambdas <- 10^seq(2, -3, by = -.1)
  fit <- glmnet(x_season, y_season, nlambda = 25, alpha = 0, lambda = lambdas)
  fit_test <- glmnet(x_test, y_test, nlambda = 25, alpha = 0, lambda = lambdas)
  
  #Get optimal lambda:
  cv_fit <- cv.glmnet(x, y_season, alpha = 0, lambda = lambdas)
  opt_lambda <- cv_fit$lambda.min
  cv_fit_test <- cv.glmnet(x_test, y_test, alpha = 0, lambda = lambdas)
  opt_lambda_test <- cv_fit_test$lambda.min
  
  #Predict on training
  y_predicted <- predict(fit, s = opt_lambda, newx = x_season)
  y_predicted_test <- predict(fit_test, s = opt_lambda_test, newx = x_test)
  # Sum of Squares Total and Error
  sst <- sum((y_season - mean(y_season))^2)
  sse <- sum((y_predicted - y_season)^2)
  
  sst_test <- sum((y_test - mean(y_test))^2)
  sse_test <- sum((y_predicted_test - y_test)^2)
  
  # R squared
  rsq <- 1 - (sse / sst)
  rsq_test <- 1 - (sse_test / sst_test)
  
  #bind vectors
  train_results <- c(sst,sse,rsq)
  test_results <- c(sst_test,sse_test,rsq_test)
  results <- rbind(train_results,test_results)
  
  return(results)
  
}





