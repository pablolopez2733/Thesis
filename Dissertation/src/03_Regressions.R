###############################################################################
############################### 03_ REGRESSIONS ###############################
###############################################################################
# ' Script for running the ridge, lasso and elastic net regressions

set.seed(33)



# No transform playoffs elastic with manual alpha :---------------------------
t <- "GSW"
x_train <- x_matrix(t)
y_train <- y_vector(t)
x_test <- x_matrix_playoffs(t)
y_test <- y_vector_playoffs(t)
a <- 0

# Fit model:
lambdas <- 10^seq(4, -4, by = -.1)
fit <- glmnet(x_train, y_train, alpha = a , lambda = lambdas,standardize = FALSE)

#Get optimal lambdas:
cv_fit <- cv.glmnet(x_train, y_train, alpha = a, lambda = lambdas)
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





# No transform elastic playoffs with auto parameter search----------------------
# Define train and test set
train <- as.data.frame(cbind(x_train,y_train))
test <- as.data.frame(cbind(x_test,y_test))

# Set training control
train_cont <- trainControl(method = "cv",
                           number = 10,
                           search = "random",
                           verboseIter = TRUE)

# Train the model
elastic_reg <- train(y_train ~ .,
                     data = train,
                     method = "glmnet",
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


# Elastic experiment but all samples are in the regular season and manual alpha ==================
set.seed(33)
t = "CLE"
x <- x_matrix(t)
y <- y_vector(t)
x_train <- x[1:58,]
y_train <- y[1:58]
x_test <- x[59:82,]
y_test <- y[59:82]
a<-0

# Fit model:
lambdas <- 10^seq(4, -4, by = -.1)
fit <- glmnet(x_train, y_train, alpha = a, lambda = lambdas)

#Get optimal lambdas:
cv_fit <- cv.glmnet(x_train, y_train, alpha = a, lambda = lambdas,standardize = FALSE)
opt_lambda <- cv_fit$lambda.min


# Predict on training(regular season)
y_predicted <- predict(fit, s = opt_lambda, newx = x_train)
sst <- sum((y_train - mean(y_train))^2)
sse <- sum((y_predicted - y_train)^2)
rsq <- 1 - (sse / sst)

#Predict on test (last 24 games)
y_predicted_test <- predict(fit, s = opt_lambda, newx = x_test)
sst_test <- sum((y_test - mean(y_test))^2)
sse_test <- sum((y_predicted_test - y_test)^2)
rsq_test <- 1 - (sse_test / sst_test)




###############################################################################
############################# RIDGE REGRESSION ################################
###############################################################################

# Function for getting full season fit on Ridge Regression
fit_ridge <- function(team)
{

  x_train <- x_matrix(team)
  y_train <- y_vector(team)
  
  lambdas <- 10^seq(4, -4, by = -.1)
  fit <- glmnet(x_train, y_train, alpha = 0 , lambda = lambdas,standardize = FALSE)
  
  #Get optimal lambdas:
  cv_fit <- cv.glmnet(x_train, y_train, alpha = 0, lambda = lambdas)
  opt_lambda <- cv_fit$lambda.min
  
  
  # Predict on training(regular season)
  y_predicted <- predict(fit, s = opt_lambda, newx = x_train)
  sst <- sum((y_train - mean(y_train))^2)
  sse <- sum((y_predicted - y_train)^2)
  rsq <- 1 - (sse / sst)
  rmse = sqrt(sse/nrow(x_train))
  
  
  
  
  # tLL <- fit$nulldev - deviance(fit)
  # k <- fit$df
  # n <- fit$nobs
  # 
  # AICc <- -tLL+2*k+2*k*(k+1)/(n-k-1)
  # BIC<-log(n)*k - tLL
  
  
  print(paste0("TEAM: ",team))
  print(paste0("Lambda: ",opt_lambda))
  print(paste0("R^2: ",rsq))
  print(paste0("sst: ",sst,"| sse: ",sse))
  # print(paste0("AIC: ",aic,"| sse: ",sse))
  # print(paste0("BIC: ",bic,"| sse: ",sse))
  
  results <- data.frame(TEAM = team,
                        LAMBDA = opt_lambda,
                        R2 = rsq,
                        SST = sst,
                        SSE = sse,
                        RMSE = rmse
                        )
  return(results)
  
  
}

# Create results dataframe:
names <- c("TEAM","LAMBDA","R2","SST","SSE","RMSE")
ridge_res <- data.frame()
for (k in names) ridge_res[[k]] <- as.character()

# Execute function and append results into a dataframe
for (t in teams) {
  ridge_res <- rbind(ridge_res,fit_ridge(t))
  print(paste0("Progress: ",nrow(ridge_res)," out of 30"))
}

# Write results to a csv file
write.csv(ridge_res,"outputs/ridge_seasonfit_nonorm.csv")


################################################################################
######################### LASSO REGRESSION #####################################
################################################################################

# Function for fitting LASSO regression to whole season=========================
fit_lasso <- function(team)
{
  x_train <- x_matrix(team)
  y_train <- y_vector(team)
  lambdas <- 10^seq(4, -4, by = -.1)
  fit <- glmnet(x_train, y_train, alpha = 1 , lambda = lambdas, standardize = FALSE)
  
  #Get optimal lambdas:
  cv_fit <- cv.glmnet(x_train, y_train, alpha = 1, lambda = lambdas)
  opt_lambda <- cv_fit$lambda.min
  
  
  # Predict on training(regular season)
  y_predicted <- predict(fit, s = opt_lambda, newx = x_train)
  sst <- sum((y_train - mean(y_train))^2)
  sse <- sum((y_predicted - y_train)^2)
  rsq <- 1 - (sse / sst)
  rmse = sqrt(sse/nrow(x_train))
  
  print(paste0("TEAM: ",team))
  print(paste0("Lambda: ",opt_lambda))
  print(paste0("R^2: ",rsq))
  print(paste0("sst: ",sst,"| sse: ",sse))
  

  
  
  
  
  # tLL <- fit$nulldev - deviance(fit)
  # k <- fit$df
  # n <- fit$nobs
  # 
  # AICc <- -tLL+2*k+2*k*(k+1)/(n-k-1)
  # BIC<-log(n)*k - tLL

  # print(paste0("AIC: ",aic,"| sse: ",sse))
  # print(paste0("BIC: ",bic,"| sse: ",sse))
  
  results <- data.frame(TEAM = team,
                        LAMBDA = opt_lambda,
                        R2 = rsq,
                        SST = sst,
                        SSE = sse,
                        RMSE = rmse
  )
  return(results)
  
}

# Create results dataframe:
names <- c("TEAM","LAMBDA","R2","SST","SSE","RMSE")
lasso_res <- data.frame()
for (k in names) lasso_res[[k]] <- as.character()

# Execute function and append results into a dataframe
for (t in teams) {
  lasso_res <- rbind(lasso_res,fit_lasso(t))
  print(paste0("Progress: ",nrow(lasso_res)," out of 30"))
}

# Write results to a csv file
write.csv(lasso_res,"outputs/lasso_seasonfit_no_standarization.csv")





###############################################################################
########################## ELASTIC NET ########################################
###############################################################################

# Function for extracting best fit in an elastic net regression
get_best_result = function(caret_fit) 
  {
  best = which(rownames(caret_fit$results) == rownames(caret_fit$bestTune))
  best_result = caret_fit$results[best, ]
  rownames(best_result) = NULL
  best_result
  }



# Function for fitting ELASTIC NET model to whole season:======================

fit_elastic <- function(team)
{
  set.seed(33)
  x_train <- x_matrix(team)
  y_train <- y_vector(team)
  
  # Define train and test set
  train <- as.data.frame(cbind(x_train,y_train))
  
  # Set training control
  train_cont <- trainControl(method = "cv",
                             number = 10,
                             search = "random",
                             verboseIter = TRUE)
  
  # Train the model
  elastic_reg <- train(y_train ~ .,
                       data = train,
                       method = "glmnet",
                       preProcess = c("center", "scale"),
                       tuneLength = 10, #10 alpha values and 10 lambdas for each
                       trControl = train_cont)
  
  
  # Best tuning parameter
  best_e <- elastic_reg$bestTune
  params <- get_best_result(elastic_reg)
  
  # Make predictions on training set
  predictions_train <- predict(elastic_reg, x_train)
  
  
  sst <- sum((y_train - mean(y_train))^2)
  sse <- sum((predictions_train - y_train)^2)
  rsq <- 1 - (sse / sst)
  rmse = sqrt(sse/nrow(x_train))
  
  res <- data.frame(TEAM = team,
                        R2_pred = rsq,
                        SST = sst,
                        SSE = sse
  )
  
  results <- cbind(res,params) 
  
  return(results)
  
}

# Create results dataframe:
names <- c("TEAM",
           "R2_pred",
           "SST",
           "SSE",
           "alpha",
           "lambda",
           "RMSE",
           "Rsquared",
           "MAE",
           "RMSESD",
           "RsquaredSD",
           "MAESD")
elastic_net_res <- data.frame()
for (k in names) elastic_net_res[[k]] <- as.character()

for (t in teams) {
  elastic_net_res <- rbind(elastic_net_res,fit_elastic(t))
  print(paste0("Progress: ",nrow(elastic_net_res)," out of 30 teams"))
}

# Write results to a csv file
write.csv(elastic_net_res,"outputs/elasticnet_seasonfits_withproc.csv")

################################################################################
# Elastic net but with training and test in regular season
################################################################################


# Function for extracting best fit in an elastic net regression
get_best_result = function(caret_fit) 
{
  best = which(rownames(caret_fit$results) == rownames(caret_fit$bestTune))
  best_result = caret_fit$results[best, ]
  rownames(best_result) = NULL
  best_result
}



fit_elastic <- function(team)
{
  set.seed(33)
  x_train <- x_matrix(team)
  y_train <- y_vector(team)
  x_train <- x[1:58,]
  y_train <- y[1:58]
  x_test <- x[59:82,]
  y_test <- y[59:82]
  
  
  # Define train and test set
  train <- as.data.frame(cbind(x_train,y_train))
  
  # Set training control
  train_cont <- trainControl(method = "cv",
                             number = 10,
                             search = "random",
                             verboseIter = TRUE)
  
  # Train the model
  elastic_reg <- train(y_train ~ .,
                       data = train,
                       method = "glmnet",
                       preProcess = c("center", "scale"),
                       tuneLength = 10, #10 alpha values and 10 lambdas for each
                       trControl = train_cont)
  
  
  # Best tuning parameter
  best_e <- elastic_reg$bestTune
  params <- get_best_result(elastic_reg)
  
  # Make predictions on training set
  predictions_train <- predict(elastic_reg, x_test)
  
  
  sst <- sum((y_train - mean(y_train))^2)
  sse <- sum((predictions_train - y_train)^2)
  rsq <- 1 - (sse / sst)
  rmse = sqrt(sse/nrow(x_train))
  
  res <- data.frame(TEAM = team,
                    R2_pred = rsq,
                    SST = sst,
                    SSE = sse
  )
  
  results <- cbind(res,params) 
  
  return(results)
  
}

# Create results dataframe:
names <- c("TEAM",
           "R2_pred",
           "SST",
           "SSE",
           "alpha",
           "lambda",
           "RMSE",
           "Rsquared",
           "MAE",
           "RMSESD",
           "RsquaredSD",
           "MAESD")
elastic_net_res <- data.frame()
for (k in names) elastic_net_res[[k]] <- as.character()

for (t in teams) {
  elastic_net_res <- rbind(elastic_net_res,fit_elastic(t))
  print(paste0("Progress: ",nrow(elastic_net_res)," out of 30 teams"))
}

# Write results to a csv file
write.csv(elastic_net_res,"outputs/elasticnet_train_test.csv")
