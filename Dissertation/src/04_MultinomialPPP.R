###############################################################################
##### SCRIPT FOR RUNNING THE MULTINOMIAL POINTS PER POSSESSION MODEL #########
###############################################################################
adv$id <- 

df_train <- adv %>% 
  filter(Year < 2014) %>% 
  mutate(MPG = MP/G) %>% 
  group_by(Year,Player) %>% 
  summarise(across(everything(), mean))

df_test <- adv %>% 
  filter(Year == 2014) %>% 
  mutate(MPG = MP/G) %>% 
  group_by(Year,Player) %>% 
  summarise(across(everything(), mean))

names(df_train) <- gsub("%","_p",names(df_train))
setnames(df_train, old = c('3PAr'), new = c('x3PAr'))
names(df_test) <- gsub("%","_p",names(df_test))
setnames(df_test, old = c('3PAr'), new = c('x3PAr'))



x_train <- df_train %>% select(-c(BPM,Player,Year,Rk,Pos,Tm,G,MP,X25,DBPM,OBPM))
x_test <- df_test %>% select(-c(BPM,Player,Year,Rk,Pos,Tm,G,MP,X25,DBPM,OBPM))

y_train <- df_train %>% select(BPM)
y_test <- df_test %>% select(BPM)

print(paste("Using train dataset of length ",nrow(df_train)))
print(paste("Using test dataset of length ",nrow(df_test)))

# Check Linear Reg
fit <- lm(BPM ~
            Age +
            MPG+
            PER +
            TS_p+
            x3PAr+
            FTr+
            ORB_p+
            DRB_p+
            TRB_p+
            AST_p+
            STL_p+
            BLK_p+
            TOV_p+
            USG_p+
            OWS+
            DWS+
            WS+
            #OBPM+
            #DBPM+
            VORP, data = df_train)
summary(fit)
scaled <- scale(fit)
pred <- predict(fit,df_test)
resids <-  sum((df_test[,"BPM"] - pred)^2)
SSE <- sum((pred - df_test[,"BPM"])^2)
SST <- sum((df_test[,"BPM"] - mean(df_test[,"BPM"]))^2)
R_square <- 1 - SSE / SST



# Fit model:
lambdas <- 10^seq(3, -3, by = -.1)
fit <- glmnet(x_train[,-c("X20","Year")], y_train[,"BPM"], nlambda = 25, alpha = 0.1, lambda = lambdas)

#Get optimal lambdas:
cv_fit <- cv.glmnet(x_train, y_train, alpha = 0.1, lambda = lambdas)
opt_lambda <- cv_fit$lambda.min


# Predict on training(regular season)
y_predicted <- predict(fit, s = opt_lambda, newx = log(x_train +1))
sst <- sum((y_train - mean(y_train))^2)
sse <- sum((y_predicted - y_train)^2)
rsq <- 1 - (sse / sst)

#Predict on test (playoffs)
y_predicted_test <- predict(fit, s = opt_lambda, newx = log(x_test+1))
sst_test <- sum((y_test - mean(y_test))^2)
sse_test <- sum((y_predicted_test - y_test)^2)
rsq_test <- 1 - (sse_test / sst_test)
