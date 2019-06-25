library(lasso2)
library(glmnet)
library(stats)
data(Prostate, package="lasso2")

set.seed(22)
train.index = sample(x=1:nrow(Prostate),
                     size=ceiling(0.8*nrow(Prostate)))

train = Prostate[train.index, ]
test = Prostate[-train.index, ]

# 1.先用 glmnet() 建立基本的Lasso模型
lasso1 = glmnet(x = as.matrix(train[, -9]), 
                y = train[, 9],
                #lambda = 0.1, #lambda像是懲罰值，是誤差項的係數
                alpha = 1, #0(Ridge)  1(Lasso)。
                family = "gaussian")

# 2.用 cv.glmnet() 找出最佳的懲罰值 best.lambda
cv.lasso = cv.glmnet(x = as.matrix(train[, -9]), 
                     y = train[, 9], 
                     alpha = 1,  #Lasso
                     family = "gaussian")
best.lasso.lambda = cv.lasso$lambda.min

#用 predict()進行預測
lasso.test = predict(lasso1, 
                     s = best.lasso.lambda, 
                     newx = as.matrix(test[, -9]))

#寫一個R_squared函數
R_squared <- function(actual, predict){
  mean_of_obs <- rep(mean(actual), length(actual))
  
  SS_tot <- sum((actual - mean_of_obs)^2)
  SS_reg <- sum((predict - mean_of_obs)^2)
  #SS_res <- sum((actual - predict)^2)
  R_squared <- SS_reg/SS_tot   #1 - (SS_res/SS_tot)
  R_squared
}

R_squared(test$lpsa, lasso.test)
