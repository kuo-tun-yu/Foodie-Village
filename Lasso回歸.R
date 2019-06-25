library(lasso2)
library(glmnet)
library(stats)
data(Prostate, package="lasso2")

set.seed(22)
train.index = sample(x=1:nrow(Prostate),
                     size=ceiling(0.8*nrow(Prostate)))

train = Prostate[train.index, ]
test = Prostate[-train.index, ]

lasso1 = glmnet(x = as.matrix(train[, -9]), 
               y = train[, 9],
               #lambda = 0.1, #lambda像是懲罰值，是誤差項的係數
               alpha = 1, #0(Ridge)  1(Lasso)。
               family = "gaussian")
#family參數的值要看Y，Y是連續值就用"gaussian"
#Y是二元就用"binomial"，Y是多元分類就用"multinomial"


plot(lasso1, xvar='lambda', main="Lasso")

#---------cv就是Cross Validation -------------------
#這塊的目的只是要找出 最佳的lambda
cv.lasso = cv.glmnet(x = as.matrix(train[, -9]), 
                     y = train[, 9],
                     alpha = 1,  # lasso
                     family = "gaussian")

# 評估每個模型的 cvm(mean cross-validated error)後
# 取最小 cvm 模型所對應的 lambda
length(cv.lasso$lambda) 
best.lambda = cv.lasso$lambda.min
best.lambda
#-------找到最佳的lambda(best.lambda)-----------------


plot(lasso1, xvar='lambda', main="Lasso")
abline(v=log(best.lambda), col="blue", lty=5.5 )

#看看那些X被踢了
coef(cv.lasso, s = "lambda.min")
#取出重要變數 但發現第一個是截距項
select.ind = which(coef(cv.lasso, s = "lambda.min") != 0)
select.ind = select.ind[-1]-1 #扣掉一開始的截距項
select.ind # 第幾個變數是重要的(沒被踢)

#---踢完沒用的欄位，馬上用新的欄位重做--------------

# 挑出重要變數的名稱
select.varialbes = colnames(train)[select.ind]
select.varialbes

#"lpsa"是Y，得知重要變數後重做一次迴歸
lasso2=lm(lpsa ~ ., train[, c(select.varialbes, "lpsa")])
summary(lasso2) #看R-squared

#使用predict()進行預測
lasso.test = predict(lasso2, 
                     #s = best.lambda, 
                     newx = as.matrix(test[,select.varialbes ]))


