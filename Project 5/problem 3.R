# problem 3

library(caret) # for cross-validation
library(ISLR)
library(glmnet) # for ridge and lasso
library(pls) # for pcr and pls
train.data <- College
str(train.data)
attach(train.data)

# part a)
lm.fit <- train(Apps ~ ., 
                method = "lm", 
                data = train.data, 
                trControl = trainControl(method = "LOOCV"))

print(lm.fit)
# Resampling results:
#   
#     RMSE      Rsquared  MAE     
# 1130.038  0.91468   630.0335

1130.038^2 # = 1276986

lm.pred <- predict(lm.fit, train)
mean((Apps - lm.pred)^2) # training error
# [1] 1059279




# part b)
# Create response vector and the design matrix (without the first column of 1s)
y <- Apps 
x <- model.matrix(Apps ~ ., train.data)[, -1]

grid <- 10^seq(8, -2, length = 200)

# Fit ridge regression for each lambda on the grid
ridge.out <- glmnet(x, y, alpha = 0, lambda = grid)
plot(ridge.out, xvar = "lambda")

# leave one out cross-validation
set.seed(1)
ridge.cv.out <- cv.glmnet(x, y, alpha = 0, lambda = grid, nfolds = dim(train.data)[1])
plot(ridge.cv.out)

print(ridge.cv.out)
# Measure: Mean-Squared Error 
# 
#     Lambda Measure     SE Nonzero
# min   0.01 1277006 258548      17
# 1se 235.43 1516697 508117      17

# Find the best value of lambda
ridge.bestlam <- ridge.cv.out$lambda.min
# [1] 0.01
# log(ridge.bestlam) = -4.60517


coef.ridge <- predict(ridge.out, type = "coefficients", s = ridge.bestlam)
#                         1
# (Intercept) -445.26830402
# PrivateYes  -494.15980608
# Accept         1.58570739
# Enroll        -0.88022903
# Top10perc     49.92174051
# Top25perc    -14.23153321
# F.Undergrad    0.05734870
# P.Undergrad    0.04444658
# Outstate      -0.08586349
# Room.Board     0.15104313
# Books          0.02090569
# Personal       0.03109799
# PhD           -8.67805769
# Terminal      -3.33091855
# S.F.Ratio     15.38992988
# perc.alumni    0.17691111
# Expend         0.07789878
# Grad.Rate      8.66799704 



ridge.pred <- predict(ridge.out, s = ridge.bestlam, newx = x, type='response')
train.error.ridge <- mean((ridge.pred - y)^2) # training error
# [1] 1059279



# part c)
grid <- 10^seq(5, -2, length = 200)

# Fit lasso regression for each lambda on the grid
lasso.out <- glmnet(x, y, alpha = 1, lambda = grid)
plot(lasso.out, xvar = "lambda")

# leave one out cross-validation
set.seed(1)
lasso.cv.out <- cv.glmnet(x, y, alpha = 1, lambda = grid, nfolds = dim(train.data)[1])
plot(lasso.cv.out)

print(lasso.cv.out)
# Measure: Mean-Squared Error 
# 
#     Lambda Measure     SE Nonzero
# min   0.01 1277099 258652      17
# 1se  318.1 1524047 355598       3

# Find the best value of lambda
lasso.bestlam <- lasso.cv.out$lambda.min
# [1] 0.01
# log(lasso.bestlam) = -4.60517


coef.lasso <- predict(lasso.out, type = "coefficients", s = lasso.bestlam)
#                         1
# (Intercept) -445.26830402
# PrivateYes  -494.15980608
# Accept         1.58570739
# Enroll        -0.88022903
# Top10perc     49.92174051
# Top25perc    -14.23153321
# F.Undergrad    0.05734870
# P.Undergrad    0.04444658
# Outstate      -0.08586349
# Room.Board     0.15104313
# Books          0.02090569
# Personal       0.03109799
# PhD           -8.67805769
# Terminal      -3.33091855
# S.F.Ratio     15.38992988
# perc.alumni    0.17691111
# Expend         0.07789878
# Grad.Rate      8.66799704 



lasso.pred <- predict(lasso.out, s = lasso.bestlam, newx = x, type='response')
train.error.lasso <- mean((lasso.pred - y)^2) # training error
# [1] 1059279





# part d)
pcr.fit <- pcr(Apps~., data=train.data, scale=T, validation="LOO")

# Scree plot
validationplot(pcr.fit, val.type="MSEP")

MSEP(pcr.fit)
#        (Intercept)   1 comps  2 comps  3 comps  4 comps  5 comps  6 comps  7 comps  8 comps  9 comps  10 comps
# CV         1.5e+07  14744421  4123358  4157777  2883584  2506292  2510774  2492036  2376917  2245553   2237022
# adjCV      1.5e+07  14744466  4123261  4157770  2879623  2505933  2510639  2492027  2376694  2245437   2236914
#        11 comps  12 comps  13 comps  14 comps  15 comps  16 comps  17 comps
# CV      2251874   2251604   2267449   2267624   2067302   1357315   1276987
# adjCV   2251762   2251488   2267334   2267512   2066763   1357131   1276825

pcr.pred <- predict(pcr.fit, train.data, ncomp=which.min(MSEP(pcr.fit)$val[1,1,]) - 1)
mean((Apps - pcr.pred)^2) # training error
# [1] 1059279



# part e)
pls.fit <- plsr(Apps~., data=train.data, scale=T, validation="LOO")

# Scree plot
validationplot(pls.fit, val.type="MSEP")

MSEP(pls.fit)
#        (Intercept)  1 comps  2 comps  3 comps  4 comps  5 comps  6 comps  7 comps  8 comps  9 comps  10 comps
# CV         1.5e+07  3418008  2414639  2049701  1806343  1371151  1325548  1301942  1297801  1283053   1282432
# adjCV      1.5e+07  3417887  2414598  2049563  1805870  1370529  1325245  1301788  1297869  1282936   1282273
#        11 comps  12 comps  13 comps  14 comps  15 comps  16 comps  17 comps
# CV      1280546   1279732   1278057   1277875   1277232   1276966   1276987
# adjCV   1280398   1279576   1277894   1277712   1277070   1276804   1276825



pls.pred <- predict(pls.fit, train.data, ncomp=which.min(MSEP(pls.fit)$val[1,1,]) - 1)
mean((Apps - pls.pred)^2) # training error
# [1] 1059279




