# Illustration of LDA and QDA for classification using Stock Market dataset
# (which was also used for KNN; see also Chapter 1 and Lab in Chapter 4)

# Use Direction as response, Lag1 and Lag2 as predictors,
# data from 2001-2004 as training data, and data from 2005
# as test data (see Chapter 4 Lab)

library(ISLR)

# > str(Smarket)
# 'data.frame':	1250 obs. of  9 variables:
 # $ Year     : num  2001 2001 2001 2001 2001 ...
 # $ Lag1     : num  0.381 0.959 1.032 -0.623 0.614 ...
 # $ Lag2     : num  -0.192 0.381 0.959 1.032 -0.623 ...
 # $ Lag3     : num  -2.624 -0.192 0.381 0.959 1.032 ...
 # $ Lag4     : num  -1.055 -2.624 -0.192 0.381 0.959 ...
 # $ Lag5     : num  5.01 -1.055 -2.624 -0.192 0.381 ...
 # $ Volume   : num  1.19 1.3 1.41 1.28 1.21 ...
 # $ Today    : num  0.959 1.032 -0.623 0.614 0.213 ...
 # $ Direction: Factor w/ 2 levels "Down","Up": 2 2 1 2 2 2 1 2 2 2 ...
# > 

attach(Smarket)

# Training data

train <- (Year < 2005)
train.X <- cbind(Lag1, Lag2)[train, ]
train.y <- Direction[train]

# > dim(train.X)
# [1] 998   2
# >
# > table(train.y)
# train.y
# Down   Up 
# 491  507 
# > 

# Plot data

plot(train.X, xlab = "Lag1", ylab = "Lag2", col = ifelse(train.y == 
	"Up", "green", "red"))

par(mfrow = c(1, 2))
boxplot(train.X[, "Lag1"] ~ train.y, ylab = "Lag1", ylim = c(-6, 6))
boxplot(train.X[, "Lag2"] ~ train.y, ylab = "Lag2", ylim = c(-6, 6))
par(mfrow = c(1, 1))

# --------------------------------------------------------------------#

# Our function for performing LDA on training data

s6340.lda <- function(y, X) {
	# y = training data response vector (a factor)
	# X = training data predictor matrix 
	N <- length(y) # no of observations
	K <- nlevels(y) # no of classes
	p <- ncol(X) # no of predictors
	n <- as.numeric(table(y)) # class frequencies
	names(n) <- levels(y)
	pi <- n/N # class proportions
	# mean vector
	mu <- matrix(unlist(by(X, y, colMeans)), byrow = T, ncol = p)
	rownames(mu) <- levels(y)
	colnames(mu) <- colnames(X)
	# pooled covariance matrix 
	S <- by(X, y, cov)
	Sigma <- Reduce("+", lapply(1:K, FUN = function(k) {(n[k] - 1) * S[[k]]}))/(N - K)
	# its inverse
	Sigma.inv <- solve(Sigma)
	# delta functions
	delta <- t(sapply(1:K, FUN = function(k) {
		c(-(1/2) * drop(t(mu[k, ]) %*% Sigma.inv %*% mu[k, ]) + 
			log(pi[k]), t(mu[k, ]) %*% Sigma.inv)}))
	rownames(delta) <- levels(y)
	colnames(delta) <- c("(Intercept)", colnames(X))
	# pairwise difference of delta functions
	idx.pair <- combn(K, 2)
	delta.diff <- t(apply(idx.pair, MAR = 2, FUN = function(pair) {
		delta[pair[1], ] - delta[pair[2], ]}))
	rownames(delta.diff) <- apply(idx.pair, MAR = 2, FUN = function(pair) {
		paste0(levels(y)[pair[1]], "-", levels(y)[pair[2]])})
	# multiply intecept difference by 1 to get the cutoff c	
	delta.diff[, 1] <- -delta.diff[, 1]
	colnames(delta.diff)[1] <- "Cutoff"
	# result
	result <- list(N = N, n = n, pi = pi, mu = mu, Sigma = Sigma, 
		delta = delta, disc = delta.diff)
	return(result)
}

# Apply our LDA function

our.lda.fit <- s6340.lda(train.y, train.X)

# > our.lda.fit
# $N
# [1] 998

# $n
# Down   Up 
 # 491  507 

# $pi
    # Down       Up 
# 0.491984 0.508016 

# $mu
            # Lag1        Lag2
# Down  0.04279022  0.03389409
# Up   -0.03954635 -0.03132544

# $Sigma
            # Lag1        Lag2
# Lag1  1.51189766 -0.03346941
# Lag2 -0.03346941  1.51256749

# $delta
     # (Intercept)        Lag1        Lag2
# Down  -0.7103162  0.02881250  0.02304587
# Up    -0.6781024 -0.02662828 -0.02129933

# $disc
            # Cutoff       Lag1      Lag2
# Down-Up 0.03221375 0.05544078 0.0443452

# > 

pi <- our.lda.fit$pi
mu <- our.lda.fit$mu
Sigma <- our.lda.fit$Sigma
coeff <- our.lda.fit$disc[1, -1]
cutoff <- our.lda.fit$disc[1, 1]

# Predictions for training data

score.train <- train.X %*% coeff
pred.train <- ifelse(score.train >= cutoff, "Down", "Up")

# > table(pred.train, train.y)
          # train.y
# pred.train Down  Up
      # Down  168 160
      # Up    323 347
# > 

# Predictions for test data

test.X <- cbind(Lag1, Lag2)[!train, ]
test.y <- Direction[!train]

score.test <- test.X %*% coeff
pred.test <- ifelse(score.test >= cutoff, "Down", "Up")

# > table(pred.test, test.y)
         # test.y
# pred.test Down  Up
     # Down   35  35
     # Up     76 106
# > 

# Plot test data together with decision boundary

plot(test.X, xlab = "Lag1", ylab = "Lag2", col = ifelse(test.y == 
	"Up", "green", "red"))
abline(coef = c(cutoff, -coeff[1])/coeff[2])

# > c(cutoff, -coeff[1])/coeff[2]
                 # Lag1 
 # 0.7264315 -1.2502092 
# > 

# Posteriors for test data

library(mvtnorm)

t1 <- pi[1]*dmvnorm(test.X, mean = mu[1, ], sigma = Sigma)
t2 <- pi[2]*dmvnorm(test.X, mean = mu[2, ], sigma = Sigma)
prob.test <- cbind(t1, t2)/(t1 + t2)
colnames(prob.test) <- rownames(mu)

# > head(prob.test)
          # Down        Up
# [1,] 0.4901792 0.5098208
# [2,] 0.4792185 0.5207815
# [3,] 0.4668185 0.5331815
# [4,] 0.4740011 0.5259989
# [5,] 0.4927877 0.5072123
# [6,] 0.4938562 0.5061438
# > 

# --------------------------------------------------------------------#

# Apply LDA function from MASS package to training data

library(MASS)

lda.fit <- lda(Direction ~ Lag1 + Lag2, data = Smarket, subset = train)

# > lda.fit
# Call:
# lda(Direction ~ Lag1 + Lag2, data = Smarket, subset = train)

# Prior probabilities of groups:
    # Down       Up 
# 0.491984 0.508016 

# Group means:
            # Lag1        Lag2
# Down  0.04279022  0.03389409
# Up   -0.03954635 -0.03132544

# Coefficients of linear discriminants:
            # LD1
# Lag1 -0.6420190
# Lag2 -0.5135293
# > 

# Note: These coefficients don't match up with those given by 
# our.lda.fit$disc as these are obtained by applying a linear 
# transformation. However, we will see that the
# predictions and the posterior probabilities are identical.

# > our.lda.fit$disc
            # Cutoff       Lag1      Lag2
# Down-Up 0.03221375 0.05544078 0.0443452
# > 

# > coef(lda.fit)
            # LD1
# Lag1 -0.6420190
# Lag2 -0.5135293
# > 

# > our.lda.fit$disc[1, 2:3]/coef(lda.fit)
             # LD1
# Lag1 -0.08635379
# Lag2 -0.08635379
# > 

# Get predictions for test data

Smarket.2005 <- Smarket[!train, ]
lda.pred <- predict(lda.fit, Smarket.2005)

# > names(lda.pred)
# [1] "class"     "posterior" "x"        
# > 

# > table(lda.pred$class, test.y)
      # test.y
       # Down  Up
  # Down   35  35
  # Up     76 106
# > 

# Compare predictions from the two fits

# > mean(lda.pred$class == pred.test)
# [1] 1
# > 

# The predictions are identical!

# Compare posterior probabilities from the fits

# > head(lda.pred$posterior)
          # Down        Up
# 999  0.4901792 0.5098208
# 1000 0.4792185 0.5207815
# 1001 0.4668185 0.5331815
# 1002 0.4740011 0.5259989
# 1003 0.4927877 0.5072123
# 1004 0.4938562 0.5061438
# > 

# > summary(abs(lda.pred$posterior - prob.test))
      # Down                 Up           
 # Min.   :0.000e+00   Min.   :0.000e+00  
 # 1st Qu.:0.000e+00   1st Qu.:0.000e+00  
 # Median :5.551e-17   Median :0.000e+00  
 # Mean   :5.595e-17   Mean   :4.890e-17  
 # 3rd Qu.:1.110e-16   3rd Qu.:1.110e-16  
 # Max.   :2.220e-16   Max.   :2.776e-16  
# > 


# The posterior probabilities can also be considered identical!

# Decision boundary (using the "blind" contour approach; test data)
# Set up a dense grid and compute posterior prob on the grid

n.grid <- 50
x1.grid <- seq(f = min(test.X[, 1]), t = max(test.X[, 1]), l = n.grid)
x2.grid <- seq(f = min(test.X[, 2]), t = max(test.X[, 2]), l = n.grid)
grid <- expand.grid(x1.grid, x2.grid)
colnames(grid) <- colnames(test.X)

# > head(grid)
       # Lag1   Lag2
# 1 -1.672000 -1.672
# 2 -1.597592 -1.672
# 3 -1.523184 -1.672
# 4 -1.448776 -1.672
# 5 -1.374367 -1.672
# 6 -1.299959 -1.672
# > 

pred.grid <- predict(lda.fit, grid)

# > head(pred.grid$posterior)
       # Down        Up
# 1 0.3720668 0.6279332
# 2 0.3748877 0.6251123
# 3 0.3777171 0.6222829
# 4 0.3805549 0.6194451
# 5 0.3834008 0.6165992
# 6 0.3862548 0.6137452
# >
# > dim(pred.grid$posterior)
# [1] 2500    2
# > 

prob <- matrix(pred.grid$posterior[, "Up"], nrow = n.grid, ncol = n.grid, byrow = F)

plot(test.X, col = ifelse(test.y == "Up", "green", "red"))

contour(x1.grid, x2.grid, prob, levels = 0.5, labels = "", xlab = "", ylab = "", 
        main = "", add = T)
# Add boundary from our fit
abline(coef = c(cutoff, -coeff[1])/coeff[2], col = "blue") 

# Note: As expected, the boundaries from the two fits are identical.

# --------------------------------------------------------------------#

# Quadratic Discriminant Analysis

qda.fit <- qda(Direction ~ Lag1 + Lag2, data = Smarket, subset = train)

# > qda.fit
# Call:
# qda(Direction ~ Lag1 + Lag2, data = Smarket, subset = train)

# Prior probabilities of groups:
    # Down       Up 
# 0.491984 0.508016 

# Group means:
            # Lag1        Lag2
# Down  0.04279022  0.03389409
# Up   -0.03954635 -0.03132544
# > 

qda.pred <- predict(qda.fit, Smarket.2005)

# > table(qda.pred$class, test.y)
      # test.y
       # Down  Up
  # Down   30  20
  # Up     81 121
# >

#---------------------------------------------------------------------#
# Optimal KNN (K = 70) --- Recall

library(class)
set.seed(1)
knn.fit <- knn(train.X, test.X, train.y, k = 70, prob = T)

knn.prob <- attr(knn.fit, "prob") # prob is voting fraction for winning class
knn.prob <- ifelse(knn.fit == "Up", knn.prob, 1 - knn.prob) # now it is voting fraction for Direction == "Up"


#---------------------------------------------------------------------#

# A Naive approach --- predict Up for every day

naive <- rep("Up", length(test.y))
#---------------------------------------------------------------------#

# Comparison of performance (p = 0.5 for LDA and QDA)
# Test error rates

# > mean(lda.pred$class != test.y)
# [1] 0.4404762
# >  

# > mean(qda.pred$class != test.y)
# [1] 0.4007937
# > 

# > mean(knn.fit != test.y)
# [1] 0.452381
# > 

# > mean(naive != test.y)
# [1] 0.4404762
# > 


# Confusion matrix and (sensitivity, specificity)
# `+' = Up, `-' = Down

# > table(lda.pred$class, test.y)
      # test.y
       # Down  Up
  # Down   35  35
  # Up     76 106
# > 

# > c(106/(106 + 35), 35/(35 + 76))
# [1] 0.7517730 0.3153153
# > 

# > table(qda.pred$class, test.y)
      # test.y
       # Down  Up
  # Down   30  20
  # Up     81 121
# > 

# > c(121/(20 + 121), 30/(30 + 81))
# [1] 0.8581560 0.2702703
# > 

# > table(knn.fit, test.y)
       # test.y
# knn.fit Down Up
   # Down   48 51
   # Up     63 90
# > 

# > c(90/(90 + 51), 48/(48 + 63))
# [1] 0.6382979 0.4324324
# > 

# > table(naive, test.y)
     # test.y
# naive Down  Up
   # Up  111 141
# > 

# > c(141/141, 0/111)
# [1] 1 0
# >

# ROC curves
# case = '+' , control = '-'

library(pROC)

roc.lda <- roc(test.y, lda.pred$posterior[, "Up"], levels = c("Down", "Up"))

# > roc.lda

# Call:
# roc.default(response = test.y, predictor = lda.pred$posterior[,     "Up"], levels = c("Down", "Up"))

# Data: lda.pred$posterior[, "Up"] in 111 controls (test.y Down) < 141 cases (test.y Up).
# Area under the curve: 0.5584
# > 

roc.qda <- roc(test.y, qda.pred$posterior[, "Up"], levels = c("Down", "Up"))

# > roc.qda

# Call:
# roc.default(response = test.y, predictor = qda.pred$posterior[,     "Up"], levels = c("Down", "Up"))

# Data: qda.pred$posterior[, "Up"] in 111 controls (test.y Down) < 141 cases (test.y Up).
# Area under the curve: 0.562
# > 

roc.knn <- roc(test.y, knn.prob, levels = c("Down", "Up"))

# > roc.knn

# Call:
# roc.default(response = test.y, predictor = knn.prob, levels = c("Down",     "Up"))

# Data: knn.prob in 111 controls (test.y Down) < 141 cases (test.y Up).
# Area under the curve: 0.5129
# > 


plot(roc.lda, legacy.axes = T)
plot(roc.qda, add = T, col = "blue")
plot(roc.knn, add = T, col = "purple")


