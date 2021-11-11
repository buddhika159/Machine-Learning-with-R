

# problem 2

library(e1071) # for SVM


wine <- read.csv("winequality-white.csv", header = T, sep=';')
wine$quality <- ifelse(wine$quality >= 7, 1, 0)
wine$quality <- as.factor(wine$quality)
attach(wine)
str(wine)


# part a)

set.seed(1)
tune.out.a <- tune(svm, quality ~ ., data = wine, kernel = "linear", cross = 10, 
                   ranges = list(cost = seq(15,25)), scale = T)

# 0.1


summary(tune.out.a)
# - sampling method: 10-fold cross validation 
# 
# - best parameters:
#   cost
# 0.1
# 
# - best performance: 0.2164154

bestmod.a <- tune.out.a$best.model
summary(bestmod.a)
# Call:
#   best.tune(method = svm, train.x = quality ~ ., data = wine, ranges = list(cost = seq(0.1, 
#                                                                                        1, 0.1)), kernel = "linear", cross = 10, scale = T)
# 
# 
# Parameters:
#   SVM-Type:  C-classification 
# SVM-Kernel:  linear 
# cost:  0.1 
# 
# Number of Support Vectors:  2218
# 
# ( 1158 1060 )
# 
# 
# Number of Classes:  2 
# 
# Levels: 
#   0 1
# 
# 10-fold cross-validation on training data:
#   
#   Total Accuracy: 78.35851 
# Single Accuracies:
#   75.86912 77.14286 78.16327 80.40816 76.73469 79.5501 78.16327 76.53061 80.40816 80.61224 


table(true = quality, pred = predict(bestmod.a, newdata = wine[,-12]))
#     pred
# true  1  2  3
#    1 25  0  1
#    2  0 23  0
#    3  1  2 18







# part b)

set.seed(1)
tune.out.b <- tune(svm, quality ~ ., data = wine, kernel = "polynomial", degree = 2,
                   cross = 10, ranges = list(cost = seq(8.5,9.5,0.1)), scale = T)


# 8.9

summary(tune.out.b)
# Parameter tuning of 'svm':
#   
#   - sampling method: 10-fold cross validation 
# 
# - best parameters:
#   cost
# 8.9
# 
# - best performance: 0.1872213 


bestmod.b <- tune.out.b$best.model
summary(bestmod.b)
# Call:
#   best.tune(method = svm, train.x = quality ~ ., data = wine, ranges = list(cost = seq(8.5, 
#                                                                                        9.5, 0.1)), kernel = "polynomial", degree = 2, cross = 10, scale = T)
# 
# 
# Parameters:
#   SVM-Type:  C-classification 
# SVM-Kernel:  polynomial 
# cost:  8.9 
# degree:  2 
# coef.0:  0 
# 
# Number of Support Vectors:  2078
# 
# ( 1061 1017 )
# 
# 
# Number of Classes:  2 
# 
# Levels: 
#   0 1
# 
# 10-fold cross-validation on training data:
#   
#   Total Accuracy: 81.4414 
# Single Accuracies:
#   81.39059 82.2449 80.40816 81.22449 82.04082 78.11861 81.22449 84.89796 79.59184 83.26531 

table(true = quality, pred = predict(bestmod.b, newdata = wine[,-12]))
#     pred
# true    0    1
#    0 3766   72
#    1  818  242


3766   72



# part c)

set.seed(1)
tune.out.c <- tune(svm, quality ~ ., data = wine, kernel = "radial",
                   cross = 10, ranges = list(cost = 1:100, gamma = seq(1,100)))




summary(tune.out.c)
# Parameter tuning of 'svm':
#   
#   - sampling method: 10-fold cross validation 
# 
# - best parameters:
#   cost gamma
# 15     1
# 
# - best performance: 0.04285714 


bestmod.c <- tune.out.c$best.model
summary(bestmod.c)
# Call:
#   best.tune(method = svm, train.x = as.factor(Group) ~ ., data = train, 
#             ranges = list(cost = 15:25, gamma = seq(0, 3, 0.5)), kernel = "radial", 
#             cross = 10)
# 
# 
# Parameters:
#   SVM-Type:  C-classification 
# SVM-Kernel:  radial 
# cost:  15 
# 
# Number of Support Vectors:  21
# 
# ( 8 7 6 )
# 
# 
# Number of Classes:  3 
# 
# Levels: 
#   1 2 3
# 
# 10-fold cross-validation on training data:
#   
#   Total Accuracy: 95.71429 
# Single Accuracies:
#   85.71429 85.71429 100 100 85.71429 100 100 100 100 100 

plot(bestmod.c, train, main = 'SVM classification on training set')
table(true = train$Group, pred = predict(bestmod.c, newdata = train[,1:2]))
#     pred
# true  1  2  3
#    1 26  0  0
#    2  0 23  0
#    3  0  0 21


plot(bestmod.c, test, main = 'SVM classification on test set')
table(true = test$Group, pred = predict(bestmod.c, newdata = test[,1:2]))
#    pred
# true 1 2 3
#    1 4 0 1
#    2 0 5 0
#    3 0 0 5





















