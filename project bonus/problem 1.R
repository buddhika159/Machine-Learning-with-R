

# problem 1

library(e1071) # for SVM


admission.data <- read.csv('admission.csv', header = T)
attach(admission.data)
str(admission.data)


test <- rbind(admission.data[Group==1,][1:5,],
              admission.data[Group==2,][1:5,],
              admission.data[Group==3,][1:5,])

train <- rbind(admission.data[Group==1,][-(1:5),],
               admission.data[Group==2,][-(1:5),],
               admission.data[Group==3,][-(1:5),])


plot(train[,1:2], xlab = "GPA", ylab = "GMAT", 
     col = train$Group, pch=19)


# part a)


set.seed(1)
tune.out.a <- tune(svm, as.factor(Group) ~ ., data = train, kernel = "linear", 
                 cross = 10, ranges = list(cost = seq(0.01,0.5,0.01)))




summary(tune.out.a)
# Parameter tuning of 'svm':
#   
#   - sampling method: 10-fold cross validation 
# 
# - best parameters:
#   cost
# 0.1
# 
# - best performance: 0.05714286

bestmod.a <- tune.out.a$best.model
summary(bestmod.a)
# Call:
#   best.tune(method = svm, train.x = as.factor(Group) ~ ., data = train, ranges = list(cost = seq(0.01,0.5,0.01)), kernel = "linear", 
#             cross = 10)
# 
# 
# Parameters:
#   SVM-Type:  C-classification 
# SVM-Kernel:  linear 
# cost:  0.1 
# 
# Number of Support Vectors:  50
# 
# ( 13 17 20 )
# 
# 
# Number of Classes:  3 
# 
# Levels: 
#   1 2 3
# 
# 10-fold cross-validation on training data:
#   
#   Total Accuracy: 91.42857 
# Single Accuracies:
#   100 100 85.71429 100 85.71429 100 71.42857 85.71429 100 85.71429 

plot(bestmod.a, train, main = 'SVM classification on training set')
table(true = train$Group, pred = predict(bestmod.a, newdata = train[,1:2]))
#    pred
# true  1  2  3
#    1 24  0  2
#    2  0 21  2
#    3  0  0 21


plot(bestmod.a, test, main = 'SVM classification on test set')
table(true = test$Group, pred = predict(bestmod.a, newdata = test[,1:2]))
#   pred
# true 1 2 3
#    1 4 0 1
#    2 0 5 0
#    3 0 0 5





# part b)

set.seed(1)
tune.out.b <- tune(svm, as.factor(Group) ~ ., data = train, kernel = "polynomial", degree = 2,
                 cross = 10, ranges = list(cost = seq(70,80,0.1)))




summary(tune.out.b)
# Parameter tuning of 'svm':
#   
#   - sampling method: 10-fold cross validation 
# 
# - best parameters:
#   cost
# 73.3
# 
# - best performance: 0.3142857 

bestmod.b <- tune.out.b$best.model
summary(bestmod.b)
# Call:
#   best.tune(method = svm, train.x = as.factor(Group) ~ ., data = train, ranges = list(cost = seq(0.1, 100, 0.1)), kernel = "polynomial", 
#             degree = 2, cross = 10)
# 
# 
# Parameters:
#   SVM-Type:  C-classification 
# SVM-Kernel:  polynomial 
# cost:  73.3 
# degree:  2 
# coef.0:  0 
# 
# Number of Support Vectors:  46
# 
# ( 20 20 6 )
# 
# 
# Number of Classes:  3 
# 
# Levels: 
#   1 2 3
# 
# 10-fold cross-validation on training data:
#   
#   Total Accuracy: 65.71429 
# Single Accuracies:
#   57.14286 57.14286 85.71429 71.42857 57.14286 85.71429 42.85714 85.71429 57.14286 57.14286 

plot(bestmod.b, train, main = 'SVM classification on training set')
table(true = train$Group, pred = predict(bestmod.b, newdata = train[,1:2]))
#     pred
# true  1  2  3
#    1 21  3  2
#    2 13 10  0
#    3  2  1 18


plot(bestmod.b, test, main = 'SVM classification on test set')
table(true = test$Group, pred = predict(bestmod.b, newdata = test[,1:2]))
#   pred
# true 1 2 3
#    1 1 1 3
#    2 2 3 0
#    3 0 0 5






# part c)

set.seed(1)
tune.out.c <- tune(svm, as.factor(Group) ~ ., data = train, kernel = "radial",
                   cross = 10, ranges = list(cost = seq(10,15,0.1), gamma = seq(0.1,1,0.1)))




summary(tune.out.c)
# Parameter tuning of 'svm':
#   
#   - sampling method: 10-fold cross validation 
# 
# - best parameters:
#   cost gamma
# 12.5   0.8
# 
# - best performance: 0.04285714 


bestmod.c <- tune.out.c$best.model
summary(bestmod.c)
# Call:
#   best.tune(method = svm, train.x = as.factor(Group) ~ ., data = train, ranges = list(cost = seq(10, 15, 0.1), gamma = seq(0.1, 
#                                                                                                                            1, 0.1)), kernel = "radial", cross = 10)
# 
# 
# Parameters:
#   SVM-Type:  C-classification 
# SVM-Kernel:  radial 
# cost:  12.5 
# 
# Number of Support Vectors:  20
# 
# ( 8 6 6 )
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
#   100 100 100 85.71429 85.71429 85.71429 100 100 100 100

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





















