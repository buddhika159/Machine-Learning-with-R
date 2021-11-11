# problem 2

library(ISLR)

train.data <- College
str(train.data)
attach(train.data)


# part a)

library(tree)
tree.College <- tree(Apps ~ ., train.data)
# node), split, n, deviance, yval
#       * denotes terminal node
# 
#   1) root 777 1.162e+10  3002.0  
#     2) Accept < 3439.5 651 1.732e+09  1736.0  
#       4) Accept < 1927 532 3.251e+08  1182.0  
#         8) Accept < 955.5 335 3.457e+07   708.3 *
#         9) Accept > 955.5 197 8.769e+07  1987.0 *
#       5) Accept > 1927 119 5.130e+08  4213.0  
#        10) Top10perc < 86.5 113 1.883e+08  3859.0 *
#        11) Top10perc > 86.5 6 4.255e+07 10900.0 *
#     3) Accept > 3439.5 126 3.461e+09  9541.0  
#       6) Accept < 8061.5 101 6.351e+08  7770.0  
#        12) Accept < 5162.5 53 1.439e+08  6340.0 *
#        13) Accept > 5162.5 48 2.632e+08  9349.0 *
#       7) Accept > 8061.5 25 1.229e+09 16700.0  
#        14) Accept < 12973.5 20 1.172e+08 14630.0 *
#        15) Accept > 12973.5 5 6.849e+08 24960.0 *

summary(tree.College)
# Regression tree:
# tree(formula = Apps ~ ., data = train.data)
# Variables actually used in tree construction:
# [1] "Accept"    "Top10perc"
# Number of terminal nodes:  8 
# Residual mean deviance:  2031000 = 1.562e+09 / 769 
# Distribution of residuals:
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -8371.0  -429.3  -104.3     0.0   286.7 23140.0 


plot(tree.College)
text(tree.College, pretty = 0, cex = 0.7)



# part b)

cv.College <- cv.tree(tree.College, FUN = prune.tree, K=777)
# $size
# [1] 8 7 6 5 4 3 2 1
# 
# $dev
# [1]  2381430909  2667272688  3129789615  3196896546  3251213689
# [6]  4065549912  5778625446 12478016777
# 
# $k
# [1]       -Inf  202873515  228050501  282105525  426616632  893815939
# [7] 1596851915 6430760151
# 
# $method
# [1] "deviance"
# 
# attr(,"class")
# [1] "prune"         "tree.sequence"

plot(cv.College$size, cv.College$dev, type = "b")

# get the best size
cv.College$size[which.min(cv.College$dev)]
# [1] 8

prune.College <- prune.tree(tree.College, best = 8)
summary(prune.College)

# Regression tree:
# tree(formula = Apps ~ ., data = train.data)
# Variables actually used in tree construction:
# [1] "Accept"    "Top10perc"
# Number of terminal nodes:  8 
# Residual mean deviance:  2031000 = 1.562e+09 / 769 
# Distribution of residuals:
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -8371.0  -429.3  -104.3     0.0   286.7 23140.0



plot(prune.College)
text(prune.College, pretty = 0)





# part c)

library(randomForest)
set.seed(1)
bag.College <- randomForest(Apps ~ ., data = College, mtry = 17, 
                            ntree = 1000, importance = TRUE)
# Call:
#   randomForest(formula = Apps ~ ., data = College, mtry = 17, ntree = 1000,      importance = TRUE) 
# Type of random forest: regression
# Number of trees: 1000
# No. of variables tried at each split: 17
# 
# Mean of squared residuals: 2124209
# % Var explained: 85.8

bag.imp <- data.frame(bag.College$importance)
bag.imp[order(bag.imp$IncNodePurity, decreasing = T),]
#                  X.IncMSE IncNodePurity
# Accept      22016845.6518   10598466352
# Top10perc     349136.3838     286343989
# Enroll        295881.2979     245209919
# Top25perc     188104.5242     161109658
# Expend        122938.0253      88231957


plot(bag.College, main = "Bagging model")


# part d)

set.seed(1)
forest.College <- randomForest(Apps ~ ., data = College, mtry = 6, 
                            ntree = 1000, importance = TRUE)
# Call:
#   randomForest(formula = Apps ~ ., data = College, mtry = 6, ntree = 1000,      importance = TRUE) 
# Type of random forest: regression
# Number of trees: 1000
# No. of variables tried at each split: 6
# 
# Mean of squared residuals: 2213661
# % Var explained: 85.2

forest.imp <- data.frame(forest.College$importance)
forest.imp[order(forest.imp$IncNodePurity, decreasing = T),]
#               X.IncMSE IncNodePurity
# Accept      9357061.39    4787760962
# Enroll      3622110.84    2533886881
# F.Undergrad 2260502.82    1745082241
# P.Undergrad  178029.66     369345190
# Top25perc    302858.86     320552183


plot(forest.College, main = "Random Forest model")


# part e)
library(gbm)

set.seed(1)
boost.College <- gbm(Apps ~ ., data = College, distribution = "gaussian", 
                    n.trees = 1000, interaction.depth = 1, shrinkage = 0.01, cv.folds = 777)
# gbm(formula = Apps ~ ., distribution = "gaussian", data = College, 
#     n.trees = 1000, interaction.depth = 1, shrinkage = 0.01, 
#     cv.folds = 777)
# A gradient boosted model with gaussian loss function.
# 1000 iterations were performed.
# The best cross-validation iteration was 999.
# There were 17 predictors of which 14 had non-zero influence.


summary(boost.College)
# var     rel.inf
# Accept           Accept 78.79257671
# Enroll           Enroll  7.55712464
# F.Undergrad F.Undergrad  4.68956879
# Top10perc     Top10perc  4.22216126
# Top25perc     Top25perc  2.01856968
# P.Undergrad P.Undergrad  0.88111834


mean(boost.College$cv.error)
# [1] 3724476


