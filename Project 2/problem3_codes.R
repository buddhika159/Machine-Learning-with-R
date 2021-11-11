
# problem 3

# Import data
credit.data <- read.csv(file.choose(), header = T)
attach(credit.data)

# Seperate data into respond and prodictors
train.y <- credit.data[,1]
train.x <- credit.data[,2:21]




# a)

library(MASS) # Need for LDA and QDA

# Fit LDA for the training data
LDA.fit <- lda(Default ~. , data = credit.data)

# Predictions for training data
LDA.pred <- predict(LDA.fit, credit.data)

# Confusion matrix for LDA
table(LDA.pred$class, Default)
# sensitivity = 162/(138+162) = 162/300
# specificity = 615/(615+85) = 615/700
# mc.rate = (85+138)/(615+85+138+162) = 223/1000



# install.packages("pROC")
library(pROC) # Need for roc

# Calculating and Plotting ROC curve for LDA 
roc.LDA <- roc(train.y, LDA.pred$posterior[, 1], levels = c(0, 1))
plot(roc.LDA, legacy.axes = T)



# b)
# Fit QDA for the training data
QDA.fit <- qda(Default ~. , data = credit.data)

# Predictions for traning data
QDA.pred <- predict(QDA.fit, credit.data)

# Confusion matrix for QDA
table(QDA.pred$class, Default)
# sensitivity = 230/(70+230) = 230/300
# specificity = 593/(593+107) = 593/700
# mc.rate = (107+70)/(593+107+70+230) = 177/1000

# Calculating and Plotting ROC curve for QDA 
roc.QDA <- roc(train.y, QDA.pred$posterior[, 1], levels = c(0, 1))
plot(roc.QDA, legacy.axes = T)




# c)
# Plotting ROC curve for LDA and QDA 
plot(roc.LDA, legacy.axes = T)
plot(roc.QDA, add = T, col = "green")
legend(0.2, 0.3, legend=c("LDA", "QDA"), col=c("black", "green"), lty=1:2, cex=0.8)






