

# problem 2

admission.data <- read.csv(file.choose(), header = T)
attach(admission.data)


admit.data <- subset(admission.data, Group == 1)
reject.data <- subset(admission.data, Group == 2)
borderline.data <- subset(admission.data, Group == 3)

dim(admit.data)
dim(reject.data)
dim(borderline.data)

# Separating into train and test data
train.data <- rbind(admit.data[6:31,],reject.data[6:28,],borderline.data[6:26,])
train.X <- cbind(GPA=train.data$GPA, GMAT=train.data$GMAT)
train.y <- train.data$Group

test.data <- rbind(admit.data[1:5,],reject.data[1:5,],borderline.data[1:5,])
test.x <- cbind(GPA=test.data$GPA, GMAT=test.data$GMAT)
test.y <- test.data$Group
test.x


# a)
# Visualizing the structure of the dataset
str(train.data)
summary(train.data[,-3])

# Scatterplot of the variables
pairs(~. , data=train.data, main="Simple Scatterplot Matrix")

plot(train.X, xlab = "GPA", ylab = "GMAT", 
     pch = ifelse(train.y == 1, 1, ifelse(train.y == 2, 4, 16)))

cor(train.X[, 1], train.X[, 2])

par(mfrow = c(1, 2))
boxplot(train.X[, 1] ~ train.y, ylab = "GPA", xlab = "Group")
boxplot(train.X[, 2] ~ train.y, ylab = "GMAT", xlab = "Group")
par(mfrow = c(1, 1))


# b)

library(MASS)

LDA.fit <- lda(Group ~ GPA + GMAT, data = train.data)

# Predictions for test and training observations
LDA.pred <- predict(LDA.fit, test.data)
LDA.pred_train=predict(LDA.fit, train.data)


# Decision boundary (using the "blind" contour approach; test data)
# Set up a dense grid and compute posterior prob on the grid
n.grid <- 50
x1.grid <- seq(f = min(test.x[, 1]), t = max(test.x[, 1]), l = n.grid)
x2.grid <- seq(f = min(test.x[, 2]), t = max(test.x[, 2]), l = n.grid)
grid <- expand.grid(x1.grid, x2.grid)
colnames(grid) <- colnames(test.x)


pred.grid <- predict(LDA.fit, grid)
pred.grid


prob1 <- matrix(c(pred.grid$posterior[,1] - pmax(pred.grid$posterior[,2],
                  pred.grid$posterior[,3])), nrow = n.grid, ncol = n.grid)
prob2 <- matrix(c(pred.grid$posterior[,2] - pmax(pred.grid$posterior[,1],
                  pred.grid$posterior[,3])), nrow = n.grid, ncol = n.grid)


# ploting test data and decision boundaries
par(mfrow = c(1, 2))
plot(test.x, xlab = "GPA", ylab = "GMAT", main = "Test data and decision boundaries",
     pch = ifelse(test.y == 1, 1, ifelse(test.y == 2, 4, 16)))
contour(x1.grid, x2.grid, prob1, levels = 0, labels = "", xlab = "", ylab = "", 
        main = "", add = T)
contour(x1.grid, x2.grid, prob2, levels = 0, labels = "", xlab = "", ylab = "", 
        main = "", add = T)

plot(train.X, xlab = "GPA", ylab = "GMAT", main = "Training data and decision boundaries",
     pch = ifelse(train.y == 1, 1, ifelse(train.y == 2, 4, 16)))
contour(x1.grid, x2.grid, prob1, levels = 0, labels = "", xlab = "", ylab = "", 
        main = "", add = T)
contour(x1.grid, x2.grid, prob2, levels = 0, labels = "", xlab = "", ylab = "", 
        main = "", add = T)
par(mfrow = c(1, 1))

# confusion matrix for test and training data
table(LDA.pred$class, test.y)
table(LDA.pred_train$class, train.y)

mc.rate_test = 3/15
mc.rate_train = 6/70









# c)
QDA.fit <- qda(Group ~ GPA + GMAT, data = train.data)

# Predictions for test and training observations
QDA.pred <- predict(QDA.fit, test.data)
QDA.pred_train=predict(QDA.fit, train.data)



pred.grid <- predict(QDA.fit, grid)


prob1 <- matrix(c(pred.grid$posterior[,1] - pmax(pred.grid$posterior[,2],
                  pred.grid$posterior[,3])), nrow = n.grid, ncol = n.grid)
prob2 <- matrix(c(pred.grid$posterior[,2] - pmax(pred.grid$posterior[,1],
                  pred.grid$posterior[,3])), nrow = n.grid, ncol = n.grid)


# ploting test data and decision boundaries
par(mfrow = c(1, 2))
plot(test.x, xlab = "GPA", ylab = "GMAT", main = "Test data and decision boundaries",
     pch = ifelse(test.y == 1, 1, ifelse(test.y == 2, 4, 16)))
contour(x1.grid, x2.grid, prob1, levels = 0, labels = "", xlab = "", ylab = "", 
        main = "", add = T)
contour(x1.grid, x2.grid, prob2, levels = 0, labels = "", xlab = "", ylab = "", 
        main = "", add = T)

plot(train.X, xlab = "GPA", ylab = "GMAT", main = "Training data and decision boundaries",
     pch = ifelse(train.y == 1, 1, ifelse(train.y == 2, 4, 16)))
contour(x1.grid, x2.grid, prob1, levels = 0, labels = "", xlab = "", ylab = "", 
        main = "", add = T)
contour(x1.grid, x2.grid, prob2, levels = 0, labels = "", xlab = "", ylab = "", 
        main = "", add = T)
par(mfrow = c(1, 1))

# confusion matrix for test and training data
table(QDA.pred$class, test.y)
table(QDA.pred_train$class, train.y)

mc.rate_test = 1/15
mc.rate_train = 2/70








