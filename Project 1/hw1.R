library(class)


training.data <- read.csv(file.choose(), header = T)
test.data <- read.csv(file.choose(), header = T)

set.seed(1)
train.X <- cbind(training.data$x.1, training.data$x.2)
train.Y <- training.data$y
test.X <- cbind(test.data$x.1, test.data$x.2)
test.Y <- test.data$y
mod.test <- knn(train.X, test.X, train.Y, k = 1)
table(mod.test, test.Y)
mean(mod.test != test.Y)



ks <- seq(1, 496, by = 1)
nks <- length(ks)
err.rate.train <- numeric(length = nks)
err.rate.test <- numeric(length = nks)
names(err.rate.train) <- names(err.rate.test) <- ks

for (i in seq(along = ks)) {
  set.seed(1)
  mod.train <- knn(train.X, train.X, train.Y, k = ks[i])
  set.seed(1)
  mod.test <- knn(train.X, test.X, train.Y, k = ks[i])
  err.rate.train[i] <- mean(mod.train != train.Y)
  err.rate.test[i] <- mean(mod.test != test.Y)
}

plot(ks, err.rate.train, xlab = "Number of nearest neighbors", ylab = "Error rate", type = "b", ylim = range(c(err.rate.train, err.rate.test)), col = "blue", pch = 20)
lines(ks, err.rate.test, type="b", col="purple", pch = 20)
legend("bottomright", lty = 1, col = c("blue", "purple"), legend = c("training", "test"))


result <- data.frame(ks, err.rate.train, err.rate.test)
optimal.point <- result[err.rate.test == min(result$err.rate.test), ]



n.grid <- 200
x1.grid <- seq(f = min(train.X[, 1]), t = max(train.X[, 1]), l = n.grid)
x2.grid <- seq(f = min(train.X[, 2]), t = max(train.X[, 2]), l = n.grid)
grid <- expand.grid(x1.grid, x2.grid)

k.opt <- optimal.point[1,1] # 256
set.seed(1)
mod.opt <- knn(train.X, grid, train.Y, k = k.opt, prob = T)
prob <- attr(mod.opt, "prob") # prob is voting fraction for winning class
prob <- ifelse(mod.opt == "yes", prob, 1 - prob) # now it is voting fraction for Direction == "Up"
prob <- matrix(prob, n.grid, n.grid)

plot(train.X, col = ifelse(train.Y == "yes", "green", "red"))
contour(x1.grid, x2.grid, prob, levels = 0.5, labels = "", xlab = "", ylab = "", main = "", add = T)


k.opt <- optimal.point[2,1] # 276
set.seed(1)
mod.opt <- knn(train.X, grid, train.Y, k = k.opt, prob = T)
prob <- attr(mod.opt, "prob") # prob is voting fraction for winning class
prob <- ifelse(mod.opt == "yes", prob, 1 - prob) # now it is voting fraction for Direction == "Up"
prob <- matrix(prob, n.grid, n.grid)

plot(train.X, col = ifelse(train.Y == "yes", "green", "red"))
contour(x1.grid, x2.grid, prob, levels = 0.5, labels = "", xlab = "", ylab = "", main = "", add = T)














library(class)
library(keras)
cifar <- dataset_cifar10()
str(cifar)
x.train <- cifar$train$x
y.train <- cifar$train$y
x.test <- cifar$test$x
y.test <- cifar$test$y
# reshape the images as vectors (column-wise)
# (aka flatten or convert into wide format)
# (for row-wise reshaping, see ?array_reshape)
dim(x.train) <- c(nrow(x.train), 32*32*3) # 50000 x 3072
dim(x.test) <- c(nrow(x.test), 32*32*3) # 50000 x 3072
# rescale the x to lie between 0 and 1
x.train <- x.train/255
x.test <- x.test/255
# categorize the response
y.train <- as.factor(y.train)
y.test <- as.factor(y.test)
# randomly sample 1/5 of the data to reduce computing time
set.seed(1)
id.train <- sample(1:50000, 10000)
id.test <- sample(1:10000, 2000)
x.train <- x.train[id.train,]
y.train <- y.train[id.train]
x.test <- x.test[id.test,]
y.test <- y.test[id.test]



mod.test <- knn(x.train, x.test, y.train, k = 1)
table(mod.test, y.test)
mean(mod.test != y.test)


ks <- c(50, seq(100, 400, by = 100))
nks <- length(ks)
err.rate.test <- numeric(length = nks)
names(err.rate.test) <- ks

for (i in seq(along = ks)) {
  set.seed(1)
  mod.test <- knn(x.train, x.test, y.train, k = ks[i])
  err.rate.test[i] <- mean(mod.test != y.test)
}

table(mod.test, y.test)


plot(ks, err.rate.test, xlab = "Number of nearest neighbors", ylab = "Test error rate", type = "b", ylim = range(c(err.rate.test)), col = "blue", pch = 20)


result <- data.frame(ks, err.rate.train, err.rate.test)
result[err.rate.test == min(result$err.rate.test), ]





i=4
set.seed(1)
mod.test <- knn(x.train, x.test, y.train, k = ks[i])
err.rate.test[i] <- mean(mod.test != y.test)
err.rate.test

i=5
set.seed(1)
mod.test <- knn(x.train, x.test, y.train, k = ks[i])
err.rate.test[i] <- mean(mod.test != y.test)
err.rate.test


mod.test <- knn(x.train, x.test, y.train, k = 50)
table(mod.test, y.test)


