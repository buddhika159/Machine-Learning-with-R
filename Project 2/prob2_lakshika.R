# Importing the dataset
admission_data <- read.csv(file.choose(), header = T)
admission_data=admission_data[,1:3]
# extracting training data
attach(admission_data)
#seperating the test dataset and training dataset
test_data1= admission_data[(length(Group[Group==1])-4):(length(Group[Group==1])),]
test_data2=admission_data[(length(Group[Group==2|Group==1])-4):(length(Group[Group==2| Group==1])),]
test_data3=admission_data[(length(Group[Group==3|Group==2|Group==1])-4):(length(Group[Group==3|Group==2|Group==1])),]

test_data=rbind.data.frame(test_data1,test_data2,test_data3)
A=as.vector(as.numeric(row.names(test_data)))
training_data=admission_data[-A,]

####part a)###
#plotting the scatter plot of traning data
plot(training_data$GPA,training_data$GMAT,col=c("red","blue","green")[training_data$Group],main="Scatter plot of Training Data")
par(xpd = TRUE)
legend(x=3.5,y=300, legend = levels(factor(training_data$Group)), col=c("red","blue","green"), pch=1,bty = "n", xpd = NA)
cor(training_data$GPA,training_data$GMAT)

#plotting the group specific boxplots of training data predictors
par(mfrow=c(1,2))
boxplot(training_data$GPA~training_data$Group, ylab="GPA")
boxplot(training_data$GMAT~training_data$Group, ylab="GMAT")

###part b)####

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

lda_fit_new=s6340.lda(factor(training_data$Group),training_data[1:2])
lda_fit_new

#fitting the LDA
library(MASS)
lda.fit <- lda(training_data$Group ~ GPA + GMAT, data = training_data)
lda.fit

#Predictions for test observations
lda.pred=predict(lda.fit, test_data)
lda.pred
#confusion matrix for test data 
table(lda.pred$class, test_data$Group)
#overall misclassification rate test data
3/15
#Predictions for training observations
lda.pred_tr=predict(lda.fit, training_data)
lda.pred_tr
#confusion matrix for training data 
table(lda.pred_tr$class, training_data$Group)
#overall misclassification rate training data
3/70

##Decision Boundary###
# making a fine grid of x values
n.grid <- 50
x1.grid <- seq(f = min(test_data[, 1]), t = max(test_data[, 1]), l = n.grid)
x2.grid <- seq(f = min(test_data[, 2]), t = max(test_data[, 2]), l = n.grid)
grid <- expand.grid(x1.grid, x2.grid)
colnames(grid) <- colnames(test_data[,1:2])

pred.grid <- predict(lda.fit, grid)

p1=matrix(c(pred.grid$posterior[,1]-pmax(pred.grid$posterior[,2],pred.grid$posterior[,3])),nrow = n.grid,ncol=n.grid)
p2=matrix(c(pred.grid$posterior[,2]-pmax(pred.grid$posterior[,1],pred.grid$posterior[,3])),nrow = n.grid,ncol=n.grid)

plot(test_data$GPA,test_data$GMAT,pch=c(0,16,21)[test_data$Group])
par(xpd = TRUE)
legend(x=3.5,y=340, legend = levels(factor(training_data$Group)),pch=c(0,16,21) ,bty = "n", xpd = NA,horiz = T)

contour(x1.grid, x2.grid, p1 , levels = 0,  labels = "", xlab = "", ylab = "", 
        main = "", add = T)

contour(x1.grid, x2.grid, p2 , levels = 0,  labels = "", xlab = "", ylab = "", 
        main = "", add = T)
dev.off()

