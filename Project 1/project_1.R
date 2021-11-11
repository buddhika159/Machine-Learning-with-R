library(ISLR)
library(class)
library(readr)
X1_training_data <- read_csv("C:/Users/bcj170130/Dropbox/UT Dallas/courses/2020 Spring/STAT 6340 Statistical and Machine Learning/Projects/1-training_data.csv")
X1_test_data <- read_csv("C:/Users/bcj170130/Dropbox/UT Dallas/courses/2020 Spring/STAT 6340 Statistical and Machine Learning/Projects/1-test_data.csv")


data_training$y[data_training$y == "No"] <- 0
data_training$y[data_training$y == "Yes"] <- 1
data_set$y[data_set$y == "No"] <- 0
data_set$y[data_set$y == "Yes"] <- 1

x.train = data_training[c(1,2)]
y.train =data_training[c(3)]
x.test = data_test[c(1,2)]
y.test = data_test[c(3)]


KNN_algorithm <- knn(x.train, x.test, y.train[,1, drop=TRUE], k = 20, prob=FALSE)



plot(data_test[c(1,2)], col=KNN_algorithm)
# KNN_algorithm_new <- ifelse(KNN_algorithm=="Yes", 1, 2)
KNN_algorithm[KNN_algorithm == "yes"] <- 1
KNN_algorithm[KNN_algorithm == "no"] <- 2
z <- matrix(KNN_algorithm_new, nrow=100)


contour(x=seq(-1,3, length.out = nrow(z)), y=seq(-1,3, length.out = ncol(z)), z, levels=0.5,
        col="grey", drawlabels=FALSE, lwd=2)
print(ncol(z))

mean(KNN_algorithm != y.test)

str(y.test)
str(KNN_algorithm)
