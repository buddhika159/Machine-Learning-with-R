library(ISLR)
library(class)
summary(Smarket)
attach(Smarket)

head(Smarket)

# 
# # class labels: simple distance from origin
# classes <- ifelse(x^2 + y^2 > 60^2, "blue", "orange")
# classes.test <- ifelse(x.test^2 + y.test^2 > 60^2, "blue", "orange")
# 
# grid <- expand.grid(x=1:100, y=1:100)
# classes.grid <- knn(train.df, grid, classes, k=25, prob=TRUE)  # note last argument
# prob.grid <- attr(classes.grid, "prob")
# prob.grid <- ifelse(classes.grid == "blue", prob.grid, 1 - prob.grid)
# 
# # plot the boundary
# contour(x=1:100, y=1:100, z=matrix(prob.grid, nrow=100), levels=0.5,
#         col="grey", drawlabels=FALSE, lwd=2)
# # add points from test dataset
# points(test.df, col=classes.test)

