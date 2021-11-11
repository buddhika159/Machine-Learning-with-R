library(caret)

g.credit = read.csv("germancredit.csv")
y.train = g.credit$Default
##KNN
default_knn_mod = train(
  y.train ~ .,
  data = g.credit,
  method = "knn",
  trControl = trainControl(method = "LOOCV"),
  tuneGrid = expand.grid(k = seq(1, 101, by = 2))
)
head(default_knn_mod$results, 5)
plot(default_knn_mod)



set.seed(1)
fit.full.KNN <- train(as.factor(Default) ~ .,
                      method     = "knn",
                      tuneGrid   = expand.grid(k = 1:100),
                      trControl  = trainControl(method="LOOCV"),
                      data       = german_credit)




























