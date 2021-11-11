# problem 2 

train.data <- read.csv("nyt.train.csv", header = T)
test.data <- read.csv("nyt.test.csv", header = T)

train.pca <- prcomp(train.data[, -1]) 
# We need to omit the first column because it contains categorical variables, and PCA doesn't apply to them.
train.latent.sem <- train.pca$rotation
signif(sort(train.latent.sem[, 1], decreasing = TRUE)[1:30], 2)
#     theater        music            m     theaters    composers      matinee        opera 
#       0.180        0.130        0.080        0.079        0.078        0.077        0.075 
#      sunday      musical       jersey            p    orchestra         band    committee 
#       0.067        0.065        0.064        0.064        0.062        0.061        0.060 
# performance performances         east        organ        dance         hour      program 
#       0.059        0.056        0.056        0.053        0.052        0.051        0.051 
#      events    yesterday         will     recitals       ballet     purchase          X.d 
#       0.050        0.049        0.049        0.048        0.048        0.048        0.047 
#   guitarist        calif 
#       0.045        0.044 

signif(sort(train.latent.sem[, 1], decreasing = FALSE)[1:30], 2)
#    her         she          ms    painting   paintings      mother      cooper     artists 
# -0.150      -0.140      -0.130      -0.110      -0.100      -0.092      -0.090      -0.086 
#  white      images           i        said     process   sculpture     picasso    gagosian 
# -0.078      -0.077      -0.071      -0.070      -0.070      -0.070      -0.068      -0.065 
#    art          my      nature       image       color  sculptures        work         red 
# -0.064      -0.064      -0.064      -0.061      -0.061      -0.059      -0.059      -0.058 
# artist      rothko       paint photographs       paper      figure 
# -0.056      -0.055      -0.055      -0.055      -0.054      -0.054 


signif(sort(train.latent.sem[, 2], decreasing = TRUE)[1:30], 2)
#   she        her    theater       said          i         ms     mother     cooper 
# 0.240      0.240      0.200      0.170      0.120      0.110      0.110      0.110 
#  says      opera         my       hour         id         im production        was 
# 0.089      0.084      0.084      0.082      0.081      0.079      0.075      0.075 
#   mrs       play        sir   broadway     awards        you   national      garde 
# 0.074      0.074      0.071      0.070      0.066      0.066      0.065      0.063 
#    me     season   jonathan       week       baby   networks 
# 0.062      0.062      0.062      0.060      0.059      0.059 


signif(sort(train.latent.sem[, 2], decreasing = FALSE)[1:30], 2)
#   patterns      chinese          feb      chelsea    computers      diamond    europeans 
#     -0.065       -0.051       -0.046       -0.046       -0.046       -0.045       -0.044 
#    gallery       museum          art        heads        white        stone        views 
#     -0.042       -0.041       -0.040       -0.039       -0.039       -0.039       -0.039 
#    painted    recalling         soho      artists        pills       statue       newman 
#     -0.039       -0.039       -0.039       -0.038       -0.037       -0.037       -0.037 
#   computer compositions         grid   landscapes      spatial       images         wood 
#     -0.037       -0.037       -0.037       -0.037       -0.037       -0.036       -0.035 
# technology     personal 
#     -0.035       -0.035

plot(train.pca$x[, 1:2], 
     pch = ifelse(train.data[, "class.labels"] == "music", "m", "a"),
     col = ifelse(train.data[, "class.labels"] == "music", "blue","red"))




# part c)
train.set <- data.frame(class.labels=train.data$class.labels,
                              CP1 = train.pca$x[, 1],
                              CP2 = train.pca$x[, 2])
predictors <- train.pca$x[, 1:2]
lm.fit <- glm(class.labels ~ CP1 + CP2, data = train.set, family = "binomial")

lm.pred <- predict(lm.fit, train.set, type = 'response')
predicted.classes <- as.factor(ifelse(lm.pred < 0.5, 'art', 'music'))
mean(train.data$class.labels != predicted.classes) # training error
# [1] 0.1625



slope <- coef(lm.fit)[2]/(-coef(lm.fit)[3])
intercept <- coef(lm.fit)[1]/(-coef(lm.fit)[3]) 

abline(intercept , slope)


# part d)
pca.scores <- predict(train.pca, test.data)[,1:2]
test.set <- cbind.data.frame(class.labels = test.data$class.labels, 
                            CP1 = pca.scores[, 1],
                            CP2 = pca.scores[, 2])



plot(pca.scores[, 1:2], 
     pch = ifelse(test.data[, "class.labels"] == "music", "m", "a"),
     col = ifelse(test.data[, "class.labels"] == "music", "blue","red"))

test.lm.pred <- predict(lm.fit, test.set , type = 'response')
test.predicted.classes <- as.factor(ifelse(test.lm.pred < 0.5, 'art', 'music'))
mean(test.data$class.labels != test.predicted.classes) # test error
# [1] 0.3181818
abline(intercept , slope)

# confusion matrix for test and training data
table(test.data$class.labels, test.predicted.classes)

