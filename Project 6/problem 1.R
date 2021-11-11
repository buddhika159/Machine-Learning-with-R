# problem 1


planet <- read.csv("planet.csv", header = T)
attach(planet)

head(planet)


summary(planet)
#      Mass            Period          Eccentricity   
# Min.   : 0.050   Min.   :   2.985   Min.   :0.0000  
# 1st Qu.: 0.930   1st Qu.:  44.280   1st Qu.:0.1000  
# Median : 1.760   Median : 337.110   Median :0.2700  
# Mean   : 3.327   Mean   : 666.531   Mean   :0.2815  
# 3rd Qu.: 4.140   3rd Qu.:1089.000   3rd Qu.:0.4100  
# Max.   :17.500   Max.   :5360.000   Max.   :0.9270 

library(corrplot)

plot(planet, main="Scatterplot", pch=19)
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(cor(planet), method="color", col = col(200),  
         type="upper",
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, sig.level = 0.01, insig = "blank")


# part b)

# standardizing the data
planet.scaled <- scale(planet)

apply(planet.scaled, 2, mean)
apply(planet.scaled, 2, sd)


par(mfrow=c(1,2))
boxplot(planet, col = rainbow(3, s = 0.5), main="Original variables")
boxplot(planet.scaled, col = rainbow(3, s = 0.5), main="Scaled variables")
par(mfrow=c(1,1))



# part d)

# hierarchical cluster using complete linkage and Euclidean distance.

hc.complete <- hclust(dist(planet.scaled), method = "complete")

plot(hc.complete, main = "Complete Linkage", xlab = "", sub = "", 
     cex = 0.7)

hc.clusters <- cutree(hc.complete, 3)
#  [1] 1 1 2 2 1 1 1 2 1 1 1 2 1 2 1 1 1 2 1 2 2 1 2 1 1 1 1 2 2 2 2 2 1 2
# [35] 2 2 1 1 2 2 1 1 2 1 2 2 2 2 2 1 2 1 1 2 2 1 2 2 1 1 1 2 2 2 2 2 1 2
# [69] 2 1 2 2 1 3 2 1 2 2 2 2 2 1 1 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2


a <- cbind(planet,hc.clusters)
aggregate(a[, 1:3], list(a$hc.clusters), mean)
#   Group.1     Mass    Period Eccentricity
# 1       1 1.703316  488.7479   0.07077105
# 2       2 4.311774  699.7941   0.41269355
# 3       3 4.000000 5360.0000   0.16000000

library(scatterplot3d)
scatterplot3d(Mass, Period, Eccentricity, color = (hc.clusters + 1), main="3D Scatterplot", pch = 16)
plot(planet, main="Scatterplot of 3 clusters", col = (hc.clusters + 1), pch=19)



# part e)
set.seed(3)
km.out <- kmeans(planet, 3, nstart = 20)
# K-means clustering with 3 clusters of sizes 68, 9, 24

# Cluster means:
#       Mass    Period Eccentricity
# 1 2.734500  187.5163    0.2606221
# 2 5.390000 2767.2444    0.3283333
# 3 4.233333 1235.9729    0.3232917
# 
# Clustering vector:
#  [1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 1 3 1 2 1 3 1 1 1 1 3 1 1 1 3 1 1 1 1
# [37] 2 3 1 1 1 1 1 3 1 1 1 1 3 3 1 1 1 3 1 3 1 1 3 1 2 1 1 1 1 1 3 1 3 1 1 1
# [73] 1 2 3 1 3 3 3 3 1 1 3 1 1 3 1 2 1 3 3 1 1 2 2 1 2 3 1 3 1
# 
# Within cluster sum of squares by cluster:
#   [1] 2526577 8224052 2492267
# (between_SS / total_SS =  82.7 %)



scatterplot3d(Mass, Period, Eccentricity, color = (km.out$cluster+ 1), main="3D Scatterplot", pch = 16)


scatterplot3d(Mass, Period, Eccentricity, color = (km.out$cluster+ 1), main="3D Scatterplot", pch = 16)
plot(planet, main="Scatterplot of 3 clusters", col = (km.out$cluster+ 1), pch=19)


















