
# Problem 1

head(state.x77)
str(state.x77)
# Extract the names of states
states <- row.names(state.x77)

# part a)
summary(state.x77)
chart.Correlation(state.x77) #matrix of scatterplot and correlation after transformation

# part b)
# Look at mean and sd
apply(state.x77, 2, mean)
apply(state.x77, 2, sd)

# part c)
# Perform PCA
pca <- prcomp(state.x77, center = T, scale = T)
names(pca)

# Get the loading matrix
pca$rotation

# Get the score matrix
dim(pca$x)
head(pca$x)

# Check the covariance matrix of the scores
round(cov(pca$x), 4)

# Display a biplot the results (shows both pc scores and loading vectors)
pca$rotation
biplot(pca, scale=0)


# Display the biplot after changing the signs of loadings and scores
pca$rotation <- -pca$rotation
pca$x <- -pca$x
pca$rotation
biplot(pca, scale=0)


# Compute the proportion of variance explained (PVE)
pc.var <- pca$sdev^2
pve <- pc.var/sum(pc.var)
pve
cumsum(pve)

#Correlations
rbind(PC1 = c(pca$rotation[,1]*pca$sdev[1], cumalative.PVE = cumsum(pve)[1]),
      PC2 = c(pca$rotation[,2]*pca$sdev[2], cumalative.PVE = cumsum(pve)[2]))

par(mfrow=c(1,2))
# Scree plot
plot(pve, xlab = "Principal Component", ylab = "Proportion of Variance Explained", ylim = c(0,1), type = 'b')



# Plot of cumulative PVE
plot(cumsum(pve), xlab = "Principal Component", ylab = "Cumulative Proportion of Variance Explained", ylim = c(0,1), type = 'b')
par(mfrow=c(1,1))







#Correlations
rbind(PC1 = c(pca$rotation[,1]*pca$sdev[1], cumalative.PVE = cumsum(pve)[1]),
      PC2 = c(pca$rotation[,2]*pca$sdev[2], cumalative.PVE = cumsum(pve)[2]))







