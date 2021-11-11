

problem 1. a) and b)

train.data <- read.csv(file.choose(), header = T)
attach(train.data)


log.Length <- log(train.data$Length)

boxplot(train.data$Length, main="Length as the response
variable", ylab="Length")


boxplot(log.Length, main="Length as the response
variable", ylab="log of Length")


par(mfrow = c(1, 2))
boxplot(train.data$Length, main="Length as the response
variable", ylab="Length")
boxplot(log.Length, main="log of Length as the response
variable", ylab="log of Length")


log.Length <- log(Length)
par(mfrow = c(1, 2))
hist(Length, main="Length as the response
variable", breaks=10, xlab="Length", ylab="Count")
hist(log.Length, main="log of Length as the response
variable", breaks=10, xlab="log of Length", ylab="Count")

shapiro.test(Length)
shapiro.test(log.Length)

part c)

# ShellType
# Width
# AperHt
# AperWdt
# LU
# LipWdt
Model.ShellType = lm(Length~ShellType, data=train.data) 
summary(Model.ShellType)
par(mfrow = c(2, 2))
plot(Model.ShellType, pch = 19)

Model.Width = lm(Length~Width, data=train.data) 
summary(Model.Width)
par(mfrow = c(2, 2))
plot(Model.Width, pch = 19)

Model.AperHt = lm(Length~AperHt, data=train.data) 
summary(Model.AperHt)
par(mfrow = c(2, 2))
plot(Model.AperHt, pch = 19)

Model.AperWdt = lm(Length~AperWdt, data=train.data) 
summary(Model.AperWdt)
par(mfrow = c(2, 2))
plot(Model.AperWdt, pch = 19)

Model.LU = lm(Length~LU, data=train.data) 
summary(Model.LU)
par(mfrow = c(2, 2))
plot(Model.LU, pch = 19)

Model.LipWdt = lm(Length~LipWdt, data=train.data) 
summary(Model.LipWdt)
par(mfrow = c(2, 2))
plot(Model.LipWdt, pch = 19)





part d)

Model.All = lm(Length ~. , data=train.data)
summary(Model.All)

Model.droped = lm(Length ~ AperHt + LU , data=train.data)
summary(Model.droped)


part e)

lm.Length.Shell.Width = lm(Length ~ AperHt + LU + ShellType:AperHt, data=train.data) #Create the linear regression
summary(lm.Length.Shell.Width)

lm.Length.Shell.Width = lm(Length ~ AperHt + LU + ShellType:LU, data=train.data) #Create the linear regression
summary(lm.Length.Shell.Width)

#fitting the multiple linear regression model with interactions
Model.int=lm(Length ~ AperHt + LU + AperHt:ShellType + LU:ShellType, data=train.data)
summary(Model.int)

# Residual plot
plot(fitted(Model.int), resid(Model.int))
abline(h = 0)
# QQ plot
qqnorm(resid(Model.int))



lm.Length.Shell.Width = lm(Length ~ ShellType + Width + AperHt + AperWdt + LU + LipWdt + 
                     ShellType:Width, data=train.data) #Create the linear regression
summary(lm.Length.Shell.Width)

lm.Length.Shell.AperHt = lm(Length ~ ShellType + Width + AperHt + AperWdt + LU + LipWdt + 
                             ShellType:AperHt, data=train.data) #Create the linear regression
summary(lm.Length.Shell.AperHt)

lm.Length.Shell.AperWdt = lm(Length ~ ShellType + Width + AperHt + AperWdt + LU + LipWdt + 
                             ShellType:AperWdt, data=train.data) #Create the linear regression
summary(lm.Length.Shell.AperWdt)

lm.Length.Shell.LU = lm(Length ~ ShellType + Width + AperHt + AperWdt + LU + LipWdt + 
                             ShellType:LU, data=train.data) #Create the linear regression
summary(lm.Length.Shell.LU)

lm.Length.Shell.LipWdt = lm(Length ~ ShellType + Width + AperHt + AperWdt + LU + LipWdt + 
                          ShellType:LipWdt, data=train.data) #Create the linear regression
summary(lm.Length.Shell.LipWdt)





interaction.plot(train.data$Width, train.data$ShellType, train.data$Length, ylab = "Length", xlab = "Width")
interaction.plot(train.data$AperHt, train.data$ShellType, train.data$Length, ylab = "Length", xlab = "AperHt")
interaction.plot(train.data$AperWdt, train.data$ShellType, train.data$Length, ylab = "Length", xlab = "AperWdt")
interaction.plot(train.data$LU, train.data$ShellType, train.data$Length, ylab = "Length", xlab = "LU")
interaction.plot(train.data$LipWdt, train.data$ShellType, train.data$Length, ylab = "Length", xlab = "LipWdt")



Model.int=lm(Length ~ AperHt + LU + AperHt:ShellType + LU:ShellType, data=train.data)
summary(Model.int)



anova(Model.droped, Model.int)


f)

par(mfrow = c(1, 3))
# Plot of residuals vs fitted for main effects only model
plot(fitted(Model.int), residuals(Model.int), pch = 19, xlab = "Fitted Values", ylab = "Residuals", main = "Plot of Residuals vs Fitted Values")
abline(h = 0, col = "red")

# Normal Q-Q plot
qqnorm(residuals(Model.int), pch = 19, main = "Normal Q-Q Plot")
qqline(residuals(Model.int), col = "red")

# Residual sequential plot to check the independence of residuals
plot(residuals(Model.int), xlab = "psa_level", ylab = "Residual", main="Residual Sequence Plot", type = "l")
abline(h = 0, col = "red")

# Normality test: Shapiro-Wilk normality test
shapiro.test(residuals(Model.int))


# Constant variance: Breusch-Pagan test
library(lmtest)
bptest(Model.int)
# Durbin-Watson test to check whether residuals are uncorrelated
dwtest(Model.int)


g)

# predicting the PSA Level at mean levels of quantitative variables and when ShellType=0

Length.Type1 = predict(Model.int, newdata)

newdata <- data.frame(ShellType=1, AperHt=mean(AperHt), LU=mean(LU))


ShellType










