

### problem 1. a)

german_credit <- read.csv("germancredit.csv", header = T)

variables <- names(german_credit)
str(german_credit)

# 13 categorical and 7 numerical predictors for 1 response variable Default

attach(german_credit)

table(Default)

### problem 1. b)
chi_value <- matrix(nrow = 20, ncol = 1)
par(mfrow=c(2,10))
for (i in 2:21) {
  ta1 <- table(german_credit[,i], Default)
  barplot(prop.table(ta1, mar = 2), ylab = variables[i], xlab =variables[1])
  # chisq.test(ta1)
  fit1 <- glm(Default ~ german_credit[,i], family = binomial, data = german_credit) # fit glm for each predictor
  # summary(fit1)
  anova_table <- anova(fit1, test = "Chisq")
  chi_value[i-1,1] <- anova_table$'Pr(>Chi)'[2] # Extracting chisqure value from anova_table
}


predictors.tables <- data.frame(variables[2:21], chi_value, ifelse(chi_value < 0.25, "ueseful", "unuseful"))
colnames(predictors.tables) <- c("respond", "Chi-value", "decision")
predictors.tables
#            respond    Chi-value decision
# 1  checkingstatus1 2.787203e-28  ueseful
# 2         duration 2.398744e-11  ueseful
# 3          history 2.313958e-12  ueseful
# 4          purpose 7.268797e-05  ueseful
# 5           amount 1.928791e-06  ueseful
# 6          savings 7.049052e-08  ueseful
# 7           employ 1.146430e-03  ueseful
# 8      installment 2.116372e-02  ueseful
# 9           status 2.396272e-02  ueseful
# 10          others 3.597102e-02  ueseful
# 11       residence 9.252355e-01 unuseful
# 12        property 3.106302e-05  ueseful
# 13             age 3.389931e-03  ueseful
# 14      otherplans 2.129794e-03  ueseful
# 15         housing 1.448466e-04  ueseful
# 16           cards 1.443321e-01  ueseful
# 17             job 6.032614e-01 unuseful
# 18          liable 9.239826e-01 unuseful
# 19            tele 2.477554e-01  ueseful
# 20         foreign 4.494505e-03  ueseful

fit.full <- glm(Default ~ . , family = binomial, data = german_credit) 
summary(fit.full)

fit2 <- glm(Default ~ factor(checkingstatus1) + duration + factor(history) + factor(purpose) + amount + 
              factor(savings) + factor(employ) + installment + factor(status) + factor(others) + 
              factor(property) + age + factor(otherplans) + factor(housing) + cards + 
              factor(tele) + factor(foreign), family = binomial, data = german_credit) 
anova(fit.full, fit2, test = "Chisq")
summary(fit2)

# Remove employ
fit3 <- glm(Default ~ factor(checkingstatus1) + duration + factor(history) + factor(purpose) + amount + 
              factor(savings)  + installment + factor(status) + factor(others) + 
              factor(property) + age + factor(otherplans) + factor(housing) + cards + 
              factor(tele) + factor(foreign), family = binomial, data = german_credit) 
summary(fit3)
anova(fit.full, fit3, test = "Chisq")

# Remove property
fit4 <- glm(Default ~ factor(checkingstatus1) + duration + factor(history) + factor(purpose) + amount + 
              factor(savings)  + installment + factor(status) + factor(others) + 
              age + factor(otherplans) + factor(housing) + cards + 
              factor(tele) + factor(foreign), family = binomial, data = german_credit) 
summary(fit4)
anova(fit.full, fit4, test = "Chisq")

# Remove age
fit5 <- glm(Default ~ factor(checkingstatus1) + duration + factor(history) + factor(purpose) + amount + 
              factor(savings)  + installment + factor(status) + factor(others) + 
              factor(otherplans) + factor(housing) + cards + 
              factor(tele) + factor(foreign), family = binomial, data = german_credit) 
summary(fit5)
anova(fit.full, fit5, test = "Chisq")

# Remove cards
fit6 <- glm(Default ~ factor(checkingstatus1) + duration + factor(history) + factor(purpose) + amount + 
              factor(savings) + installment + factor(status) + factor(others) + 
              factor(otherplans) + factor(housing) + 
              factor(tele) + factor(foreign), family = binomial, data = german_credit) 
summary(fit6)
anova(fit.full, fit6, test = "Chisq")

# Remove tele
fit7 <- glm(Default ~ factor(checkingstatus1) + duration + factor(history) + factor(purpose) + amount + 
              factor(savings) + installment + factor(status) + factor(others) + 
              factor(otherplans) + factor(housing) + 
              factor(foreign), family = binomial, data = german_credit) 
summary(fit7)
anova(fit.full, fit7, test = "Chisq")

### problem 1. c)
confint(fit7)



