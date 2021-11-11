# problem 1

library(boot) # for bootstrap
library(gdata) # for resample
data_set <- read.delim("oxygen_saturation.txt")
attach(data_set)
n <- dim(data_set)[1]

# problem 1 a)

plot(osm, pos, main="Scatterplot",
     xlab="Oxygen Saturation Monitor", 
     ylab="Pulse Oximetry Screener", pch=19)
abline(0, 1)


# problem 1 d)

# function to compute concordance correlation coefficient (CCC)
CCC.fn <- function(data, indices) {
  x <- data[indices,1]
  y <- data[indices,2]
  mu1 <- mean(x)
  mu2 <- mean(y)
  sigma1 <- var(x)
  sigma2 <- var(y)
  sigma12 <- cov(x, y)
  result <- 2*sigma12/((mu1-mu2)^2+sigma1+sigma2)
  return(result)
}

CCC.fn(data_set, 1:n) # original CCC 
# [1] 0.9892748



# my own code for the bootstrap method
my_boot.fn <- function(data, n, B) {
  para <- c()
  i = 1
  while(i<=B) {
    sample <- data[resample(1:n, n, replace = TRUE, prob = NULL),]
    para[i] <- CCC.fn(sample, 1:n) # CCC of the sample
    i <- i+1
  }
  CCC_original <- CCC.fn(data_set, 1:n) # original CCC 
  CCC_my <- mean(para) # mean of the CCC from samples
  bias <- CCC_my - CCC_original
  
  #computation of the CI from standard error
  std_error<-sd(para)/sqrt(n) # standard error 
  lower_bound_SE <- mean(para)-std_error
  upper_bound_SE <- mean(para)+std_error
  
  #computation of the 95% CI of the mean
  perc <- quantile(para, probs = c(0.475, 0.525))
  lower_bound_Perc <- perc[1]
  upper_bound_Perc <- perc[2]
  return(list(data.frame(CCC_original, CCC_my, bias, std_error),
              "Standerd Error CI" = data.frame(lower_bound_SE, upper_bound_SE),
              "Percentile method CI" = data.frame(lower_bound_Perc),
              "Percentile method CI" = data.frame(upper_bound_Perc)))
}

set.seed(1)
my_boot.fn(data_set, n, 1000) 
#   CCC_original    CCC_my          bias    std_error
# 1    0.9892748 0.9889337 -0.0003411283 0.0002304046
# 
# $`Standerd Error CI`
#   lower_bound_SE upper_bound_SE
# 1      0.9887033      0.9891641
# 
# $`Percentile method CI`
#       lower_bound_Perc
# 47.5%        0.9889895
# 
# $`Percentile method CI`
#       upper_bound_Perc
# 52.5%        0.9892421



# problem 1 f)

set.seed(1)
CCC.boot <- boot(data_set, CCC.fn, R = 1000) # bootstrap estimates of bias and standard error for CCC
# ORDINARY NONPARAMETRIC BOOTSTRAP
# 
# 
# Call:
#   boot(data = data_set, statistic = CCC.fn, R = 1000)
# 
# 
# Bootstrap Statistics :
#      original        bias    std. error
# t1* 0.9892748 -0.0003717729 0.002032808


boot.ci(CCC.boot, conf = 0.95, type = "perc") # 95% lower confidence bound for CCC
# BOOTSTRAP CONFIDENCE INTERVAL CALCULATIONS
# Based on 1000 bootstrap replicates
# 
# CALL : 
# boot.ci(boot.out = CCC.boot, type = "perc")
# 
# Intervals : 
# Level     Percentile     
# 95%   ( 0.9841,  0.9923 )  
# Calculations and Intervals on Original Scale












