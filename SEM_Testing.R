# lavvan library
library(lavaan)
# read dataset
dat <- read.csv("https://stats.idre.ucla.edu/wp-content/uploads/2021/02/worland5.csv")
# covariance matrix
cov(dat)
#simple regression using lm()
m1a <- lm(read ~ motiv, data=dat)
(fit1a <-summary(m1a))
#simple regression using lavaan 
m1b <-   '
  # regressions
    read ~ 1 + motiv
  # variance (optional)
    motiv ~~ motiv
'
fit1b <- sem(m1b, data=dat)
summary(fit1b)
# mean
mean(dat$motiv)
# variance
var(dat$motiv)
# convert the variance to maximum likelihood
498/500*(fit1a$sigma)**2
#@ match the lavaan output

# how Negative Parental Psychology ppsych and Motivation motiv
# to predict student readings scores read.
m2 <- '
  # regressions
    read ~ 1 + ppsych + motiv
 # covariance
    ppsych ~~ motiv
'
fit2 <- sem(m2, data=dat)
summary(fit2)
