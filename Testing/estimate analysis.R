# simulation analysis
library(moments)

# load appropriate estimates file

# load all files for North Sea Strat and combine estimates
# estimates1 <- read.csv('Estimates/North Sea Strat904linezigzag.csv')
# estimates2 <- read.csv('Estimates/North Sea Strat744linezigzag.csv')
# estimates3 <- read.csv('Estimates/North Sea Strat300linezigzag.csv')
# estimates4 <- read.csv('Estimates/North Sea Strat832linezigzag.csv')
# estimates5 <- read.csv('Estimates/North Sea Strat404linezigzag.csv')
# estimates6 <- read.csv('Estimates/North Sea Strat1043linezigzag.csv')
# estimates7 <- read.csv('Estimates/North Sea Strat220linezigzag.csv')
# estimates8 <- read.csv('Estimates/North Sea Stratline886.csv')
# 
# test <- rbind(estimates1,
#               estimates2,
#               estimates3,
#               estimates4,
#               estimates5,
#               estimates6,
#               estimates7,
#               estimates8)
# 
# test <- test[test$dsm.est != 0, ]
# test <- test[1:5000,]
# 
# write.csv(test, file = 'Estimates/North Sea Strat5000linezigzag.csv')


estimates <- read.csv('Estimates/North Sea O2line5000.csv')

N = 1000

# calculate mean of each method
dsm.mean <- mean(estimates$dsm.est)
ds.mean <- mean(estimates$ds.est)

dsm.sd <- sd(estimates$dsm.est)
ds.sd <- sd(estimates$ds.est)


# calculate percentage bias
dsm.bias <- (dsm.mean - N) / N *100
ds.bias <- (ds.mean - N) / N *100

# need standard errors of dsm estimates?
dsm.mean.se <- mean(estimates$dsm.se)
ds.mean.se <- mean(estimates$ds.se)

# coefficients of variation
ds.cvs <- estimates$ds.se/estimates$ds.est
summary(ds.cvs)
ds.cv.mean <- mean(ds.cvs)

dsm.cvs <- estimates$dsm.se/estimates$dsm.est
summary(dsm.cvs)
dsm.cv.mean <- mean(dsm.cvs)

# Truth in confidence interval
dsm.ci.coverage <- mean(estimates$dsm.ci.lo<N & estimates$dsm.ci.up > N)
ds.ci.coverage <- mean(estimates$ds.ci.lo<N & estimates$ds.ci.up > N)

# Skewness and Kurtosis of estimates
dsm.skew <- skewness(estimates$dsm.est)
dsm.kur <- kurtosis(estimates$dsm.est)

ds.skew <- skewness(estimates$ds.est)
ds.kur <- kurtosis(estimates$ds.est)

dsm.results <- c(dsm.mean, 
                 dsm.bias,
                 dsm.mean.se,
                 dsm.sd,
                 dsm.cv.mean,
                 dsm.ci.coverage,
                 dsm.skew,
                 dsm.kur)

ds.results <- c(ds.mean, 
                ds.bias,
                ds.mean.se,
                ds.sd,
                ds.cv.mean,
                ds.ci.coverage,
                ds.skew,
                ds.kur)




results <- cbind(dsm.results, ds.results)
results
write.csv(results, file = 'Results/results NS O2 estimator 5000.csv')




