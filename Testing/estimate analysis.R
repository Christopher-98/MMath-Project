# simulation analysis

# load appropriate estimates file

estimates <- read.csv('Estimates/region1000point.csv')

# calculate mean of each method
dsm.mean <- mean(estimates$dsm.est)
ds.mean <- mean(estimates$ds.est)

dsm.sd <- sd(estimates$dsm.est)
ds.sd <- sd(estimates$ds.est)


# bootstrap confidence intervals
N <- length(estimates$dsm.est)
alpha <- 0.025                               # So confidence level is 100(1-2*alpha)=95%

low <- round((N+1)*alpha)                    # Locate lower percentile (must be integer)
high <- round((N+1)*(1-alpha))               # Locate upper percentile (must be integer)

sorted.dsm.ests <- sort(estimates$dsm.est)
sorted.ds.ests <- sort(estimates$ds.est)     # sort the estimates

sorted.dsm.ests[c(low, high)]  # the estimated CI for dsm and ds
sorted.ds.ests[c(low, high)]

dsm.mean +c(-1, 1) *qnorm(0.975)*dsm.sd
ds.mean+c(-1, 1) *qnorm(0.975)*ds.sd

# calculate percentage bias
(dsm.mean - 1000) / 1000 *100
(ds.mean - 1000) / 1000 *100

# need standard errors of dsm estimates?
dsm.sd.mean <- mean(sqrt(estimates$dsm.var))


ds.se.mean <- mean(estimates$ds.se)

# coefficients of variation
ds.cvs <- estimates$ds.se/estimates$ds.est
summary(ds.cvs)

dsm.cvs <- sqrt(estimates$dsm.var)/estimates$dsm.est
summary(dsm.cvs)
