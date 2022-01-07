# simulation analysis

# load appropriate estimates file

estimates #<- 

# calculate mean of each method
dsm.mean <- mean(estimates$dsm.est)
ds.mean <- mean(estimates$ds.est)

dsm.sd <- sd(estimates$dsm.est)
ds.sd <- sd(estimates$ds.est)


dsm.mean +c(-1, 1) *qnorm(0.975)*dsm.sd
ds.mean+c(-1, 1) *qnorm(0.975)*ds.sd

# calculate percentage bias
(dsm.mean - 1000) / 1000 *100
(ds.mean - 1000) / 1000 *100

# need standard errors of dsm estimates?
dsm.sd.mean <- mean(sqrt(estimates$dsm.var))


ds.se.mean <- mean(estimates$ds.se)

