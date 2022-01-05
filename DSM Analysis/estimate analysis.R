# simulation analysis

mean(dsm.estimates)
mean(ds.estimates)

quantile(dsm.estimates, c(0.025, 0.975))
quantile(ds.estimates, c(0.025, 0.975))

# is dsm more likely to provide an over or under estimate compared to ds
sum(dsm.estimates>1000)
sum(dsm.estimates<1000)

mean(dsm.estimates[dsm.estimates>1000])
mean(dsm.estimates[dsm.estimates<1000])