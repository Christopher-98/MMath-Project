# Script to produce all plots for report
library(dsims)

par(mfrow = c(1,3))
detect_hn <- make.detectability()
detect_hr <- make.detectability(key.function = 'hr', shape.param = 5)
detect_uf <- make.detectability(key.function = 'uf', scale.param = 1)

pop.desc <- make.population.description()

plot(detect_hn, pop.desc, main = 'hn detection function')
plot(detect_hr, pop.desc, main = 'hr detection function')
plot(detect_uf, pop.desc, main = 'uf detection function')


source('default region.R')
plot(density, region)# main = 'Basic density surface for Default region.')

plot(detect, pop.desc)

default.point <- read.csv('Estimates/region1000point.csv')

hist(default.point$dsm.est,breaks = 50, main = 'Histogram of DSM estimates')
hist(default.point$ds.est,breaks = 50, main = 'Histogram of DS estimates')

default.line <-  read.csv('Estimates/region1000line.csv')

hist(default.line$dsm.est,breaks = 50, main = 'Histogram of DSM estimates')
hist(default.line$ds.est,breaks = 50, main = 'Histogram of DS estimates')