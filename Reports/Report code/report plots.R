# Script to produce all plots for report
library(dsims)

detect_hn <- make.detectability()
detect_hr <- make.detectability(key.function = 'hr', shape.param = 5)
detect_uf <- make.detectability(key.function = 'uf', scale.param = 1)

pop.desc <- make.population.description()

jpeg("Reports/Report plots/hn detect func.jpeg")
plot(detect_hn, pop.desc, main = 'hn detection function')
dev.off()

jpeg("Reports/Report plots/hr detect func.jpeg")
plot(detect_hr, pop.desc, main = 'hr detection function')
dev.off()

jpeg("Reports/Report plots/uf detect func.jpeg")
plot(detect_uf, pop.desc, main = 'uf detection function')
dev.off()

source('Regions/default region.R')

jpeg("Reports/Report plots/Default density.jpeg")
plot(density)#, main = 'Basic density surface for Default region.')
dev.off()

jpeg("Reports/Report plots/Default detect.jpeg")
plot(detect, pop.desc)
dev.off()


default.point <- read.csv('Estimates/region1000point.csv')

hist(default.point$dsm.est,breaks = 50, main = 'Histogram of DSM estimates')
hist(default.point$ds.est,breaks = 50, main = 'Histogram of DS estimates')

default.line <-  read.csv('Estimates/region1000line.csv')

hist(default.line$dsm.est,breaks = 50, main = 'Histogram of DSM estimates')
hist(default.line$ds.est,breaks = 50, main = 'Histogram of DS estimates')