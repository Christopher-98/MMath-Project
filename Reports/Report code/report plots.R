# Script to produce all plots for report
library(dsims)

detect_hn <- make.detectability()
detect_hr <- make.detectability(key.function = 'hr', shape.param = 5)
detect_uf <- make.detectability(key.function = 'uf', scale.param = 1)

pop.desc <- make.population.description()

jpeg("Reports/Plots/hn_detectfunc.jpg")
plot(detect_hn, pop.desc, main = 'hn detection function')
dev.off()

jpeg("Reports/Plots/hr_detectfunc.jpg")
plot(detect_hr, pop.desc, main = 'hr detection function')
dev.off()

jpeg("Reports/Plots/uf_detectfunc.jpg")
plot(detect_uf, pop.desc, main = 'uf detection function')
dev.off()

source('Regions/default region.R')

jpeg("Reports/Plots/Default_density.jpg")
plot(density)
dev.off()

jpeg("Reports/Plots/Default_detect.jpg")
plot(detect, pop.desc)
dev.off()

jpeg("Reports/Plots/Default survey point.jpg")
plot(region, samplers)
dev.off()

source('Regions/default region line.R')

jpeg("Reports/Plots/Default survey line.jpg")
plot(region, samplers)
dev.off()

source('Regions/default region line zigzag.R')

jpeg("Reports/Plots/Default survey zigzag.jpg")
plot(region, samplers)
dev.off()


#North Sea Non-Stratified
source('Regions/North Sea region.R')

jpeg("Reports/Plots/North Sea survey point.jpg")
plot(region, samplers)
dev.off()

source('Regions/North Sea region line.R')

jpeg("Reports/Plots/North Sea survey line.jpg")
plot(region, samplers)
dev.off()

source('Regions/North Sea region line zigzag.R')

jpeg("Reports/Plots/North Sea survey zigzag.jpg")
plot(region, samplers)
dev.off()


#North Sea Stratified
source('Regions/North Sea Strata.R')

jpeg("Reports/Plots/North Sea Strata survey point.jpg")
plot(region, samplers)
dev.off()

source('Regions/North Sea Strata line.R')

jpeg("Reports/Plots/North Sea Strata survey line.jpg")
plot(region, samplers)
dev.off()

source('Regions/North Sea Strata line zigzag.R')

jpeg("Reports/Plots/North Sea Strata survey zigzag.jpg")
plot(region, samplers)
dev.off()



#Montrave region
source('Regions/Montrave region parallel line.R')

jpeg("Reports/Plots/Montrave survey parallel line.jpg")
plot(region, samplers)
dev.off()

jpeg("Reports/Plots/Montrave density.jpg")
plot(density)
dev.off()

jpeg("Reports/Plots/Montrave detect.jpg")
plot(detect, pop.desc)
dev.off()

source('Regions/Montrave region random line.R')

jpeg("Reports/Plots/Montrave survey random line.jpg")
plot(region, samplers)
dev.off()

source('Regions/Montrave region zigzag.R')

jpeg("Reports/Plots/Montrave survey zigzag.jpg")
plot(region, samplers)
dev.off()