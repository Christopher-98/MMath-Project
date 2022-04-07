library(dsims)
region <- make.region(region.name = "Buckland 2015",
                      shape = "Regions/Region.shp")

cover <- make.coverage(region = region,
                       n.grid.points = 1000)

design.trunc <- 1000

design <- make.design(region = region,
                      transect.type = 'line',
                      design.angle = 45,
                      spacing = 12000,
                      #samplers = 15,
                      truncation = design.trunc,
                      coverage.grid = cover)

samplers <- generate.transects(design, region)
plot(region, samplers)



density <- make.density(region = region,
                        x.space = 1000,
                        constant = 1)

density <- add.hotspot(density, c(0, 2200000), 10000, 3)
density <- add.hotspot(density, c(-10000, 2230000), 5000, 2)
density <- add.hotspot(density, c(-40000, 2250000), 5000, 3)
density <- add.hotspot(density, c(50000, 2185000), 10000, 3)
density <- add.hotspot(density, c(10000, 2165000), 5000, 2)
density <- add.hotspot(density, c(25000, 2210000), 7500, 3)
density <- add.hotspot(density, c(-25000, 2205000), 7500, -0.75)
density <- add.hotspot(density, c(-45000, 2270000), 5000, -0.5)
density <- add.hotspot(density, c(20000, 2180000), 5000, -0.5)
density <- add.hotspot(density, c(-45000, 2270000), 10000, -0.35)

#Extract density values
densities <- get.densities(density)
#Shift the values down so minimum density values are 0 (no scaling required)
min.d <- min(densities)
densities <- densities-min.d
#Put adjusted densities back in density grid
density <- set.densities(density, densities)

plot(density, region)

pop.desc <- make.population.description(region = region,
                                        density = density,
                                        N=1500)

detect <- make.detectability(key.function = "hn",
                             scale.param = 500,
                             truncation = design.trunc)
plot(detect, pop.desc)



analyses <- make.ds.analysis(dfmodel = list(~1),
                             key = detect@key.function,
                             er.var = "R2",
                             truncation = design.trunc)


sim <- make.simulation(reps = 10,
                       design = design,
                       population.description = pop.desc,
                       detectability = detect,
                       ds.analysis = analyses)

survey <- run.survey(sim)
plot(survey)

survey@dist.data[!is.na(survey@dist.data$object),]

jpeg("Reports/Plots/Montrave survey parallel line.jpg")
plot(region, samplers)
dev.off()

