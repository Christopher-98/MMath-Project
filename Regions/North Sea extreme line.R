 

library(dsm)
library(dsims)

# region to be north sea shape file
region <- make.region(region.name = "North Sea Extreme",
                      shape = "Regions/RegionPrj.shp")

cover <- make.coverage(region = region,
                       n.grid.points = 1000)
design.trunc = 10

design <- make.design(region = region,
                             transect.type = 'line',
                             design.angle = 90,
                             samplers = 25,
                             truncation = design.trunc,
                             coverage.grid = cover)


samplers <- generate.transects(design, region)

plot(region, samplers)


density <- make.density(region = region,
                        x.space = 25,
                        constant = 1)

density <- add.hotspot(density, c(250, 400), 100, 1.5)
density <- add.hotspot(density, c(300, 300), 100, 2.5)
density <- add.hotspot(density, c(100, 700), 300, -0.5)
density <- add.hotspot(density, c(-50, 700), 100, 1.5)
density <- add.hotspot(density, c(200, 800), 200, 4)


plot(density, region)

pop.desc <- make.population.description(region = region,
                                        density = density,
                                        N=1000)

detect <- make.detectability(key.function = "hn",
                             scale.param = 5,
                             truncation = design.trunc)

plot(detect, pop.desc)

analyses <- make.ds.analysis(dfmodel = list(~1),
                             key = "hn",
                             er.var = "R2",
                             truncation = design.trunc)

sim <- make.simulation(reps = 1,
                       design = design,
                       population.description = pop.desc,
                       detectability = detect,
                       ds.analysis = analyses)


survey <- run.survey(sim)

