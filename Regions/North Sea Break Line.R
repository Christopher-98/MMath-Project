
library(dsims)

# region to be north sea shape file
region <- make.region(region.name = "North Sea Low Edge Density",
                      shape = "Regions/RegionPrj.shp")

cover <- make.coverage(region = region,
                       n.grid.points = 1000)
design.trunc = 10

design <- make.design(region = region,
                             transect.type = 'line',
                             design.angle = 90,
                             samplers = 20,
                             truncation = design.trunc,
                             coverage.grid = cover)


samplers <- generate.transects(design, region)

plot(region, samplers)


density <- make.density(region = region,
                        x.space = 5,
                        constant = 0.25)

density <- add.hotspot(density, c(150, 450), 50, 2)
density <- add.hotspot(density, c(-50, 700), 30, 2)
density <- add.hotspot(density, c(100, 850), 20, 2.5)
density <- add.hotspot(density, c(0, 825), 50, 2.5)
density <- add.hotspot(density, c(70, 620), 30, 2.5)


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
                             er.var = "S1",
                             truncation = design.trunc)

sim <- make.simulation(reps = 1,
                       design = design,
                       population.description = pop.desc,
                       detectability = detect,
                       ds.analysis = analyses)


survey <- run.survey(sim)

plot(survey)
