
library(dsims)

# region to be north sea shape file
region <- make.region(region.name = "North Sea Break",
                      shape = "Regions/RegionPrj.shp")

cover <- make.coverage(region = region,
                       n.grid.points = 1000)
design.trunc = 5

design <- make.design(region = region,
                             transect.type = 'line',
                             design.angle = 90,
                             samplers = 25,
                             truncation = design.trunc,
                             coverage.grid = cover)


samplers <- generate.transects(design, region)

plot(region, samplers)


density <- make.density(region = region,
                        x.space = 5,
                        constant = 1)

density <- add.hotspot(density, c(250, 400), 10, 1.5)
density <- add.hotspot(density, c(300, 300), 10, 2.5)
density <- add.hotspot(density, c(100, 700), 30, -1)
density <- add.hotspot(density, c(-50, 700), 10, 1.5)
density <- add.hotspot(density, c(100, 850), 20, 3)
density <- add.hotspot(density, c(50, 400), 10, 2)
density <- add.hotspot(density, c(150, 400), 10, 2)
density <- add.hotspot(density, c(0, 500), 10, 2)
density <- add.hotspot(density, c(-50, 600), 10, 2)
density <- add.hotspot(density, c(-50, 900), 10, 2)
density <- add.hotspot(density, c(100, 500), 10, 2)
density <- add.hotspot(density, c(0, 800), 10, 2)
density <- add.hotspot(density, c(170, 550), 10, 2)
density <- add.hotspot(density, c(70, 620), 10, 2)

# Extract density values 
densities <- get.densities(density)
# Shift the values down so minimum density values are 0 (no scaling required)
min.d <- min(densities)
densities <- densities-min.d
# Put adjusted densities back in density grid
density <- set.densities(density, densities)


plot(density, region)

pop.desc <- make.population.description(region = region,
                                        density = density,
                                        N=1000)

detect <- make.detectability(key.function = "hn",
                             scale.param = 2.5,
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
