library(dsims)

# create region
region <- make.region(region.name = "North Sea Strat",
                            strata.name = c('South', "North"),
                            shape = "Regions/StrataPrj.shp")
plot(region)

design.trunc <- 10
# generate a coverage grid with approx 1000 points
cover <- make.coverage(region = region,
                       n.grid.points = 1000)

design <- make.design(region = region,
                      transect.type = "point",
                      bounding.shape = rep("convex.hull",2),
                      samplers = c(30, 30),
                      truncation = design.trunc,
                      coverage.grid = cover)

samplers <- generate.transects(design, region)
plot(region, samplers)


density <- make.density(region = region,
                        x.space = 5,
                        constant = 1)

density <- add.hotspot(density, c(150, 400), 100, 3)
density <- add.hotspot(density, c(100, 700), 300, 0.5)
density <- add.hotspot(density, c(200, 900), 200, 0.5)
density <- add.hotspot(density, c(250, 350), 50, -0.5)
density <- add.hotspot(density, c(-50, 700), 35, 1)

plot(density, region)

pop.desc <- make.population.description(region = region,
                                        density = density,
                                        N = c(400,600))

detect <- make.detectability(key.function = "hn",
                             scale.param = 5,
                             truncation = design.trunc)
plot(detect, pop.desc)

analyses <- make.ds.analysis(dfmodel = list(~1),
                             key = detect@key.function,
                             er.var = "P3",
                             truncation = design.trunc)
