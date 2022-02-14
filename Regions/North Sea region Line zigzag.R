library(dsims)

# create region
region <- make.region(region.name = "North Sea",
                            shape = "Regions/RegionPrj.shp")
plot(region)

design.trunc <- 10
# generate a coverage grid with approx 1000 points
cover <- make.coverage(region = region,
                       n.grid.points = 1000)

design <- make.design(region = region,
                      transect.type = "Line",
                      bounding.shape = "convex.hull",
                      design = "eszigzagcom",
                      samplers = 25,
                      design.angle = 0,
                      truncation = design.trunc,
                      coverage.grid = cover)


samplers <- generate.transects(design, region)
plot(region, samplers)


density <- make.density(region = region,
                        x.space = 25,
                        constant = 1)

density <- add.hotspot(density, c(150, 400), 100, 3)
density <- add.hotspot(density, c(100, 700), 300, 0.5)
density <- add.hotspot(density, c(200, 900), 200, 0.5)
density <- add.hotspot(density, c(250, 350), 50, -0.5)
density <- add.hotspot(density, c(-50, 700), 35, 1)

plot(density, region)

pop.desc <- make.population.description(region = region,
                                        density = density,
                                        N = 1000)

detect <- make.detectability(key.function = "hn",
                             scale.param = 5,
                             truncation = design.trunc)
plot(detect, pop.desc)

analyses <- make.ds.analysis(dfmodel = list(~1),
                             key = detect@key.function,
                             er.var = "R2",
                             truncation = design.trunc)

sim <- make.simulation(reps = 1,
                       design = design,
                       population.description = pop.desc,
                       detectability = detect,
                       ds.analysis = analyses)


survey <- run.survey(sim)
plot(survey)

# jpeg("Reports/Plots/North Sea survey zigzag.jpg")
# plot(survey)
# dev.off()

