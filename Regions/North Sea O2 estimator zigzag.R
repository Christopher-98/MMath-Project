library(dsims)

# region to be north sea shape file
region <- make.region(region.name = "North Sea O2",
                      shape = "Regions/RegionPrj.shp")

cover <- make.coverage(region = region,
                       n.grid.points = 1000)
design.trunc = 10

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

density <- add.hotspot(density, c(0, 600), 100, 3)
density <- add.hotspot(density, c(100, 700), 300, 0.5)
density <- add.hotspot(density, c(200, 900), 200, 1)
density <- add.hotspot(density, c(250, 350), 50, -0.5)

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
                             er.var = "O2",
                             truncation = design.trunc)

sim <- make.simulation(reps = 1,
                       design = design,
                       population.description = pop.desc,
                       detectability = detect,
                       ds.analysis = analyses)
# non standard variance estimators not available for zigzag designs

survey <- run.survey(sim)
plot(survey)

# jpeg("Reports/Plots/North Sea survey line.jpg")
# plot(survey)
# dev.off()

