library(dsims)

# file to contain the appropriate shapfile and design for the unstratified
# north sea region with a point transect design

#  change region to be north sea shapefile
region <- make.region(region.name = "North Sea",
                      shape = "Regions/RegionPrj.shp")

cover <- make.coverage(region = region,
                       n.grid.points = 1000)
design.trunc = 10
design <- make.design(region = region,
                      transect.type = "point",
                      samplers = 70,
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
                             key = detect@key.function,
                             er.var = "P3",
                             truncation = design.trunc)

sim <- make.simulation(reps = 1,
                       design = design,
                       population.description = pop.desc,
                       detectability = detect,
                       ds.analysis = analyses)


survey <- run.survey(sim)
plot(survey)

# jpeg("Reports/Plots/North Sea survey point.jpg")
# plot(survey)
# dev.off()

