library(dsims)

# file to contain the basic region as a default with a point transect design
# with 30 samplers

region <- make.region(region.name = 'Default')

density <- make.density(region = region, x.space = 25, constant = 1)

density <- add.hotspot(density, c(500, 200), 100, 3)
density <- add.hotspot(density, c(700, 100), 300, 0.5)
density <- add.hotspot(density, c(1500, 400), 200, 1)
density <- add.hotspot(density, c(1000, 450), 50, 2)

plot(density, region)

pop.desc <- make.population.description(region = region,
                                        density = density,
                                        N=1000)

design.trunc = 60

design <- make.design(region = region,
                      transect.type = "point",
                      samplers = 35,
                      truncation = design.trunc)

samplers <- generate.transects(design, region)

plot(region, samplers)


detect <- make.detectability(key.function = "hn",
                             scale.param = 30,
                             truncation = design.trunc)

analyses <- make.ds.analysis(dfmodel = list(~1),
                             key = detect@key.function,
                             er.var = "P3",
                             truncation = design.trunc)
#plot(detect, pop.desc)
sim <- make.simulation(reps = 1,
                       design = design,
                       population.description = pop.desc,
                       detectability = detect,
                       ds.analysis = analyses)


survey <- run.survey(sim)
plot(survey)

# jpeg("Reports/Plots/Default survey point.jpg")
# plot(survey, main = 'Example Point transect survey')
# dev.off()
