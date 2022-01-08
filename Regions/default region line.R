library(dsm)
library(dsims)

# file to contain the basic region as a default with a point transect design
# with 25 samplers

region <- make.region()

design.trunc = 60

design <- make.design(region = region,
                      transect.type = "line",
                      samplers = 12,
                      truncation = design.trunc,
                      design.angle = 0)

samplers <- generate.transects(design, region)

plot(region, samplers)

density <- make.density(region = region, x.space = 25, constant = 1)

density <- add.hotspot(density, c(500, 200), 100, 3)
density <- add.hotspot(density, c(700, 100), 300, 0.5)
density <- add.hotspot(density, c(1500, 400), 200, 1)
density <- add.hotspot(density, c(1000, 450), 50, 2)

plot(density, region)

pop.desc <- make.population.description(region = region,
                                        density = density,
                                        N=1000)

detect <- make.detectability(key.function = "hn",
                             scale.param = 30,
                             truncation = design.trunc)

analyses <- make.ds.analysis(dfmodel = list(~1),
                             key = detect@key.function,
                             er.var = "R2",
                             truncation = design.trunc)

#plot(detect, pop.desc)
sim <- make.simulation(reps = 1,
                       design = design,
                       population.description = pop.desc,
                       detectability = detect,
                       ds.analysis = analyses)


survey <- run.survey(sim)
