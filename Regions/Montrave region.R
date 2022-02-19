library(DSsim)


library(shapefiles)
region.shapefile <- read.shapefile("Regions/DSsim_study/Region")

region <- make.region(region.name = "Montrave", units = "m", shapefile = region.shapefile)


design.trunc = 1000

load("Regions/DSsim_study/density.surface.robj")
density <- make.density(region = region,
                        density.surface = density.surface,
                        x.space = 1000,
                        y.space = 1000)



plot(density,region)

pop.desc <- make.population.description(region.obj = region,
                                        density.obj = density,
                                        N = 1500,
                                        fixed.N = TRUE)


detect <- make.detectability(key.function = "hn",
                             scale.param = 500,
                             truncation = design.trunc)
plot(detect, pop.desc)



new.directory <- paste(getwd(), "Regions/DSsim_study/Survey_Transects/Subjective_Design",sep = "/")
design <- DSsim::make.design(transect.type = "Line",
                             design.details = c("user specified"),
                             region = region,
                             plus.sampling = FALSE,
                             path = new.directory)
design@filenames


ddf.analyses <- make.ddf.analysis.list(dsmodel = list(~cds(key = "hn", formula = ~1), #half-normal model
                                                      ~cds(key = "hr", formula = ~1)), #hazard-rate model
                                       method = "ds",
                                       criteria = "AIC",
                                       truncation = design.trunc)


sim<- make.simulation(reps = 1,
                      single.transect.set = TRUE,
                      region.obj = region,
                      design.obj = design,
                      population.description.obj = pop.desc,
                      detectability.obj = detect,
                      ddf.analyses.list = ddf.analyses)



# set the display window up for 4 plots
par(mfrow = c(2, 2))
# generate and plot and example population
pop <- generate.population(sim)
plot(region)
plot(pop)
# generate (or rather load from file) the transects
transects <- generate.transects(sim)
plot(region)
plot(transects)
# simulate the survey process of detection
survey <- create.survey.results(sim)
plot(survey)
# have a look at the distance data from the simulated survey
dist.data <- get.distance.data(survey)
hist(dist.data$distance, xlab = "Distance (m)", main = "Distance Data")

par(mfrow = c(1, 1))

dsims::run.survey(sim, region)

my.simulation.subjective.run <- run(sim)

analyses <- make.ds.analysis(dfmodel = list(~1),
                             key = detect@key.function,
                             er.var = "P3",
                             truncation = design.trunc)


sim <- dsims::make.simulation(reps = 1,
                       design = design,
                       population.description = pop.desc,
                       detectability = detect,
                       ds.analysis = ddf.analyses)

