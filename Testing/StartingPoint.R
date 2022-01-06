library(dsm)
library(dsims)
library(dssd)
library(raster)
library(rgeos)
library(dismo)
library(sf)
library(Distance)

region <- make.region()

design.trunc = 60
design <- make.design(region = region,
                      transect.type = "point",
                      samplers = 25,
                      truncation = design.trunc )

samplers <- generate.transects(design, region)
plot(region, samplers)

coverage <- run.coverage(design, reps = 10)
coverage
class(coverage)
density <- make.density(region = region,
                        x.space = 25,
                        constant = 1)

density <- add.hotspot(density,
                       c(500, 200), 100, 3)
density <- add.hotspot(density,
                       c(700, 100), 300, 0.5)
density <- add.hotspot(density,
                       c(1500, 400), 200, 1)
density <- add.hotspot(density, 
                       c(1000, 450), 50, 2)

plot(density, region)

pop.desc <- make.population.description(region = region,
                                        density = density,
                                        N=1200)

detect <- make.detectability(key.function = "hn",
                             scale.param = 30,
                             truncation = design.trunc)

plot(detect, pop.desc)

analyses <- make.ds.analysis(dfmodel = list(~1),
                             key = "hn",
                             er.var = "P3",
                             truncation = design.trunc)

sim <- make.simulation(reps = 10,
                       design = design,
                       population.description = pop.desc,
                       detectability = detect,
                       ds.analysis = analyses)


survey <- run.survey(sim)
plot(survey, region)

survey.sims <- run.simulation(sim)

summary(survey.sims)

fit <- analyse.data(analyses, data.obj = survey)
fit

# To extract coordinates to make segdata
test <- survey@transect@samplers


segdata <- data.frame(Sample.Label = survey@transect@samplers$transect,
                     Effort = rep(1, nrow(test)),
                     X = sf::st_coordinates(test)[,1],
                     Y = sf::st_coordinates(test)[,2],
                     Area = 2*pi*design.trunc)


# observed data - dont have either- can extract from survey??
obsdata <- survey@dist.data[,c("object", "Sample.Label", "distance")]
obsdata$size <- 1
str(obsdata)

sim.obsdata <- survey@dist.data[,c("object", "Sample.Label", "distance")]
sim.obsdata$size <- rep(1, nrow(sim.obsdata))
sim.obsdata <- sim.obsdata[!is.na(sim.obsdata$object),]


# Point transect example
# https://examples.distancesampling.org/dsm-point/hare_point_transect_dsm-distill.html

# create a prediction grid
# method from http://rfunctions.blogspot.co.uk/2014/12/how-to-create-grid-and-intersect-it.html


# Create an empty raster
grid <- raster(extent(sf::st_bbox(region@region)))
# Choose its resolution. 500 m in both X and Y (truncation distance)
res(grid) <- design.trunc
# Transform this raster into a polygon and you will have a grid
gridpolygon <- rasterToPolygons(grid)

sp.region <- sf::as_Spatial(region@region)

# Intersect our grid with shape
pred.grid <- raster::intersect(gridpolygon, sp.region)

# Plot the intersected shape to check if everything is fine.
plot(pred.grid)


# create the data.frame for prediction
preddata <- as.data.frame(matrix(NA, ncol=3, nrow=nrow(pred.grid@data)))
colnames(preddata) <- c("X", "Y", "area")
for (i in 1:dim(pred.grid@data)[1]){
  preddata[i, c("X", "Y")] <- pred.grid@polygons[[i]]@labpt
  preddata[i, c("area")] <- pred.grid@polygons[[i]]@area
}

###############################################################################

# need to extract count data from survey so that can be imputed into model
# from there density can be modelled, checked and predicted




df_ht <- ds(survey@dist.data, truncation=design.trunc, transect="point",
            formula=~1, key="hn", adjustment=NULL)
summary(df_ht)
plot(df_ht)

# dsm requires data to be in a particular format:
# https://examples.distancesampling.org/dsm-data-formatting/dsm-data-formatting.html


# density model
mod_tw <- dsm(count~s(X, Y), ddf.obj=fit$model, segment.data=segdata, 
              observation.data=sim.obsdata, family=tw(), transect="point")

summary(mod_tw)
# gam check
gam.check(mod_tw)

# obtains the abundance estimate
# needs checked to ensure is close to original population
mod_tw_est <- dsm.var.gam(mod_tw, pred.data = preddata, off.set = preddata$area)

mod_tw_est$pred[[1]]

mod_tw_pred <- predict(mod_tw, preddata, preddata$area)



# ############################## notes
# predictions inc. if split into strata
# soup film smoothers
# checks to complete
# fit data to dsm (almost complete, see above)
# possible covariates- e.g. habitat
# comparison between distance design and dsm design

