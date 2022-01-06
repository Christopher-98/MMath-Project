library(dsims)
region <- make.region()

design <- make.design(region = region,
                      transect.type = "point",
                      samplers = 50,
                      truncation = 60)

samplers <- generate.transects(design, region)
plot(region, samplers)

density <- make.density(region = region,
                        x.space = 25,
                        constant = 1)

density <- add.hotspot(density,
                       c(500, 200), 100, 3)
density <- add.hotspot(density,
                       c(700, 100), 300, 0.5)
density <- add.hotspot(density,
                       c(1500, 400), 200, 1)

plot(density, region)

pop.desc <- make.population.description(region = region,
                                        density = density,
                                        N=1200)

detect <- make.detectability(key.function = "hn",
                             scale.param = 30,
                             truncation = 60)

plot(detect, pop.desc)

analyses <- make.ds.analysis(dfmodel = list(~1),
                             key = "hn",
                             er.var = "P3",
                             truncation = 60)

sim <- make.simulation(reps = 10,
                       design = design,
                       population.description = pop.desc,
                       detectability = detect,
                       ds.analysis = analyses)


survey <- run.survey(sim)
plot(survey, region)

fit <- analyse.data(analyses, data.obj = survey)
fit$model$dht

# To extract coordinates to make segdata
library(sf)
test <- as_Spatial(survey@transect@samplers)
tmp <- as.data.frame(test)

sim.segdata <- data.frame(Sample.Label = tmp$transect,
                          Effort = rep(1,nrow(tmp)),
                          Segment.Label = tmp$transect,
                          X = tmp$coords.x1,
                          Y = tmp$coords.x2)

sim.obsdata <- survey@dist.data[,c("object", "Sample.Label", "distance")]
sim.obsdata$size <- rep(1, nrow(sim.obsdata))
sim.obsdata <-sim.obsdata[!is.na(sim.obsdata$object),]



library(dsm)
?dsm
# Point transect example
# https://examples.distancesampling.org/dsm-point/hare_point_transect_dsm-distill.html

mod_tw <- dsm(count~s(X, Y),
              ddf.obj=fit$model, 
              segment.data=sim.segdata, 
              observation.data=sim.obsdata, 
              family=tw(),
              transect="point")

summary(mod_tw)
par(mfrow = c(2,2))
gam.check(mod_tw)

par(mfrow = c(1,1))
plot(density, region)
plot(mod_tw)


# Preddata
library("raster")
library("rgeos")
library("dismo")

# Create an empty raster
grid <- raster(extent(sf::st_bbox(region@region)))
# Choose its resolution. 500 m in both X and Y (truncation distance)
res(grid) <- 60
# Transform this raster into a polygon and you will have a grid
gridpolygon <- rasterToPolygons(grid)
# convert to spatial object
sp.region <- as_Spatial(region@region)
# Intersect our grid with shape
pred.grid <- intersect(gridpolygon,sp.region)
# Plot the intersected shape to check if everything is fine.
plot(pred.grid)


# create the data.frame for prediction
preddata <- as.data.frame(matrix(NA, ncol=3, nrow=dim(pred.grid@data)[1]))
colnames(preddata) <- c("X", "Y", "area")
for (i in 1:dim(pred.grid@data)[1]){
  preddata[i, c("X", "Y")] <- pred.grid@polygons[[i]]@labpt
  preddata[i, c("area")] <- pred.grid@polygons[[i]]@area/(1000^2)
}

mod_tw_pred <- predict(mod_tw, preddata, preddata$area)

# ***********************
# Plotting needs fixed!!!!

grid_plot_obj <- function(shape,fill, name){
  
  # what data were supplied?
  names(fill) <- NULL
  row.names(fill) <- NULL
  data <- data.frame(fill)
  names(data) <- name
  
  # ! need to give the right name of the shapefile
  sp <- shape
  spdf <- SpatialPolygonsDataFrame(sp, data)
  spdf@data$id <- rownames(spdf@data)
  spdf.points <- fortify(spdf, region="id")
  spdf.df <- join(spdf.points, spdf@data, by="id")
  
  # store the x/y even when projected and labelled as "long" and "lat"
  spdf.df$x <- spdf.df$long
  spdf.df$y <- spdf.df$lat
  
  geom_polygon(aes_string(x="x",y="y",fill=name, group="group"), data=spdf.df)
}


# make the plot
pcount_tw <- ggplot() +
  grid_plot_obj(pred.grid, mod_tw_pred, "Density") + 
  scale_fill_gradient(low="white", high="chocolate4")  +
  coord_equal() + theme_minimal() +
  geom_path(aes(x=x, y=y), data=survey.area) +
  geom_point(aes(x = x, y = y, group="Point"), data = survey@dist.data, colour = "black") +
  labs(fill="Density")

pcount_tw


