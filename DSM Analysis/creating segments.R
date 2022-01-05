# segmentising data

library(dsm)
library(dsims)
library(sf)


#  change region to be north sea shapefile
region <- make.region(region.name = "North Sea",
                      shape = "RegionPrj.shp")

cover <- make.coverage(region = region,
                       n.grid.points = 1000)
design.trunc = 10

design <- make.design(region = region,
                      transect.type = 'line',
                      design.angle = 90,
                      samplers = 25,
                      truncation = design.trunc,
                      coverage.grid = cover)


samplers <- generate.transects(design, region)

plot(region, samplers)


density <- make.density(region = region,
                        x.space = 25,
                        constant = 1)

density <- add.hotspot(density,
                       c(0, 600), 100, 3)
density <- add.hotspot(density,
                       c(100, 700), 300, 0.5)
density <- add.hotspot(density,
                       c(200, 900), 200, 1)
density <- add.hotspot(density, 
                       c(250, 350), 50, -0.5)

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
                             er.var = "R2",
                             truncation = design.trunc)

sim <- make.simulation(reps = 10,
                       design = design,
                       population.description = pop.desc,
                       detectability = detect,
                       ds.analysis = analyses)


survey <- run.survey(sim)
plot(survey, region)

fit <- analyse.data(analyses, data.obj = survey)

# segmentising data

# To extract coordinates to make segdata
survey.samp <- survey@transect@samplers

segs <- stdh_cast_substring(st_segmentize(survey.samp,
                                          dfMaxLength = 2*design.trunc), to = "LINESTRING")

segs$Effort <- as.numeric(st_length(segs))
segs$Sample.Label <- 1:length(segs$transect)

seg.lines <- segs

segs <- st_centroid(segs)

seg.data <- cbind(as.data.frame(st_drop_geometry(segs)),
                 st_coordinates(segs))


 poly <- st_buffer(segs, dist = design.trunc, endCapStyle = 'FLAT')
# points <- st_centroid(poly)
# areas <- as.numeric(st_area(st_intersection(poly, region@region)))
# 
# effort <- st_length(segs)
# 
# # plot(segs)
# # plot(poly)
# # plot(st_intersection(poly, region@region))
# # plot(points)
# 
# segdata <- data.frame(Sample.Label = 1:length(points$transect),
#                       Effort = effort,
#                       x = st_coordinates(points)[,1],
#                       y = st_coordinates(points)[,2],
#                       Area = areas)
# 


obs.data <- survey@dist.data[!is.na(survey@dist.data$object),]

obs.data <- st_as_sf(obs.data, coords = c('x','y'))
# remove transects as sample lables
obs.data$Sample.Label<- NULL

st_crs(obs.data) <- st_crs(segs)

obs <- st_join(obs.data, segs, join = st_nearest_feature)


pal <- rainbow(nrow(segs), s=.6, v=.9)[sample(1:nrow(segs),nrow(segs))]
p <- ggplot() +
  # geom_sf knows what to do with spatial data
  geom_sf(data=segs, aes(colour=Sample.Label), pch=21) +
  geom_sf(data=obs, size=0.5, aes(colour=Sample.Label)) 
p

obs <- st_drop_geometry(obs)

obs <- obs[,c("object", "Sample.Label", "distance")]

obs$size <- 1

head(obs)

# # observed data
# sim.obsdata <- survey@dist.data[,c("object", "Sample.Label", "distance")]
# sim.obsdata$size <- 1
# sim.obsdata <- sim.obsdata[!is.na(sim.obsdata$object),]

# create a prediction grid
# method from http://rfunctions.blogspot.co.uk/2014/12/how-to-create-grid-and-intersect-it.html

# Create an empty raster
grid <- raster(extent(sf::st_bbox(region@region)))
res(grid) <- design.trunc
gridpolygon <- rasterToPolygons(grid)
sp.region <- as_Spatial(region@region)
pred.grid <- raster::intersect(gridpolygon, sp.region)

plot(gridpolygon)
plot(sp.region)
# create the data.frame for prediction
preddata <- as.data.frame(matrix(NA, ncol=3, nrow=dim(pred.grid@data)[1]))
colnames(preddata) <- c("X", "Y", "area")
for (i in 1:dim(pred.grid@data)[1]){
  preddata[i, c("X", "Y")] <- pred.grid@polygons[[i]]@labpt
  preddata[i, c("area")] <- pred.grid@polygons[[i]]@area
}




###############################################################################



df_ht <- ds(survey@dist.data, truncation=design.trunc, transect="line",
            formula=~1, key="hn", adjustment=NULL)

df_ht$dht$individuals$N$Estimate


mod_tw <- dsm(count~s(X, Y),
              ddf.obj=fit$model,
              segment.data=seg.data,
              #segment.area = segdata$Area,
              observation.data=obs,
              family=tw(),
              transect="line")

summary(mod_tw)
gam.check(mod_tw)

mod_tw_pred <- dsm.var.gam(mod_tw,
                           pred.data = preddata,
                           off.set = preddata$area)

mod_tw_pred

######################## algorithm plan
# 
# ############################## notes
# predictions inc. if split into strata
# soup film smoothers
# checks to complete
# possible covariates- e.g. habitat
# comparison between distance design and dsm design

