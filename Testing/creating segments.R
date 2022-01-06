# segmentising data

library(dsm)
library(dsims)
library(Distance)
library(sf)
library(ggplot2)


region <- make.region(region.name = 'Default')

design.trunc = 60

design <- make.design(region = region,
                      transect.type = 'line',
                      design = 'eszigzagcom',
                      samplers = 12,
                      design.angle = 90,
                      bounding.shape = 'convex.hull',
                      truncation = design.trunc)


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

plot(detect, pop.desc)

sim <- make.simulation(reps = 10,
                       design = design,
                       population.description = pop.desc,
                       detectability = detect,
                       ds.analysis = analyses)


survey <- run.survey(sim)
plot(survey, region)

plot(survey)

fit <- analyse.data(analyses, data.obj = survey)

# segmentising data

source('helper functions.R')

# To extract coordinates to make segdata
survey.samp <- survey@transect@samplers

segs <- stdh_cast_substring(st_segmentize(survey.samp,
                                          dfMaxLength = 2*design.trunc), to = "LINESTRING")

segs$Effort <- as.numeric(st_length(segs))
segs$Sample.Label <- 1:length(segs$transect)

seg.lines <- segs

segs <- st_centroid(segs)

poly <- st_buffer(seg.lines, dist = design.trunc, endCapStyle = 'FLAT')

plot(poly)

poly <- st_intersection(poly, region@region)

plot(poly)

segs$Area <- as.numeric(st_area(poly))

seg.data <- cbind(as.data.frame(st_drop_geometry(segs)),
                 st_coordinates(segs))



obs.data <- survey@dist.data[!is.na(survey@dist.data$object),]

obs.data <- st_as_sf(obs.data, coords = c('x','y'))
# remove transects as sample lables
obs.data$Sample.Label<- NULL

st_crs(obs.data) <- st_crs(segs)

obs <- st_join(obs.data, segs, join = st_nearest_feature)


pal <- rainbow(nrow(segs), s=.6, v=.9)[sample(1:nrow(segs),nrow(segs))]
p <- ggplot() +
  # geom_sf knows what to do with spatial data
  geom_sf(data = poly, aes(colour = pal)) +
  geom_sf(data=segs, aes(colour=pal), pch=21) +
  geom_sf(data=obs, size=0.5)
p

obs <- st_drop_geometry(obs)

obs <- obs[,c("object", "Sample.Label", "distance")]

obs$size <- 1

head(obs)


# create a prediction grid
# method from http://rfunctions.blogspot.co.uk/2014/12/how-to-create-grid-and-intersect-it.html

source('prediction grid.R')


###############################################################################



df_ht <- ds(survey@dist.data, truncation=design.trunc, transect="line",
            formula=~1, key="hn", adjustment=NULL)

df_ht$dht$individuals$N$Estimate


mod_tw <- dsm(count~s(X, Y),
              ddf.obj=fit$model,
              segment.data=seg.data,
              segment.area = seg.data$Area,
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

