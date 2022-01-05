library(dsm)
library(dsims)
#library(rgeos)
#library(dismo)
library(sf)
library(Distance)

source('helper functions.R')

# load appropriate region, design and density

#source('default region.R')

source('default region line.R')  # 1027.302 from 50

#source('North Sea region.R') # 1081.279 from 50

#source('North Sea Strata.R') # 1096.36 from 50

#source('North Sea region Line.R') # 1145.379 from 50

source('prediction grid.R')

# set transect type based on survey design
if (class(design) == "Line.Transect.Design") {transect.type <- 'line'
} else {transect.type <- 'point'}

sim <- make.simulation(reps = 100,
                       design = design,
                       population.description = pop.desc,
                       detectability = detect,
                       ds.analysis = analyses)

estimates <- list(ds.est = c(rep(NA, sim@reps)),
                  dsm.est = c(rep(NA, sim@reps)),
                  dsm.var = c(rep(NA, sim@reps)),
                  dsm.dev = c(rep(NA, sim@reps)))

mods <- 0

for (j in 1:sim@reps) {
  
  message("\r", j, " out of ", sim@reps,  " reps ",mods, " models successful \r", appendLF = FALSE)
  
  survey <- run.survey(sim)
  
  # is this needed?
  fit <- analyse.data(analyses, data.obj = survey)
  
  obsdata <- survey@dist.data[!is.na(survey@dist.data$object),]
  
  # section for it line transects to split into segments
  if (transect.type == 'line') {
    
    # extract transects
    samplers <- survey@transect@samplers
    
    # split into segments of 2*truncation distance and separate
    # into individual line strings
    segs <- stdh_cast_substring(st_segmentize(samplers,
                                              dfMaxLength = 2*design.trunc),
                                to = "LINESTRING")
    
    segs$Effort <- as.numeric(st_length(segs))
    segs$Sample.Label <- 1:length(segs$transect)
    
    # create polygons based on truncation distance from line
    # Note: mitre may need set for eszigzagcom studies
    poly <- st_buffer(segs, dist = design.trunc, endCapStyle = 'FLAT')
    
    # Find the areas for each segment, using intersection with region
    # to account for study area edge profile
    segs$Area <- as.numeric(st_area(st_intersection(poly, region@region)))
    
    segs <- st_centroid(segs)
    
    segdata <- cbind(as.data.frame(st_drop_geometry(segs)),
                      st_coordinates(segs))
    
    # link obsdata to the segment id's
    obsdata <- st_as_sf(obsdata, coords = c('x','y'))
    
    # remove transects as sample labels
    obsdata$Sample.Label<- NULL
    
    #set crs for consistency
    st_crs(obsdata) <- st_crs(segs)
    
    obsdata<- st_join(obsdata, segs, join = st_nearest_feature)
    
    obsdata <- st_drop_geometry(obsdata)
    
    
  } else {
    
    # For point designs the segments are already defined
    segs <- survey@transect@samplers
    
    segs$Effort <- 1
    segs$Sample.Label <- segs$transect
    segs$transect <- NULL
    
    poly <- st_buffer(segs,design.trunc)
    
    segs$Area <- as.numeric(st_area(st_intersection(poly, region@region)))
    
    segdata <- cbind(as.data.frame(st_drop_geometry(segs)),
                     st_coordinates(segs))
  }
  
  obsdata <- obsdata[,c("object", "Sample.Label", "distance")]
  
  obsdata$size <- 1
  
  # distance sampling model
  ds.mod <- suppressMessages(ds(survey@dist.data,
	                     truncation=design.trunc,
	                     transect=transect.type,
	                     formula=~1,
	                     key=detect@key.function,
	                     adjustment=NULL))
  estimates$ds.est[j] <- last(ds.mod$dht$individuals$N$Estimate)
  
  # density model
  dsm.mod <- dsm(count~s(X, Y,k = sum(survey@transect@samp.count)),
                 ddf.obj=fit$model,
                 segment.data=segdata,
                 segment.area = segdata$Area,
                 observation.data=obsdata,
                 family = tw(),
                 transect=transect.type)
  
  mods <- mods + 1
  
  # obtain the abundance estimate
  # needs checked to ensure is close to original population
  mod_tw_est <- dsm.var.gam(dsm.mod,
                            pred.data = preddata,
                            off.set = preddata$area)
  
  estimates$dsm.est[j] <- unlist(mod_tw_est$pred)
  
  estimates$dsm.var[j] <- mod_tw_est$pred.var
  estimates$dsm.dev[j] <- summary(dsm.mod)$dev.expl

}
plot(survey, region)
plot(survey)

write.csv(estimates, file = paste0('estimates.',region@region.name, sim@reps,'.csv'))

hist(estimates$dsm.est, breaks = 50)
hist(estimates$ds.est, breaks = 50)
hist(estimates$dsm.var, breaks = 50)
hist(estimates$dsm.dev, breaks = 50)

mean(estimates$dsm.est)
mean(estimates$ds.est)

mean(estimates$dsm.var)



# for 100 sims without taking intersection of region and polygons
# 1113.42
# 996.4175

# 

sd(estimates$dsm.est)
sd(estimates$ds.est)