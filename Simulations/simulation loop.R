
source('Simulations/helper functions.R')

# load appropriate region, design and density

#source('Regions/default region.R')

#source('Regions/default region line.R')  

#source('Regions/default region line zigzag.R')

#source('Regions/North Sea region.R') 

#source('Regions/North Sea Strata.R') 

#source('Regions/North Sea region Line.R')

#source('Regions/North Sea Strata Line.R') 

source('Regions/North Sea Strata Line zigzag.R') 

source('Simulations/prediction grid.R')

# set transect type based on survey design
if (class(design) == "Line.Transect.Design") {transect.type <- 'line'
} else {transect.type <- 'point'}

sim <- make.simulation(reps = 1000,
                       design = design,
                       population.description = pop.desc,
                       detectability = detect,
                       ds.analysis = analyses)

estimates <- list(ds.est = c(rep(NA, sim@reps)),
                  dsm.est = c(rep(NA, sim@reps)),
                  dsm.var = c(rep(NA, sim@reps)),
                  dsm.dev = c(rep(NA, sim@reps)),
                  detections = c(NULL))

mods <- 0

for (j in 1:sim@reps) {
  
  message("\r", j, " out of ", sim@reps,  " reps ",mods, " models successful \r", appendLF = FALSE)
  
  survey <- run.survey(sim)
  
  obsdata <- survey@dist.data[!is.na(survey@dist.data$object),]
  
  estimates$detections[j] <- nrow(obsdata)
  
  # obtain region boundaries for use in calculating segment areas
  seg.region <- st_union(region@region)
  
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
    segs$Area <- as.numeric(st_area(st_intersection(poly, seg.region)))
    
    segs <- st_centroid(segs)
    
  } else {
    
    # For point designs the segments are already defined
    segs <- survey@transect@samplers
    
    segs$Effort <- 1
    segs$Sample.Label <- segs$transect
    segs$transect <- NULL
    
    poly <- st_buffer(segs,design.trunc)
    
    segs$Area <- as.numeric(st_area(st_intersection(poly, seg.region)))
    
  }
  
  segdata <- cbind(as.data.frame(st_drop_geometry(segs)),
                   st_coordinates(segs))
  
  # link obsdata to the segments
  obsdata <- st_as_sf(obsdata, coords = c('x','y'))
  
  # remove existing transects as sample labels
  obsdata$Sample.Label<- NULL
  
  #set crs for consistency
  st_crs(obsdata) <- st_crs(segs)
  
  obsdata<- st_join(obsdata, segs, join = st_nearest_feature)
  obsdata <- st_drop_geometry(obsdata)
  
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
                 ddf.obj = ds.mod,
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

hist(estimates$dsm.est, breaks = 50)
hist(estimates$ds.est, breaks = 50)
hist(estimates$dsm.var, breaks = 50)
hist(estimates$dsm.dev, breaks = 50)
hist(estimates$detections, breaks = 50)

mean(estimates$dsm.est)
mean(estimates$ds.est)

mean(estimates$dsm.var)


write.csv(estimates, file = paste0('Estimates/',region@region.name, sim@reps,transect.type,'.csv'))


# for 100 sims without taking intersection of region and polygons
# 1113.42
# 996.4175

# 

sd(estimates$dsm.est)
sd(estimates$ds.est)