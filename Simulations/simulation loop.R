
source('Simulations/helper functions.R')

# load appropriate region, design and density

#source('Regions/default region.R')

#source('Regions/default region line.R')  

#source('Regions/default region line zigzag.R')

#source('Regions/North Sea region.R')

#source('Regions/North Sea region Line.R')

#source('Regions/North Sea region Line zigzag.R')

#source('Regions/North Sea Strata.R')

#source('Regions/North Sea Strata Line.R') 

#source('Regions/North Sea Strata Line zigzag.R') 

#source('Regions/North Sea extreme Line.R') 

#source('Regions/North Sea Break Line.R')

source('Regions/Montrave region parallel line.R') 

source('Simulations/prediction grid.R')

# set transect type based on survey design
if (class(design) == "Line.Transect.Design") {transect.type <- 'line'
} else {transect.type <- 'point'}

sim <- make.simulation(reps = 100,
                       design = design,
                       population.description = pop.desc,
                       detectability = detect,
                       ds.analysis = analyses)

estimates <- list(ds.est = vector(,sim@reps),
                  ds.se = vector(,sim@reps),
                  ds.ci.lo = vector(,sim@reps),
                  ds.ci.up = vector(,sim@reps),
                  dsm.est = vector(,sim@reps),
                  dsm.var = vector(,sim@reps),
                  dsm.dev = vector(,sim@reps),
                  dsm.se = vector(,sim@reps),
                  dsm.ci.lo = vector(,sim@reps),
                  dsm.ci.up = vector(,sim@reps),
                  detections = vector(,sim@reps))

mods <- 0

for (j in 1:sim@reps) {
  
  message("\r", j, " out of ", sim@reps,  " reps ",mods, " models successful \r", appendLF = FALSE)
  
  survey <- run.survey(sim)
  
  obsdata <- survey@dist.data[!is.na(survey@dist.data$object),]
  
  estimates$detections[j] <- nrow(obsdata)
  
  segs <- to_segments(region,
                      survey,
                      transect.type)
  
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
  estimates$ds.se[j] <- last(ds.mod$dht$individuals$N$se)
  estimates$ds.ci.lo[j] <- last(ds.mod$dht$individuals$N$lcl)
  estimates$ds.ci.up[j] <- last(ds.mod$dht$individuals$N$ucl)
  
  # density model
  dsm.mod <- dsm(count~s(X, Y, k = sum(survey@transect@samp.count)),
                 ddf.obj = ds.mod,
                 segment.data=segdata,
                 segment.area = segdata$Area,
                 observation.data=obsdata,
                 family = tw(),
                 transect=transect.type)
  
  mods <- mods + 1
  
  # obtain the abundance estimate
  # needs checked to ensure is close to original population
  dsm.mod.var <- dsm.var.gam(dsm.mod,
                            pred.data = preddata,
                            off.set = preddata$area)
  dsm.sum <- summary(dsm.mod.var)
  
  ci.term <- exp(qnorm(1-0.05/2) * sqrt(log(1+dsm.sum$cv**2)))
  dsm.ci <- c(dsm.sum$pred.est / ci.term,
                 dsm.sum$pred.est * ci.term)
  
  estimates$dsm.est[j] <- dsm.sum$pred.est
  estimates$dsm.var[j] <- dsm.mod.var$pred.var
  estimates$dsm.se[j] <- last(dsm.sum$se)
  estimates$dsm.ci.lo[j] <- dsm.ci[1]
  estimates$dsm.ci.up[j] <- dsm.ci[2]
  estimates$dsm.dev[j] <- summary(dsm.mod)$dev.expl

}


plot(survey, region)

hist(estimates$dsm.est, breaks = 50)
hist(estimates$ds.est, breaks = 50)

mean(estimates$dsm.est)
mean(estimates$ds.est)

#write.csv(estimates, file = paste0('Estimates/',region@region.name, sim@reps,transect.type,'.csv'))
