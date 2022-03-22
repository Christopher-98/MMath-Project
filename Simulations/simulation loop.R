
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

source('Regions/North Sea Strata Line zigzag.R') 

#source('Regions/North Sea extreme Line.R') 

#source('Regions/North Sea Break Line.R')

#source('Regions/Montrave region parallel line.R') 

#source('Regions/Montrave region random line.R')

#source('Regions/Montrave region zigzag.R')

#source('Regions/North Sea R2 estimator.R')

#source('Regions/North Sea O2 estimator.R')

#source('Regions/North Sea O3 estimator.R')

# select prediction grid to use:
#   standard is cropped to study region
#   outside region covers the entire bounding box of the study region

source('Simulations/prediction grid.R')

#source('Simulations/outside region prediction grid.R')

# set transect type based on survey design
if (class(design) == "Line.Transect.Design") {transect.type <- 'line'
} else {transect.type <- 'point'}

sim <- make.simulation(reps = 5000,
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
  
  # create a realisation of a survey
  survey <- run.survey(sim)
  
  # get the data in the form required by dsm
  dsm.data <- generate.dsm.data(region, survey, transect.type)
  
  obsdata <- dsm.data$obsdata
  
  segdata <- dsm.data$segdata
  
  estimates$detections[j] <- nrow(obsdata)
  
  # distance sampling model
  ds.mod <- suppressMessages(ds(survey@dist.data,
	                     truncation=design.trunc,
	                     transect=transect.type,
	                     formula=~1,
	                     key=detect@key.function,
	                     adjustment=NULL,
	                     er.var = sim@ds.analysis@er.var)) 
                       # some er.var not working, namely R4, S1, S2, O1.
  
  # save results to estimates
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
  dsm.mod.var <- dsm.var.gam(dsm.mod,
                            pred.data = preddata,
                            off.set = preddata$area)
  dsm.sum <- summary(dsm.mod.var)
  
  #calculate dsm confidence intervals
  ci.term <- exp(qnorm(1-0.05/2) * sqrt(log(1+dsm.sum$cv**2)))
  dsm.ci <- c(dsm.sum$pred.est / ci.term, dsm.sum$pred.est * ci.term)
  
  #save estimates 
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

write.csv(estimates, file = paste0('Estimates/',region@region.name, mods,transect.type,'zigzag.csv'))
