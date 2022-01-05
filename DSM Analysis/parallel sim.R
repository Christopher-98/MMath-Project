#Required Libraries
library(doParallel)
library(foreach)
library(parallel)


# Create/register a cluster of 4 workers
cl <- parallel::makeCluster(spec = 3, type = "PSOCK", setup_strategy = "sequential")
doParallel::registerDoParallel(cl)

clusterEvalQ(cl = cl, expr = {library(dsims)
  library(dsm)})
# Run code in parallel 
Outcomes <- foreach(j = 1:10, .combine = "c") %dopar% {

  survey <- run.survey(sim)

  fit <- analyse.data(analyses, data.obj = survey)

  segdata <- data.frame(Sample.Label = survey@transect@samplers$transect,
                        Effort = 1,
                        X = sf::st_coordinates(survey@transect@samplers)[,1],
                        Y = sf::st_coordinates(survey@transect@samplers)[,2])

  # simulated observed data
  sim.obsdata <- survey@dist.data[,c("object", "Sample.Label", "distance")]
  sim.obsdata$size <- 1
  sim.obsdata <- sim.obsdata[!is.na(sim.obsdata$object),]

  # density model
  mod_tw <- dsm(count~s(X, Y, k=survey@transect@samp.count),
                ddf.obj=fit$model,
                segment.data=segdata,
                observation.data=sim.obsdata,
                family=tw(),
                transect="point")
  # obtain the abundance estimate
  # needs checked to ensure is close to original population
  mod_tw_est <- dsm.var.gam(mod_tw,
                            pred.data = preddata,
                            off.set = preddata$area)

  mod_tw_est$pred[[1]]

}

#Stop Cluster
parallel::stopCluster(cl)
