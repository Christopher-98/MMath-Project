# Class definition

setClass("DSM.Simulation", representation(reps = "numeric",
                                      design = "Survey.Design",
                                      population.description = "Population.Description",
                                      detectability = "Detectability",
                                      ds.analysis = "DS.Analysis",
                                      ds.simulations = "Simulation",
                                      add.options = "list",
                                      ddf.param.ests = "array",
                                      results = "list",
                                      warnings = "list"))


setClass("DSM.Simulation",
         slots = list(DSM.model = "",
                      DSM.estimates = "numeric",
                      Prediction.Data = "data.frame"),
         contains = "DS.Simulation")

# to include:
# coverage, density estimates, accuracy, bias, preddata, predictions

# need new class definitions for 

# run.spatial.simulation <- based on run.simulation
# single.spatial.sim.loop <- based on single.sim.loop


