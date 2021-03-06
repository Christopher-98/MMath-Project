# MMath-Project

Folders:

Estimates: Contains all of the data generated by the simulation for each difference region profile

Regions: Contains all region profiles used for simulations

Results: Contains results of analysis on estimates data for each simulation

Reports:

  project_report.Rmd: project report written in R markdown

  plots folder: contains all plots used in the project report
  
  references.bib: file containing references for use in project report
  
  Report code Folder: contains code to produce plots required in report

Simulations:

  Class definition.R: possible new class layout for simulation
  
  helper functions.R: functions to assist in running the simulation loop and keep code                        concise
  prediction grid.R: creates prediction grid over specified region in simulation
  
  simulation loop.R: file which runs simulation of DS and DSM methods
  
  run.spatial.simulation.R: attempt at modifying existing simulation to add DSM
  
  single.spatial.sim.loop.R: attempt at modifying existing loop stage to add DSM
  
Testing:

  creating segments.R: testing to ensure segments are correctly constructed
  
  density plotting.R: constructing plots of density surface produced by DSM model
  
  estimate analysis.R: analysing estimates produced by simulation then writing to                             results folder
  parallel sim.R: attempt to run simulation in parallel, not working currently
  
  Starting point.R / starting point2.R : initial starting point provided by LHMarshall                   on DS construction on which to begin DSM simulation fitting. 
  
  
  
  