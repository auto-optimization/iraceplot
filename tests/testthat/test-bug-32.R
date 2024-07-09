
test_that("bug32", {
  
  ### To regenerate the data
  regenerate_data <- function() {
    library(irace)
    parameters <- irace:::readParameters(text='p "" r (0,1)')
    target.runner <- function(experiment, scenario)
      list(cost = experiment[['configuration']]['p'], call = toString(experiment))

    scenario <- list(parameters = parameters, targetRunner = target.runner,
      instances=1:5, maxExperiments = 250, logFile = "bug32.Rdata")
    scenario <- checkScenario (scenario)
    confs <- irace(scenario = scenario)
  }

  summarise_by_instance(read_logfile("bug32.Rdata"))
})

