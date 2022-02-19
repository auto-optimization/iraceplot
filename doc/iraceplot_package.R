## ----echo=FALSE, prompt=FALSE, message=FALSE----------------------------------

library(iraceplot, quietly = TRUE)
library(plotly)
load("../data/iraceResults.rda")


## ----eval=FALSE---------------------------------------------------------------
#  library(iraceplot)

## ----eval=FALSE---------------------------------------------------------------
#  load("~/path/to/irace.Rdta")

## ----fig.align="center", fig.width= 7, message=FALSE, prompt=FALSE------------
parallel_coord(iraceResults)

## ----fig.align="center", fig.width= 7, message=FALSE, prompt=FALSE------------
parallel_coord(iraceResults, iterations=1:iraceResults$state$nbIterations)

## ----fig.align="center", fig.width= 7, message=FALSE, prompt=FALSE------------
parallel_coord(iraceResults, iterations=1:9, only_elite=FALSE)

## ----fig.align="center", fig.width= 7, message=FALSE, prompt=FALSE------------
all_elite <- iraceResults$allConfigurations[unlist(iraceResults$allElites),]
parallel_coord2(all_elite, iraceResults$parameters)

## ----fig.align="center", fig.width= 7, fig.height=7, message=FALSE, prompt=FALSE----
parallel_cat(irace_results = iraceResults, iterations = c(3,4,5) )

## ----fig.align="center", fig.width= 7, fig.height=6, message=FALSE, prompt=FALSE----
parallel_cat(irace_results = iraceResults, 
             param_names=c("algorithm", "localsearch", "dlb", "nnls"))

## ----fig.align="center", fig.width= 7, message=FALSE, prompt=FALSE------------
sampling_pie(irace_results = iraceResults)

## ----fig.align="center", fig.width= 7, fig.height=7, message=FALSE, prompt=FALSE----
 sampling_frequency(iraceResults)

## ----fig.align="center", fig.width= 7, message=FALSE, prompt=FALSE------------
 sampling_frequency(iraceResults, param_names = c("beta"))

## ----fig.align="center", fig.width= 7, fig.height=6---------------------------
sampling_frequency_iteration(iraceResults, param_name = "beta")

## ----fig.align="center", fig.width= 7, fig.height=6---------------------------
sampling_heatmap(iraceResults, param_names = c("beta","alpha"))

## ----fig.align="center", fig.width= 7, fig.height=6---------------------------
sampling_heatmap2(iraceResults$allConfigurations, iraceResults$parameters, 
                  param_names = c("algorithm","q0"), sizes=c(0,5))

## ----fig.align="center", fig.width= 7-----------------------------------------
sampling_distance(iraceResults, t=0.05)

## ----fig.align="center", fig.width=7------------------------------------------
boxplot_test(iraceResults, type="best")

## ----fig.align="center", fig.width=7------------------------------------------
boxplot_test(iraceResults, type="all", show_points=FALSE)

## ----fig.align="center", fig.width=7------------------------------------------
scatter_test(iraceResults, id_configurations = c(808,809), interactive=TRUE)

## ----fig.align="center", fig.width=7------------------------------------------
boxplot_training(iraceResults)

## ----fig.align="center", fig.width=7------------------------------------------
scatter_training(iraceResults, id_configurations = c(808,809), interactive=TRUE)

## ----fig.align="center", fig.width=7------------------------------------------
plot_experiments_matrix(iraceResults, interactive = TRUE)

## ----fig.align="center", fig.width=7------------------------------------------
plot_model(iraceResults, param_name="algorithm")

## ----fig.align="center", fig.width=7, fig.height=6, message=FALSE, prompt=FALSE, results='hide'----
plot_model(iraceResults, param_name="alpha")

## ----fig.align="center", eval=FALSE-------------------------------------------
#  report(iraceResults, filename="report")

