## ----echo=FALSE, prompt=FALSE, message=FALSE, warning=FALSE-------------------

library(iraceplot, quietly = TRUE)
library(plotly)
load("../data/iraceResults.rda")


## ----eval=FALSE---------------------------------------------------------------
#  library(iraceplot)

## ----eval=FALSE---------------------------------------------------------------
#  load("~/path/to/irace.Rdta")

## ----fig.align="center", fig.width=7, eval=FALSE------------------------------
#  boxplot_test(iraceResults, type="best")

## ----fig.align="center", fig.width= 7, message=FALSE, prompt=FALSE, eval=FALSE----
#  parallel_coord(iraceResults)

## ----fig.align="center", fig.width= 7, message=FALSE, prompt=FALSE, eval=FALSE----
#  all_elite <- iraceResults$allConfigurations[unlist(iraceResults$allElites),]
#  parallel_coord2(all_elite, iraceResults$parameters)

## ----fig.align="center", fig.width= 7, fig.height=6, message=FALSE, prompt=FALSE, eval=FALSE----
#  parallel_cat(irace_results = iraceResults,
#               param_names=c("algorithm", "localsearch", "dlb", "nnls"))

## ----fig.align="center", fig.width= 7, message=FALSE, prompt=FALSE, eval=FALSE----
#  sampling_pie(irace_results = iraceResults, param_names=c("algorithm", "localsearch", "alpha", "beta", "rho"))

## ----fig.align="center", fig.width= 7, message=FALSE, prompt=FALSE, eval=FALSE----
#   sampling_frequency(iraceResults, param_names = c("beta"))

## ----fig.align="center", fig.width= 7, fig.height=7, message=FALSE, prompt=FALSE, eval=FALSE----
#   sampling_frequency2(iraceResults$allConfigurations, iraceResults$parameters, param_names = c("alpha"))

## ----fig.align="center", fig.width= 7, fig.height=6, eval=FALSE---------------
#  sampling_frequency_iteration(iraceResults, param_name = "beta")

## ----fig.align="center", fig.width= 7, fig.height=6, eval=FALSE---------------
#  sampling_heatmap(iraceResults, param_names = c("beta","alpha"))

## ----fig.align="center", fig.width= 7, fig.height=6, eval=FALSE---------------
#  sampling_heatmap2(iraceResults$allConfigurations, iraceResults$parameters,
#                    param_names = c("localsearch","nnls"), sizes=c(0,5))

## ----fig.align="center", fig.width= 7, eval=FALSE-----------------------------
#  sampling_distance(iraceResults, t=0.05)

## ----fig.align="center", fig.width=7, eval=FALSE------------------------------
#  boxplot_test(iraceResults, type="best")

## ----fig.align="center", fig.width=7, eval=FALSE------------------------------
#  scatter_test(iraceResults, id_configurations = c(808,809), interactive=TRUE)

## ----fig.align="center", fig.width=7, eval=FALSE------------------------------
#  boxplot_training(iraceResults)

## ----fig.align="center", fig.width=7, eval=FALSE------------------------------
#  scatter_training(iraceResults, id_configurations = c(808,809), interactive=TRUE)

## ----fig.align="center", fig.width=7, eval=FALSE------------------------------
#  boxplot_performance(iraceResults$experiments, allElites=list(c(803,808), c(809,800)), first_is_best = TRUE)

## ----fig.align="center", fig.width=7, eval=FALSE------------------------------
#  scatter_performance(iraceResults$experiments, id_configurations = c(803,809), interactive=TRUE)

## ----fig.align="center", fig.width=7, eval=FALSE------------------------------
#  plot_experiments_matrix(iraceResults, interactive = TRUE)

## ----fig.align="center", fig.width=7, eval=FALSE------------------------------
#  plot_model(iraceResults, param_name="algorithm")

## ----fig.align="center", fig.width=7, fig.height=6, message=FALSE, prompt=FALSE, results='hide', eval=FALSE----
#  plot_model(iraceResults, param_name="alpha")

## ----fig.align="center", eval=FALSE-------------------------------------------
#  report(iraceResults, filename="report")

