#' Box Plot Testing Performance
#'
#' Creates a box plot that displays the performance of a set of configurations on the test instances.
#' 
#' The performance data is obtained from the test evaluations performed 
#' by irace. Note that the testing is not a default feature in irace and should 
#' be enabled in the setup (see the irace package user guide for more details).
#'
#' @template arg_irace_results
#'
#' @param type String, (default `"all"`) possible values are `"all"`, "ibest" or "best". "all" shows all the configurations included in the test, "best" shows the elite configurations of the last iteration and "ibest" shows the elite configurations of each iteration (requires that irace includes the iteration elites in the testing).
#' 
#' @param ... Other arguments passed to [boxplot_performance()].
#'
#' @template ret_boxplot
#'
#' @seealso [boxplot_training()] [boxplot_performance()]
#'
#' @examples
#' iraceResults <- read_logfile(system.file(package="iraceplot", "exdata",
#'                                          "guide-example.Rdata", mustWork = TRUE))
#' boxplot_test(iraceResults)
#' @export
boxplot_test <- function(irace_results, type = c("all", "ibest", "best"), ...)
{
  type <- match.arg(type)
  irace_results <- read_logfile(irace_results)
  if (!has_testing_data(irace_results))
    cli_abort("{.field irace_results} does not contain the testing data")
    
  if (type=="ibest" && !irace_results$scenario$testIterationElites) {
    iraceplot_warn("irace data does not contain iteration elites testing,",
                   " changing plot type to {.code 'best'}")
    type <- "best"
  }

  id_configurations <- lapply(irace_results$allElites, utils::head, irace_results$scenario$testNbElites)
  if (type == "best") {
    id_configurations <- id_configurations[[length(id_configurations)]]
    type <- "all"
  }
  boxplot_performance(experiments = irace_results$testing$experiments,
                      allElites = id_configurations,
                      type = type, ...)
}
