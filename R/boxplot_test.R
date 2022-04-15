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
#' @param type
#' String, (default `"all"`) possible values are `"all"`, "ibest" or "best". "all" 
#' shows all the configurations included in the test, "best" shows the elite 
#' configurations of the last iteration and "ibest" shows the elite configurations 
#' of each iteration. Note that "ibest" requires that irace includes the iteration
#' elites in the testing.
#' 
#' @template arg_rpd
#' 
#' @param show_points
#' Logical, (default TRUE) TRUE to plot performance points together with the box plot.
#' 
#' @template arg_filename
#' 
#' @return box plot
#'
#' @examples
#' 
#' boxplot_test(iraceResults)
#' @export
boxplot_test <- function(irace_results, type = c("all", "ibest", "best"),
                         rpd = TRUE, show_points=TRUE, filename = NULL)
{
  type <- match.arg(type)
  
  # verify that test this in irace_results
  if (!("testing" %in% names(irace_results))) {
    stop("Error: irace_results does not contain the testing data")
  }
  
  if (type=="ibest" && !irace_results$scenario$testIterationElites) {
    warning("irace data does not contain iteration elites testing, changing plot type to \"best\"")
    type <- "best"
  }
  
  experiments <- irace_results$testing$experiments
  
  if (type=="all" || type=="ibest") {
    id_configurations <- irace_results$allElites
  } else {
    id_configurations <- irace_results$allElites[[length(irace_results$allElites)]]
    type <- "all"
  }
  
  boxplot_performance(experiments = experiments,
                      allElites = id_configurations,
                      type = type,
                      first_is_best = TRUE,
                      rpd = rpd, 
                      show_points = show_points,
                      filename = filename)
}
