#' Box Plot Training
#'
#' The `boxplot_training` function creates a box plot that displays the performance
#' of a set of configurations on the training instances. Performance data is obtained 
#' from the evaluations performed by irace during the execution process. This implies 
#' that the number of evaluations can differ between configurations. 
#' 
#'
#' @template arg_irace_results
#'
#' @param iteration
#' Numeric, iteration number that should be included in the plot (example: iteration = 5)
#' When no iteration and no id_condigurations are provided, the iterations is assumed to be
#' the last one performed by irace. 
#' 
#' The performance data is obtained from the evaluations performed by irace 
#' during the execution process. This implies that the number of evaluations 
#' can differ between configurations due to the elimination process applied by 
#' irace. This plot, consequently, does not provide a complete compaarison of
#' two configurations, for a fair comparison use the test data plot.
#'
#' @param id_configurations
#' Numeric vector, configurations ids whose performance should be included in the plot.
#' If no ids are provided, the configurations ids are set as the elite configuration ids 
#' of the selected iteration (last iteration by default) 
#' (example: id_configurations = c(20,50,100,300,500,600,700)).
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
#' @export
#'
#' @examples
#' boxplot_training(iraceResults)
#' boxplot_training(iraceResults, rpd = FALSE)
#' boxplot_training(iraceResults, iteration = 5)
#' boxplot_training(iraceResults, id_configurations = c(20, 50, 100, 300, 500, 600, 700))
#' 
boxplot_training <- function(irace_results, iteration = NULL, id_configurations = NULL, 
                             rpd = TRUE, show_points=TRUE, filename = NULL) {

  if (!is.null(iteration) & !is.null(id_configurations)) {
    stop("Error: cannot use id_configurations and iteration at the same time\n")
  }
  
  # It is checked if the filename argument was added
  if (!is.null(iteration)) {
    # We verify that iteration is within the range of values it can take
    if (iteration < 0 || iteration > length(irace_results$allElites)) {
      stop("Error: iteration number out of range\n")
    }
  } else{
    iteration <- length(irace_results$allElites)
  }
  
  # Check configurations
  if (!is.null(id_configurations)) {
    if (any(!(as.character(id_configurations) %in% colnames(irace_results$experiments)))) {
      stop(paste("Error: provided configurations id not found in experiments\n"))
    } 
  } else {
    id_configurations <- irace_results$allElites[[iteration]]
  }
  
  # A table is created with the values of all elite configurations of the id of the requested iteration
  experiments <- irace_results$experiments
  
  boxplot_performance(experiments = experiments,
                      allElites = id_configurations,
                      type = "all",
                      first_is_best = TRUE,
                      rpd = rpd, 
                      show_points = show_points,
                      filename = filename)

}
