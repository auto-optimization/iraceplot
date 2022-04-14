#' Scatter Plot Testing
#'
#' @description
#' The `scatter_test` function creates a scatter plot comparing the performance of two 
#' configurations in the test instances. Each point in the plot represents an instance
#' and the color of the points indicates if one configuration is better than the other.
#' 
#' The performance data is obtained from the test evaluations performed by irace. 
#' Note that the testing is not a default feature in irace and should be enabled in 
#' the setup. 
#' 
#' Configuration ids provided in `id_configurations` should belong to 
#' elite configuration set evaluated in the test (see the irace package user 
#' guide for more details).
#'
#' @template arg_irace_results
#' @param id_configurations
#' String vector, configuration ids whose performance should be displayed 
#' (example: id_configurations = c("92","119"))
#' 
#' @template arg_rpd
#' 
#' @template arg_filename
#' 
#' @template arg_interactive
#'
#' @return `ggplot()` object
#'
#' @export
#'
#' @examples
#' scatter_test(iraceResults, id_configurations = c("92", "119"))
#' \dontrun{ 
#' scatter_test(iraceResults, id_configurations = c("92", "119"), rpd=FALSE)
#' }
  scatter_test <- function(irace_results, id_configurations, rpd = TRUE, 
                           filename = NULL, interactive = base::interactive()) {

  conf1 <- conf2 <- best <- instance <- x_val <- y_val <- point_text <- NULL
    
  # verify that test this in irace_results
  if (!("testing" %in% names(irace_results))) {
    stop("Error: irace_results does not contain the testing data\n")
  }
  # verify that the data is correct
  id_configurations <- as.character(id_configurations)
  if (length(id_configurations) != 2) {
    stop("Error: You must enter a vector with 2 values \n")
  } else if (!(id_configurations[1] %in% colnames(irace_results$testing$experiments))) {
    stop(paste("Error: Configuration", id_configurations[1], "not found\n"))
  } else if (!(id_configurations[2] %in% colnames(irace_results$testing$experiments))) {
    stop(paste("Error: Configuration", id_configurations[2], "not found\n"))
  }

  # the table is created with all the data from testing experiments
  experiments <- irace_results$testing$experiments
  rownames(experiments) <- basename(irace_results$scenario$testInstances)
  
  scatter_performance(experiments,
                      id_configurations=id_configurations,
                      rpd=rpd,
                      filename=filename, 
                      interactive = interactive)
  
}
