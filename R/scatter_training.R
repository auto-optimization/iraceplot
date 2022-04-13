#' Scatter Plot Training
#'
#' @description
#' The `scatter_training` function creates a scatter plot that displays the performance of two
#' configurations on the training performance. Each point in the plot represents an 
#' instance and the color of the points indicates if one configuration is better 
#' than the other.
#' 
#' The performance data is obtained from the evaluations performed by irace 
#' during the execution process, consequently the number of evaluations 
#' can differ between configurations due to the elimination process applied by 
#' irace. This plot only shows performance data only for instances in which both
#' configurations are executed.
#'
#' @template arg_irace_results
#'
#' @param id_configurations
#' Numeric vector, configuration ids whose performance should be displayed 
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
#' @examples
#' scatter_training(iraceResults, id_configurations = c(806, 809))
#' scatter_training(iraceResults, id_configurations = c(806, 809), rpd = FALSE)
#' @export
#' 
scatter_training <- function(irace_results, id_configurations, rpd = TRUE, 
                             filename = NULL, interactive = base::interactive()) {
  
  # Verify that a vector of length 2 is entered
  if (length(id_configurations) != 2) {
    stop("Error: You must provide a vector with 2 values\n")
  }
  # Verify that the entered id are within the possible range
  if (!(id_configurations[1] %in% irace_results$allConfigurations[,".ID."])) {
    stop(paste0("Error: id out of range", id_configurations[1], "\n"))
  } else if (!(id_configurations[2] %in% irace_results$allConfigurations[,".ID."])) {
    stop(paste("Error: id out of range", id_configurations[2], "\n"))
  }
  
  experiments <- irace_results$experiments
  
  instances.ids <- irace_results$state$.irace$instancesList[1:nrow(experiments), "instance"]
  instances.names <- basename(irace_results$scenario$instances[instances.ids])
  rownames(experiments) <- instances.names
  
  scatter_performance(experiments,
                      id_configurations=id_configurations,
                      rpd=rpd,
                      filename=filename, 
                      interactive = interactive)

}


