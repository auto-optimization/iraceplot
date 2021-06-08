#' Frequency and Density plot based on its iteration
#'
#' @description
#' The function will return two plots, one of frequency and
#' one of density of the numerical parameter, in case of being a categorical parameter
#' it will only show one of frequency
#'
#' @template arg_irace_results
#'
#' @param parameter
#' String, value of the parameter to be analyzed (example: parameter = "alpha")
#'
#' @param file_name
#' String, A pdf will be created in the location and with the
#' assigned name (example: "~/patch/example/file_name")
#'
#' @return frequency and density plot
#' @export
#'
#' @examples
#' sampling_model_compare(iraceResults, parameter = "alpha")
sampling_model_compare <- function(irace_results, parameter, file_name = NULL) {
  vectorPlot <- NULL

  # verify that param_names is other than null
  if (!is.null(parameter)) {
    # verify that param_names contain the data entered
    if ("FALSE" %in% names(table(parameter %in% irace_results$parameters$names))) {
      return("Some wrong parameter entered")
    }
    # verify that param_names contain more than one parameter
    else if (length(parameter) != 1) {
      return("You can only enter one parameter")
    }
  }

  if (irace_results$parameters$types[[parameter]] == "c") {
    p <- sampling_frequency_iteration(irace_results, parameter)
    vectorPlot[1] <- list(p)
  } else {
    p <- sampling_frequency_iteration(irace_results, parameter)
    p <- p + theme(legend.position = "none")
    vectorPlot[1] <- list(p)
    q <- sampling_density(irace_results, parameter)
    vectorPlot[2] <- list(q)
  }

  # If the value in file_name is added the pdf file is created
  if (!is.null(file_name)) {
    if (length(vectorPlot) == 1) {
      ggsave(file_name, plot = do.call("grid.arrange", c(vectorPlot, ncol = 1)))
    } else {
      ggsave(file_name, plot = do.call("grid.arrange", c(vectorPlot, ncol = 2)))
    }
    # If you do not add the value of file_name, the plot is displayed
  } else {
    if (length(vectorPlot) == 1) {
      do.call("grid.arrange", c(vectorPlot, ncol = 1))
      return(vectorPlot)
      # grid.arrange(vectorPlot,ncol=1)
    } else {
      do.call("grid.arrange", c(vectorPlot, ncol = 2))
      return(vectorPlot)
      # grid.arrange(vectorPlot,ncol=2)
    }
  }
}
