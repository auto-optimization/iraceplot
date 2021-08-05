#' Frequency and Density plot based on its iteration
#'
#' @description
#' The function will return a frequency plot used
#' for categorical data (its values are string, show a bar plot) or
#' numeric data (show a histogram and density plot) by each iteration
#'
#' @template arg_irace_results
#'
#' @param param_name
#' String, name of the parameter to be included (example: param_name = "algorithm")
#'
#' @param file_name
#' String, file name to save plot (example: "~/path/to/file_name.png")
#'
#' @return Frequency and/or density plot
#' @export
#'
#' @examples
#' sampling_frequency_iteration(iraceResults, param_name = "alpha")
sampling_frequency_iteration <- function(irace_results, param_name, file_name = NULL) {
  # Variable assignment
  memo <- vectorPlot <- configuration <- x <- Freq <- iteration_f <- ..density.. <- NULL

  # verify that param_names is other than null
  if (!is.null(param_name)) {
    # verify that param_names contain the data entered
    if (!(param_name %in% irace_results$parameters$names)) {
      cat("Error: Unknown parameter name provided\n")
      stop()
    }
    # verify that param_names contain more than one parameter
    else if (length(param_name) != 1) {
      cat("Error: You can only provide one parameter\n")
      stop()
    }
  }

  # table is created with all settings
  tabla <- irace_results$allConfigurations[,c(".ID.", param_name)]
  filtro <- unique(irace_results$experimentLog[, c("iteration", "configuration")])

  # merge iteration and configuration data
  colnames(filtro)[colnames(filtro) == "configuration"] <- ".ID."
  tabla <- merge(filtro, tabla, by=".ID.")

  # Column .ID. and .PARENT. are removed
  tabla <- tabla[, !(colnames(tabla) %in% c(".ID.")), drop=FALSE]

  # The first column is renamed
  colnames(tabla)[colnames(tabla) %in% c(param_name)] <- "x"
  niter <- length(unique(tabla$iteration))
  tabla$iteration <- factor(tabla$iteration)

  # If the parameter is of type character a frequency graph is displayed
  if (irace_results$parameters$types[param_name] %in% c("c", "o")) {
    tabla <- as.data.frame(table(tabla))
    tabla$iteration_f <- factor(tabla$iteration, levels = rev(unique(tabla$iteration)))

    p <- ggplot(tabla, aes(x = x, y = Freq, fill = x)) +
      geom_bar(stat = "identity") +
      facet_grid(vars(iteration_f), scales = "free") +
      scale_fill_manual(
        values = viridis(length(unique(tabla$x))),
        guide = guide_legend(title = param_name)
      ) +
      labs(y = "Frequency", x = param_name) +
      scale_y_continuous(n.breaks = 3) +
      theme(strip.text.y = element_text(angle = 0))

    # The plot is saved in a list
    vectorPlot[1] <- list(p)
  } else if (irace_results$parameters$types[param_name] %in% c("i", "r", "i,log", "r,log")) {
    tabla <- na.omit(tabla)
    tabla$iteration_f <- factor(tabla$iteration, levels = rev(unique(tabla$iteration)))

    nbreaks <- pretty(range(tabla$x),
      n = nclass.Sturges(tabla$x),
      min.n = 1
    )
    
    # density and histogram plot
    p <- ggplot(as.data.frame(tabla), aes(x = x, fill = iteration)) +
      geom_histogram(aes(y = ..density..),
                     breaks = nbreaks,
                     color = "black", fill = "gray"
      ) +
      geom_density(alpha = 0.7) +
      scale_fill_manual(values = viridis(length(unique(tabla$iteration)))) +
      facet_grid(vars(iteration_f), scales = "free") +
      labs(x = param_name, y = "Frequency") +
      theme(
        axis.title.y = element_text(),
        axis.title.x = element_text(size = 10),
        plot.title = element_text(hjust = 0.5, size = rel(0.8), face = "bold"),
        axis.ticks.x = element_blank()
      ) +
      scale_y_continuous(n.breaks = 3) +
      theme(strip.text.y = element_text(angle = 0))

    # The plot is saved in a list
    vectorPlot[1] <- list(p)
  }

  # If the value in file_name is added the pdf file is created
  if (!is.null(file_name)) {
    ggsave(file_name, plot = do.call("grid.arrange", c(vectorPlot, ncol = 1)))
    # If you do not add the value of file_name, the plot is displayed
  } else {
    p
    return(p)
  }
}
