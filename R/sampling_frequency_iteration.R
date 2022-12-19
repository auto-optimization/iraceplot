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
#' @param numerical_type
#' String, (default "both") Indicates the type of plot to be displayed for numerical
#' parameters. "density" shows a density plot, "frequency" shows a frequency plot and 
#' "both" show both frequency and density.
#'
#' @template arg_filename
#'
#' @return Frequency and/or density plot
#'
#' @examples
#' iraceResults <- read_logfile(system.file(package="irace", "exdata",
#'                                          "irace-acotsp.Rdata", mustWork = TRUE))
#' sampling_frequency_iteration(iraceResults, param_name = "alpha")
#' \donttest{ 
#' sampling_frequency_iteration(iraceResults, param_name = "alpha", numerical_type="density")
#' }
#' @export
sampling_frequency_iteration <- function(irace_results, param_name, numerical_type="both", 
                                         filename = NULL)
{
  # Variable assignment
  memo <- vectorPlot <- configuration <- x <- Freq <- iteration_f <- iteration <- density <- NULL
  
  if (!(numerical_type %in% c("both", "density", "frequency"))){
    stop("Unknown numerical_type, values must be either both, density ot frequency.")
  }

  # verify that param_names is other than null
  if (!is.null(param_name)) {
    param_name <- check_unknown_param_names(param_name, irace_results$parameters$names)
    if (length(param_name) != 1L) stop("You can only provide one parameter")
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
  niter <- n_distinct(tabla$iteration)
  tabla$iteration <- factor(tabla$iteration)

  # If the parameter is of type character a frequency graph is displayed
  if (irace_results$parameters$types[param_name] %in% c("c", "o")) {
    tabla <- as.data.frame(table(tabla))
    tabla$iteration_f <- factor(tabla$iteration, levels = rev(unique(tabla$iteration)))

    p <- ggplot(tabla, aes(x = x, y = Freq, fill = x)) +
      geom_bar(stat = "identity") +
      facet_grid(vars(iteration_f), scales = "free") +
      scale_fill_manual(
        values = viridisLite::viridis(length(unique(tabla$x))),
        guide = guide_legend(title = param_name)
      ) +
      labs(y = "Frequency", x = param_name) +
      scale_y_continuous(n.breaks = 3) +
      theme(strip.text.y = element_text(angle = 0),
            legend.position = "none")

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
    p <- ggplot(as.data.frame(tabla), aes(x = x, fill = iteration)) 
    if (numerical_type %in% c("both", "frequency"))
        p <- p + geom_histogram(aes(y = after_stat(density)),
                       breaks = nbreaks,
                       color = "black", fill = "gray"
         ) 
    if (numerical_type %in% c("both", "density")) {
        # Note: We can use also density ridges for a different (nicer) looking density plot
        # ggplot(tabla, aes(x, y = iteration, height = stat(density))) +
        # ggridges::geom_density_ridges(aes(fill = iteration), na.rm = TRUE, stat = "density") +
        p <- p + geom_density(alpha = 0.7) 
    }
    p <- p + scale_fill_manual(values = viridisLite::viridis(n_distinct(tabla$iteration))) +
      facet_grid(vars(iteration_f), scales = "free") +
      labs(x = param_name, y = "Frequency") +
      theme(
        axis.title.y = element_text(),
        axis.title.x = element_text(size = 10),
        plot.title = element_text(hjust = 0.5, size = rel(0.8), face = "bold"),
        axis.ticks.x = element_blank()
      ) +
      scale_y_continuous(n.breaks = 3) +
      theme(strip.text.y = element_text(angle = 0),
            legend.position = "none")

    # The plot is saved in a list
    vectorPlot[1] <- list(p)
  }

  # If the value in filename is added the pdf file is created
  if (!is.null(filename)) {
    ggsave(filename, plot = do.call("grid.arrange", c(vectorPlot, ncol = 1)))
    # If you do not add the value of filename, the plot is displayed
  } else {
    p
    return(p)
  }
}
