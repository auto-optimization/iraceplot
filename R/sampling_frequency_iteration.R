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
  if (!(numerical_type %in% c("both", "density", "frequency"))){
    stop("Unknown numerical_type, values must be either both, density ot frequency.")
  }

  parameters <- irace_results$scenario$parameters
  # verify that param_names is other than null
  if (!is.null(param_name)) {
    param_name <- check_unknown_param_names(param_name, parameters$names)
    if (length(param_name) != 1L) stop("You can only provide one parameter")
  }

  # table is created with all settings
  tabla <- irace_results$allConfigurations[,c(".ID.", param_name)]
  filtro <- unique(irace_results$state$experiment_log[, c("iteration", "configuration")])

  # merge iteration and configuration data
  setnames(filtro, "configuration", ".ID.")
  tabla <- merge(filtro, tabla, by=".ID.")
  set(tabla, j = ".ID.", value = NULL)

  # The first column is renamed
  configuration <- Freq <- iteration <- density <- NULL # Silence CRAN warnings

  # If the parameter is of type character a frequency graph is displayed
  if (parameters$types[param_name] %in% c("c", "o")) {
    tabla <- as.data.frame(table(tabla))
    tabla$iteration <- factor(tabla$iteration, levels = rev(unique(tabla$iteration)))

    p <- ggplot(tabla, aes(x = {{param_name}}, y = Freq, fill = {{param_name}})) +
      geom_bar(stat = "identity") +
      scale_fill_manual(
        values = viridisLite::viridis(n_distinct(tabla[[param_name]])),
        guide = guide_legend(title = param_name))

  } else if (parameters$types[param_name] %in% c("i", "r")) {
    tabla <- na.omit(tabla)
    tabla$iteration <- factor(tabla$iteration, levels = rev(unique(tabla$iteration)))
    nbreaks <- pretty(range(tabla[[param_name]]),
      n = nclass.Sturges(tabla[[param_name]]),
      min.n = 1)
    
    # density and histogram plot
    # FIXME: x = {{param_name}} doesn't work, why?
    p <- ggplot(tabla, aes(x = tabla[[param_name]]))
    if (numerical_type %in% c("both", "frequency"))
        p <- p + geom_histogram(aes(y = after_stat(density)),
                       breaks = nbreaks,
                       color = "black", fill = "gray"
         )
    if (numerical_type %in% c("both", "density")) {
        # Note: We can use also density ridges for a different (nicer) looking density plot
        # ggplot(tabla, aes(x, y = iteration, height = stat(density))) +
        # ggridges::geom_density_ridges(aes(fill = iteration), na.rm = TRUE, stat = "density") +
        p <- p + geom_density()
    }
    p <- p + theme(
        axis.title.y = element_text(),
        axis.title.x = element_text(size = 10),
        plot.title = element_text(hjust = 0.5, size = rel(0.8), face = "bold"),
        axis.ticks.x = element_blank()
      )
  }
  p <- p + facet_grid(ggplot2::vars(iteration), scales = "free") +
    labs(x = param_name, y = "Frequency") +
    scale_y_continuous(n.breaks = 3) +
    theme(strip.text.y = element_text(angle = 0),
      legend.position = "none")

  # If the value in filename is added the pdf file is created
  if (!is.null(filename)) {
    ggsave(filename, plot = do.call("grid.arrange", c(list(p), ncol = 1L)))
    # If you do not add the value of filename, the plot is displayed
  } else {
    p
    return(p)
  }
}
