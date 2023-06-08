#' Parameter Frequency and Density Plot
#'
#' Frequency or density plot that depicts the sampling performed by irace
#' across the iterations of the configuration process.  For categorical
#' parameters a frequency plot is created, while for numerical parameters a
#' histogram and density plots are created. The plots are shown in groups of
#' maximum 9, the parameters included in the plot can be specified by setting
#' the param_names argument.
#'
#' @param configurations (`data.frame()`)\cr Configurations in `irace` format. Example: `iraceResults$allConfigurations`.
#' 
#' @param parameters (`list()`)\cr Parameters object in `irace` format. If this argument
#'   is missing, the first parameter is taken as the `iraceResults` data
#'   generated when loading the `.Rdata` file created by `irace` and
#'   `configurations=iraceResults$allConfigurations` and `parameters =
#'   iraceResults$parameters`.
#'
#' @template arg_param_names
#'
#' @param n
#' Numeric, for scenarios with large parameter sets, it selects a subset 
#' of 9 parameters. For example, `n=1` selects the first 9 (1 to 9) parameters, n=2 selects
#' the next 9 (10 to 18) parameters and so on.
#'
#' @template arg_filename
#'
#' @note If there are more than 9 parameters, a pdf file extension is
#'   recommended as it allows to create a multi-page document. Otherwise, you
#'   can use the `n` argument of the function to generate the plot of a subset
#'   of the parameters.
#'
#' @return Frequency and/or density plot
#'
#' @examples
#' # Either use iraceResults
#' iraceResults <- read_logfile(system.file(package="iraceplot", "exdata",
#'                                          "guide-example.Rdata", mustWork = TRUE))
#' sampling_frequency(iraceResults)
#' \donttest{ 
#' sampling_frequency(iraceResults, n = 2)
#' sampling_frequency(iraceResults, param_names = c("alpha"))
#' sampling_frequency(iraceResults, param_names = c("algorithm", "alpha", "rho", "q0", "rasrank"))
#' }
#' # Or explicitly specify the configurations and parameters.
#' sampling_frequency(iraceResults$allConfigurations, iraceResults$parameters)
#' \donttest{ 
#' sampling_frequency(iraceResults$allConfigurations, iraceResults$parameters, n = 2)
#' sampling_frequency(iraceResults$allConfigurations, iraceResults$parameters, 
#'                     param_names = c("alpha"))
#' sampling_frequency(iraceResults$allConfigurations, iraceResults$parameters, 
#'                     param_names = c("algorithm", "alpha", "rho", "q0", "rasrank"))
#' }
#' @export
sampling_frequency <- function(configurations, parameters, param_names = NULL, n = NULL, filename = NULL)
{
  if (missing(parameters)) {
    parameters <- configurations$parameters 
    configurations <- configurations$allConfigurations
  }
  param_names <- subset_param_names(param_names, parameters$names, parameters$isFixed)
  if (any(!(param_names %in% colnames(configurations)))) {
    stop("Unknown parameter name provided")
  }

  # This is needed to silence CRAN warnings.
  tabla <- Var1 <- Freq <- density <- NULL
  # Filter data by parameter names
  config <- configurations[,param_names,drop=FALSE]
  if (!is.null(n)) {
    max_p <- 9L # FIXME: Why 9 ????
    if (n < 1 | n > ceiling(length(param_names) / max_p)) {
      stop(paste0("n cannot be less than 1 or greater than ", ceiling(length(param_names) / max_p), 
                 " (", length(param_names)," parameters selected)"))
    }
    start <- (max_p * n - 8)
    end <- min(max_p * n, length(param_names))
    param_names <- param_names[start:end]
    config <- configurations[,param_names]
  }
  
  plot.list <- list()
    
  for (pname in colnames(config)) {
    if (parameters$isFixed[pname]) next
    ptype <- parameters$types[pname]
    if (ptype %in% c("c", "o")) {
      tabla <- as.data.frame(table(config[[pname]]))
      len_longest <- max(nchar(levels(tabla$Var1)))
      angle_x <- 0 # FiXME: Rotation compresses the plot. This may be because
                   # of marrangeGrob. We should find another way.
      p <- ggplot(data = tabla, aes(x = Var1, y = Freq)) +
        geom_bar(stat = "identity", fill = "grey", color = "black") +
        labs(x = "Values") +
        ggtitle(pname) +
        theme(
          axis.title.y = element_blank(),
          axis.title.x = element_text(size = 6),
          plot.title = element_text(hjust = 0.5, size = rel(0.8), face = "bold"),
          axis.ticks.x = element_blank()
        ) +
        # FIXME: This doesn't wrap if there are no spaces: https://github.com/r-lib/scales/issues/353
        # TODO: Use paste0(strsplit(string.to.split, "(?<=[[:lower:]])(?=[[:upper:]])", perl = TRUE), collapse="\n") to split camel case and replace with spaces.
        # TODO: Replace "-" and "_" with spaces if needed.
        # scale_x_discrete(labels = scales::label_wrap(8)) +
        scale_y_continuous(n.breaks = 3) +
        guides(x = guide_axis(angle = angle_x))
    } else if (ptype %in% c("i", "r", "i,log", "r,log")) {
      # histogram and density plot
      tabla <- na.omit(config[[pname]])
      nbreaks <- pretty(range(tabla),
                        n = nclass.Sturges(tabla),
                        min.n = 1)
      # FIXME: There is some repetition between this plot and the above
      # one. Avoid redundancy.
      p <- ggplot(as.data.frame(tabla), aes(x = tabla)) +
        geom_histogram(aes(y = after_stat(density)),
                       breaks = nbreaks,
                       color = "black", fill = "gray") +
        geom_density(color = "blue", fill = "blue", alpha = 0.2) +
        labs(x = "Values") +
        ggtitle(pname) +
        theme(
          axis.title.y = element_blank(),
          axis.title.x = element_text(size = 6),
          plot.title = element_text(hjust = 0.5, size = rel(0.8), face = "bold"),
          axis.ticks.x = element_blank()) +
        scale_y_continuous(n.breaks = 3)
    } else {
      stop("Unknown parameter type (", ptype, ") of parameter '", pname, "'")
    }
    # the plot is saved in a list
    plot.list <- c(plot.list, list(p))
  }

  npar <- length(plot.list)
  # Get appropriate number of columns
  col <- row <- 3
  if (npar <= 3) {
    col <- npar 
    row <- 1L 
  } else if (npar <=6){
    col <- 3L
    row <- 2L
  } 
  # Generate plots
  if (npar > 9)
    wp <- marrangeGrob(grobs=plot.list, ncol = col, nrow = row)
  else if (length(plot.list) == 1L)
    wp <- plot.list[[1L]]
  else
    wp <- do.call("grid.arrange", c(plot.list))
  
  # If the value in filename is added
  if (!is.null(filename)) {
    if (npar > 9) {
      iraceplot_warn("Multiple plots generated: If a filename with a pdf extension",
                     " was provided a multi page plot will be generated;",
                     " otherwise, only the last plot set will be saved.",
                     " Use the {.field n} argument of this function to plot by parameter set.")
    }
    # FIXME: we could save in multiple files with a counter in their name,
    ggsave(filename, wp)
  }
  wp
}
