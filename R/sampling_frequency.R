#' Parameter Frequency and density Plot
#'
#' @description
#' The `sampling_frequency2` function creates a frequency or density plot that 
#' depicts the sampling represented by the set of configurations provided.
#' 
#' For categorical parameters a frequency plot is created, while for numerical 
#' parameters a histogram and density plots are created. The plots are shown in
#' groups of maximum 9, the parameters included in the plot can be specified by
#' setting the param_names argument.
#'
#' @param configurations
#' Data frame, configurations in `irace` format 
#' (example: `configurations = iraceResults$allConfigurations`)
#' 
#' @param parameters
#' List, parameter object in irace format
#' (example: `configurations = iraceResults$parameters`)
#'
#' @param param_names
#' String vector, A set of parameters to be included
#' (example: `param_names = c("algorithm","alpha","rho","q0","rasrank")`)
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
#' sampling_frequency2(iraceResults$allConfigurations, iraceResults$parameters)
#' \dontrun{ 
#' sampling_frequency2(iraceResults$allConfigurations, iraceResults$parameters, n = 2)
#' sampling_frequency2(iraceResults$allConfigurations, iraceResults$parameters, 
#'                     param_names = c("alpha"))
#' sampling_frequency2(iraceResults$allConfigurations, iraceResults$parameters, 
#'                     param_names = c("algorithm", "alpha", "rho", "q0", "rasrank"))
#' }
#' @export
#' @md
sampling_frequency2 <- function(configurations, parameters, param_names = NULL, n = NULL, filename = NULL) {
  
  # Variable assignment
  tabla <- Var1 <- Freq <- ..density.. <- inicio <- fin <- max_p <- NULL
  max_p <- 9
  
  if (is.null(param_names))
    param_names <- parameters$names
  else
    param_names <- unlist(param_names)
  
  if (any(!(param_names %in% parameters$names))) {
    stop("Error: Unknown parameter name provided\n")
  }
  
  if (any(!(param_names %in% colnames(configurations)))) {
    stop("Error: Unknown parameter name provided\n")
  }
  
  # Filter data by parameter names
  config <- configurations[,param_names,drop=FALSE]
  if (!is.null(n)) {
    if (n < 1 | n > ceiling(length(param_names) / max_p)) {
      stop(paste("Error: n cannot be less than 1 or greater than", ceiling(length(param_names) / max_p), 
                 "(", length(param_names),"parameters selected )\n"))
    }
    inicio <- (max_p * n - 8)
    fin <- min(max_p * n, length(param_names))
    param_names <- param_names[inicio:fin]
    config <- configurations[,param_names]
  }
  
  plot.list <- list()
  npar <- ncol(config)
  
  for (i in 1:ncol(config)) {
    # plot bars
    pnames <- colnames(config)[i]
    if (parameters$types[pnames] %in% c("c", "o")) {
      tabla <- as.data.frame(table(config[[i]]))
      p <- ggplot(data = tabla, aes(x = Var1, y = Freq)) +
        geom_bar(stat = "identity", fill = "grey", color = "black") +
        labs(x = "Values") +
        ggtitle(colnames(config)[i]) +
        theme(
          axis.title.y = element_blank(),
          axis.title.x = element_text(size = 8),
          plot.title = element_text(hjust = 0.5, size = rel(0.8), face = "bold"),
          axis.ticks.x = element_blank()
        ) +
        scale_y_continuous(n.breaks = 3)
      # the plot is saved in a list
      plot.list[[i]] <- p
    } else if (parameters$types[pnames] %in% c("i", "r", "i,log", "r,log")) {
      # histogram and density plot
      tabla <- na.omit(config[[i]])
      nbreaks <- pretty(range(tabla),
                        n = nclass.Sturges(tabla),
                        min.n = 1
      )
      q <- ggplot(as.data.frame(tabla), aes(x = tabla)) +
        geom_histogram(aes(y = ..density..),
                       breaks = nbreaks,
                       color = "black", fill = "gray"
        ) +
        geom_density(color = "blue", fill = "blue", alpha = 0.2) +
        labs(x = "Values") +
        ggtitle(colnames(config)[i]) +
        theme(
          axis.title.y = element_blank(),
          axis.title.x = element_text(size = 8),
          plot.title = element_text(hjust = 0.5, size = rel(0.8), face = "bold"),
          axis.ticks.x = element_blank()
        ) +
        scale_y_continuous(n.breaks = 3)
      # the plot is saved in a list
      plot.list[[i]] <- q
    }
  }
  
  # Get appropriate number of columns
  col <- row <- 3
  if (npar <= 3) {
    col <- npar 
    row <- 1 
  } else if (npar <=6){
    col <- 3
    row <- 2
  } 
  # Generate plots
  if (npar > 9)
    wp <- do.call("marrangeGrob", list(grobs=plot.list, ncol = col, nrow = row, as.table=FALSE))
  else if (length(plot.list) ==1)
    wp <- plot.list[[1]]
  else
    wp <- do.call("grid.arrange", c(plot.list))
  
  # If the value in filename is added
  if (!is.null(filename)) {
    if (npar > 9) {
      cat("Warning: multiple plots generated. If a filename with a pdf extension was provided a multi page plot 
          will be generated. Otherwise, only the last plot set will be saved. Use the n argument of this function 
          to plot by parameter set.")
    }
    # FIXME: we could save in multiple files with a counter in their name,
    ggsave(filename, wp)
  } else {
    return(wp)
  }
}

#' Parameter Frequency and density Plot
#'
#' @description
#' The `sampling_frequency` function creates a frequency or density plot that 
#' depicts the sampling performed by irace across the iterations of the configuration 
#' process.
#' 
#' For categorical parameters a frequency plot is created, while for numerical 
#' parameters a histogram and density plots are created. The plots are shown in
#' groups of maximum 9, the parameters included in the plot can be specified by
#' setting the param_names argument.
#'
#' @template arg_irace_results
#'
#' @param param_names
#' String vector, A set of parameters to be included
#' (example: `param_names = c("algorithm","alpha","rho","q0","rasrank")`)
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
#' sampling_frequency(iraceResults)
#' \dontrun{ 
#' sampling_frequency(iraceResults, n = 2)
#' sampling_frequency(iraceResults, param_names = c("alpha"))
#' sampling_frequency(iraceResults, param_names = c("algorithm", "alpha", "rho", "q0", "rasrank"))
#' }
#' @export
#' @md
sampling_frequency <- function(irace_results, param_names = NULL, n = NULL, filename = NULL) {

  sampling_frequency2(configurations=irace_results$allConfigurations, 
                      parameters=irace_results$parameters, 
                      param_names=param_names, 
                      n=n,
                      filename=filename)
}
