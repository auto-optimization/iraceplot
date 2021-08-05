#' Parameter Frequency and density Plot
#'
#' @description
#' The function will return a frequency or density plot,
#' for categorical parameters a frequency plot is created,
#' in case of numerical parameters a histogram and density 
#' plot is created.
#'
#' @template arg_irace_results
#'
#' @param param_names
#' String vector, A set of parameters to be included
#' (example: param_names = c("algorithm","alpha","rho","q0","rasrank"))
#'
#' @param n
#' Numeric, for scenarios with large parameter sets, it selected a subset 
#' of 9 parameters. For example, n=1 selects the first 9 (1 to 9) parameters, n=2 selects
#' the next 9 (10 to 18) parameters and so on.
#'
#' @param file_name
#' String,  file name to save plot. If there are more than 9 parameters, 
#' a pdf file extension is recommended as it allows to create a multi-page
#' document. Otherwise, you can use the n argument of the function to generate 
#' the plot of a subset of the parameters. (example: "~/path/to/file_name.pdf")
#'
#' @return Frequency and/or density plot
#' @export
#'
#' @examples
#' sampling_frequency(iraceResults)
#' sampling_frequency(iraceResults, n = 2)
#' sampling_frequency(iraceResults, param_names = c("algorithm", "alpha", "rho", "q0", "rasrank"))
sampling_frequency <- function(irace_results, param_names = NULL, n = NULL, file_name = NULL) {

  # Variable assignment
  vectorG <- tabla <- Var1 <- Freq <- ..density.. <- inicio <- fin <- max_p <- NULL
  max_p <- 9
  
  if (is.null(param_names))
    param_names <- irace_results$parameters$names
  else
    param_names <- unlist(param_names)

  if (any(!(param_names %in% irace_results$parameters$names))) {
    cat("Error: Unknown parameter name provided\n")
    stop()
  }
  
  # Filter data by parameter names
  config <- irace_results$allConfigurations[,param_names,drop=FALSE]
  if (!is.null(n)) {
    if (n < 1 | n > ceiling(length(param_names) / max_p)) {
      cat(paste("Error: n cannot be less than 1 or greater than", ceiling(length(param_names) / max_p), 
                "(", length(param_names),"parameters selected )\n"))
      stop()
    }
    inicio <- (max_p * n - 8)
    fin <- min(max_p * n, length(param_names))
    param_names <- param_names[inicio:fin]
    config <- irace_results$allConfigurations[,param_names]
  }
  
  plot.list <- list()
  npar <- ncol(config)
  
  for (i in 1:ncol(config)) {
    # plot bars
    pnames <- colnames(config)[i]
    if (irace_results$parameters$types[pnames] %in% c("c", "o")) {
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
      vectorG[i] <- list(p)
      plot.list[[i]] <- p
    } else if (irace_results$parameters$types[pnames] %in% c("i", "r", "i,log", "r,log")) {
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
      vectorG[i] <- list(q)
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
  else
    wp <- do.call("grid.arrange", c(plot.list))
  
  # If the value in file_name is added
  if (!is.null(file_name)) {
    if (npar > 9) {
      cat("Warning: multiple plots generated. If a file_name with a pdf extension was provided a multi page plot 
          will be generated. Otherwise, only the last plot set will be saved. Use the n argument of this function 
          to plot by parameter set.")
    }
    # FIXME: we could save in multiple files with a counter in their name,
    ggsave(file_name, wp)
  } else {
    return(wp)
  }
}
