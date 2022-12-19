#' Sampling pie plot
#'
#' This function creates a pie plot of the values sampled of a set of selected
#' parameters. Numerical parameters are discretized to maximum `n_bins`
#' intervals. The size of the slices are proportional to the number of
#' configurations that have assigned a parameter value within the rank or the
#' value assigned to that slice. Parameters can be selected by providing their
#' names in the `param_names` argument.
#'
#' @template arg_irace_results
#' 
#' @param param_names
#' String vector, A set of parameters to be included (example: param_names = c("algorithm","dlb"))
#' 
#' @param n_bins
#' Numeric (default 3), number of intervals to generate for numerical parameters.
#' 
#' @template arg_filename
#' 
#' @return Sampling pie plot
#' @export
#'
#' @examples
#' iraceResults <- read_logfile(system.file(package="irace", "exdata",
#'                                          "irace-acotsp.Rdata", mustWork = TRUE))
#' sampling_pie(iraceResults)
#' \donttest{
#' sampling_pie(iraceResults, param_names = c("algorithm", "dlb", "ants"))
#' }
sampling_pie <- function(irace_results, param_names = NULL, n_bins=3, filename = NULL)
{
  param_names <- subset_param_names(param_names, irace_results$parameters$names, irace_results$parameters$isFixed)

  # variable assignment
  parents <- labels <- values <- ids <- depend <- NULL
  
  if (!is.numeric(n_bins) || n_bins < 1) stop("'n_bins' must be numeric > 0")
 
  # Logical (default FALSE) that allows to verify if the parameters
  # are dependent on others, modifying the visualization of the plot
  dependency <- FALSE
  
  # the table is generated only with categorical parameters
  data <- irace_results$allConfigurations[,param_names, drop=FALSE]
  
  # discretize numerical parameters
  for (pname in param_names) {
    n_bins_param <- n_bins
    if (irace_results$parameters$types[pname] %in% c("i", "r", "i,log", "r,log")) {
      not.na <- !is.na(data[,pname])
      if(any(!not.na)) {
        n_bins_param <- max(1, n_bins_param - 1)
      }
      # FIXME: this cut might improved by using the median of the data
      val <- data[not.na, pname]
      #same size bins
      ss <- seq(irace_results$parameters$domain[[pname]][1], irace_results$parameters$domain[[pname]][2], length.out=n_bins_param+1)
      if (irace_results$parameters$types[pname] %in% c("i", "i,log")) {
        ss <- round(ss)
      } 
      bins <- cut(val, breaks=ss, include.lowest=TRUE, ordered_result=TRUE)
      # quatile-based bins
      #bins <- cut(val, breaks=c(quantile(val, probs=seq(0,1, by=1/n_bins_param))),
      #            include.lowest = TRUE, ordered_result=TRUE)
      bins <- as.character(bins,scientific = F)
      data[not.na, pname] <- bins
    }
  }
  
  # checks if there is dependency between the parameters
  if (dependency == TRUE) {
    for (i in 1:length(data)) {
      if (!identical(irace_results$parameters$depends[[colnames(data)[i]]], character(0))) {
        depend[colnames(data)[i]] <- list(irace_results$parameters$depends[[colnames(data)[i]]])
      }
    }
  }

  # the table data is generated
  for (j in 1:length(data)) {
    tabla <- table(data[j], useNA = "ifany")

    for (k in 1:length(tabla)) {
      if (k == 1) {
        ids <- c(ids, colnames(data)[j])

        if (!is.null(depend[[colnames(data)[j]]]) && dependency == TRUE) {
          parents <- c(parents, depend[[colnames(data)[j]]])
        } else {
          parents <- c(parents, "")
        }
        labels <- c(labels, colnames(data)[j])
        values <- c(values, sum(tabla))
      }
      ids <- c(ids, paste(colnames(data)[j], names(tabla)[k], sep = " - "))
      parents <- c(parents, colnames(data)[j])
      labels <- c(labels, names(tabla)[k])
      values <- c(values, tabla[[k]])
    }
  }

  # The data table that will be used for the graph is created
  data_f <- data.frame(ids, parents, labels, values, stringsAsFactors = FALSE)
  data_f[is.na(data_f)] <- "NA"

  # if there is a dependency, the values <U+200B><U+200B>of the dependent data are added to its parent
  if (!is.null(depend) && dependency == TRUE) {
    for (i in 1:length(depend)) {
      data_f$values[data_f$ids == depend[[i]]] <- data_f$values[data_f$ids == depend[[i]]] + data_f$values[data_f$ids == names(depend[i])]
    }
  }

  # Create plot
  p <- plotly::plot_ly(
    type = "sunburst",
    ids = data_f$ids,
    labels = data_f$labels,
    parents = data_f$parents,
    values = data_f$values,
    branchvalues = "total"
  )

  # If the value in filename is added the pdf file is created
  if (!is.null(filename)) {
    orca_pdf(filename, p)
  } else {
    # If you do not add the value of filename, the plot is displayed
    p
    return(p)
  }
}
