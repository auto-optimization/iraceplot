#' Parallel Cordinate Category
#'
#' @description
#' The parallel_cat function will return a graph of categorical
#' parallel coordinates allowing selecting configurations by id or iteration
#' and selecting the parameter to be included in the plot
#'
#' @template arg_irace_results
#'
#' @param id_configuration
#' Numeric vector, configuration ids to be included in the plot
#' (example: id_configuration = c(20,50,100,300,500,600,700))
#'
#' @param param_names
#' String vector, parameters to be included in the plot
#' (example: param_names = c("algorithm","alpha","rho","q0","rasrank"))
#'
#' @param iterations
#' NUmeric vector, iterations from which configuration should be obtained
#' (example: iterations = c(1,4,5))
#'
#' @param plot_all_parameters
#' logical (default FALSE), TRUE if all parameter should be included or 
#' FALSE if the first set of 15 parameters provided or in the data file should
#' be displayed.
#'
#' @param file_name
#' String, file name to save plot (example: "~/path/to/file_name.png")
#'
#' @return parallel categories plot
#' @export
#'
#' @examples
#' parallel_cat(iraceResults)
#' parallel_cat(iraceResults, id_configuration = c(20, 50, 100, 300, 500, 600, 700))
#' parallel_cat(iraceResults, param_names = c("algorithm", "alpha", "rho", "q0", "rasrank"))
#' parallel_cat(iraceResults, iterations = c(1, 4, 6))
parallel_cat <- function(irace_results, id_configuration = NULL, param_names = NULL, iterations = NULL, plot_all_parameters = FALSE, file_name = NULL) {

  # Variable assignment
  memo <- configuration <- dim <- tickV <- vectorP <- x <- y <- id <- freq <- NULL
  id_configuration <- unlist(id_configuration)
  
  if (is.null(param_names))
    param_names <- irace_results$parameters$names
  else
    param_names <- unlist(param_names)

  if (!is.null(iterations)) {
    it <- c(1:length(irace_results$allElites))
    if (any(!(iterations %in% it))) {
      cat("Error: The interactions entered are outside the possible range\n")
      return()
    }
  }

  # verify that param_names contain the data entered
  if (any(!(param_names %in% irace_results$parameters$names))) {
    cat("Error: Unknown parameter provided\n")
    return()
    # verify that param_names contain more than one parameter
  } else if (length(param_names) < 2) {
    cat("Error: You must provide at least two parameters\n")
    return()
  }

  # adding discretization for numerical variables and replace NA values 
  # FIXME: Add proper ordering for each axis
  # FIXME: add number of bins as an argument (list)
  configurations <- irace_results$allConfigurations
  for (pname in irace_results$parameters$names) {
    n_bins <- 5
    if (irace_results$parameters$types[pname] %in% c("i", "r", "i,log", "r,log")) {
      not.na <- !is.na(configurations[,pname])
      snot.na <- sum(not.na) 
      if(snot.na < nrow(configurations)) {
        n_bins <- 3
        if (snot.na < nrow(configurations)/3)
          n_bins <- 2
      }
        
      val <- c(irace_results$parameters$domain[[pname]], configurations[not.na, pname])
      bins <- cut(val, breaks=c(quantile(val, probs=seq(0,1, by=1/n_bins))),
                  include.lowest = TRUE, ordered_result=TRUE)
      bins <- as.character(bins[3:length(bins)],scientific = F)
      configurations[not.na, pname] <- bins
    }

    # replace NA values
    rna <- is.na(configurations[,pname])
    if (any(rna)) {
      configurations[rna,pname] <- "NA"
    }
  }
  
  tabla <- configurations
  filtro <- unique(irace_results$experimentLog[, c("iteration", "configuration")])
  
  if (!is.null(id_configuration)) {
    # Verify that the entered id are within the possible range
    if (any(id_configuration < 1) || any(id_configuration > nrow(configurations))) {
      cat("Error: IDs provided are outside the range\n")
      return(NULL)
    }

    # Verify that the id entered are more than 1 or less than the possible total
    if (length(id_configuration) <= 1 || length(id_configuration) > nrow(configurations)) {
      cat("Error: You must provide more than one id\n")
      return(NULL)
    }

    # the table to be used and the filter with the iterations and configuration is created
    tabla <- configurations[configurations[, ".ID."] %in% id_configuration, ]
    filtro <- filtro[filtro[, "configuration"] %in% id_configuration, ]
  } 
  
  # merge iteration and configuration data
  colnames(filtro)[colnames(filtro) == "configuration"] <- ".ID."
  tabla <- merge(filtro, tabla, by=".ID.")

  # Column .ID. and .PARENT. are removed
  tabla <- tabla[, !(colnames(tabla) %in% c(".ID.", ".PARENT."))]
  if (is.null(param_names) & length(get_parameters_names(irace_results)) > 15 & plot_all_parameters == FALSE) {
    param_names <- get_parameters_names(irace_results)[1:15]
  }
  # Selected parameters
  if (!is.null(param_names)) {
    param_names <- c(param_names, "iteration")
    tabla <- tabla[, (colnames(tabla) %in% param_names)]
  }

  # Selected iterations
  if (!is.null(iterations)) {
    tabla <- tabla[tabla$iteration %in% iterations, ]
  }
  tabla$iteration <- factor(tabla$iteration, ordered=TRUE)
  
  tabla <- tabla %>%
    group_by(tabla[1:ncol(tabla)]) %>%
    summarise(freq = n()) # %>% filter(freq > 1)
  
  tabla <- gather_set_data(tabla, 1:(ncol(tabla) - 1))
  

  tabla <- tabla[tabla$x != "iteration", ]

  if (!is.null(file_name) & plot_all_parameters == TRUE & length(get_parameters_names(irace_results)) > 15) {
    inicio <- 1
    final <- 15
    for (i in 1:ceiling(length(get_parameters_names(irace_results)) / 15)) {
      n_parameters <- length(get_parameters_names(irace_results))
      params <- get_parameters_names(irace_results)[inicio:final]
      params <- params[!is.na(params)]
      q <- parallel_cat(irace_results, id_configuration, param_names = params, iterations)
      vectorP[i] <- list(q)
      inicio <- final + 1
      final <- (i + 1) * 15
    }
  }

  p <- ggplot(tabla, aes(x, id = id, split = y, value = freq)) +
    geom_parallel_sets(aes(fill = iteration), alpha = 0.8, axis.width = 0.2) +
    geom_parallel_sets_axes(axis.width = 0.5, alpha = 0.4) +
    geom_parallel_sets_labels(colour = "black", angle = 90, size = 3) +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 90, size = 6),
      axis.title.x = element_blank()
    )

  # If the value in file_name is added the pdf file is created
  if (!is.null(file_name)) {
    pdf(paste0(file_name, ".pdf"))
    for (i in 1:length(vectorP)) {
      plot(vectorP[[i]])
    }
    dev.off()
    # If you do not add the value of file_name, the plot is displayed
  } else {
    p
  }
  return(p)
}
