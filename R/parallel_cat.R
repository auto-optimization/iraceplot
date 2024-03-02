#' Parallel Coordinates Category
#'
#' Parallel categories plot of selected configurations.  Numerical parameters
#' are discretized to maximum `n_bins` intervals.  To visualize configurations
#' of other iterations these must be provided setting the argument iterations,
#' groups of configurations of different iterations are shown in different
#' colors. Specific configurations can be selected providing their ids in the
#' `id_configurations` argument.
#' 
#' The parameters to be included in the plot can be selected with the
#' param_names argument. Additionally, the maximum number of parameters to be
#' displayed in one plot. A list of plots is returned by this function in
#' several plots are required to display the selected data.
#' 
#'
#' @template arg_irace_results
#'
#' @template arg_id_configurations
#' 
#' @template arg_param_names
#'
#' @param iterations
#' Numeric vector, iterations from which configuration should be obtained
#' (example: iterations = c(1,4,5))
#' 
#' @param by_n_param
#' Numeric (optional), maximum number of parameters to be displayed.
#' 
#' @param n_bins
#' Numeric (default 3), number of intervals to generate for numerical parameters.
#'
#' @template arg_filename
#'
#' @return parallel categories plot
#'
#' @seealso [parallel_coord()] [plot_configurations()]
#' @examples
#' iraceResults <- read_logfile(system.file(package="irace", "exdata",
#'                                          "irace-acotsp.Rdata", mustWork = TRUE))
#' parallel_cat(iraceResults)
#' \donttest{ 
#' parallel_cat(iraceResults, by_n_param = 6)
#' parallel_cat(iraceResults, id_configurations = c(20, 50, 100))
#' parallel_cat(iraceResults, param_names = c("algorithm", "alpha", "rho", "q0", "rasrank"))
#' parallel_cat(iraceResults, iterations = c(1, 4, 6), n_bins=4)
#' }
#' @export
parallel_cat <- function(irace_results, id_configurations = NULL, param_names = NULL, 
                         iterations = NULL,  by_n_param = NULL, n_bins=3, filename = NULL) {

  # Variable assignment
  iteration <- configuration <- dim <- tickV <- vectorP <- x <- y <- id <- freq <- NULL
  id_configurations <- unlist(id_configurations)
  param_names <- subset_param_names(param_names, irace_results$parameters$names, irace_results$parameters$isFixed)
  # Verify that param_names contains more than one parameter
  if (length(param_names) < 2) stop("Data must have at least two parameters")

  # Check by_n_param
  if (is.null(by_n_param)) 
    by_n_param <- length(param_names)
  if (!is.numeric(by_n_param)){
    stop("Error: argument by_n_param must be numeric\n")
  } else if (by_n_param < 2) {
    stop("Error: argument by_n_param must > 1\n")
  }
  by_n_param <- min(length(param_names), by_n_param)
  
  # Check iterations
  if (!is.null(iterations)) {
    if (any(iterations %not_in% seq_along(irace_results$allElites))) {
      stop("Error: The interactions entered are outside the possible range\n")
    }
  } else {
    iterations <- seq_along(irace_results$allElites)
  }
  
  # Check configurations
  if (!is.null(id_configurations)) {
    # Verify that the entered id are within the possible range
    if (any(id_configurations[id_configurations < 1]) || any(id_configurations[id_configurations > nrow(irace_results$allConfigurations)])) {
      stop("Error: IDs provided are outside the range of settings\n")
    }
    # Verify that the id entered are more than 1 or less than the possible total
    if (length(id_configurations) <= 1 || length(id_configurations) > nrow(irace_results$allConfigurations)) {
      stop("Error: You must provide more than one configuration id\n")
    }
    iterations <- 1:length(irace_results$allElites)
  } else {
    id_configurations <- unique(irace_results$experimentLog[irace_results$experimentLog[,"iteration"] %in% iterations, "configuration"])
  }

  if (!is.numeric(n_bins) || n_bins < 1) {
    stop("Error: n_bins must be numeric > 0")
  }
  
  # Select data 
  tabla <- irace_results$allConfigurations[irace_results$allConfigurations[, ".ID."] %in% id_configurations, ]
  filtro <- unique(irace_results$experimentLog[, c("iteration", "configuration")])
  filtro <- filtro[filtro[, "configuration"] %in% id_configurations, ]
  filtro <- filtro[filtro[, "iteration"] %in% iterations, ]
  
  # Merge iteration and configuration data
  colnames(filtro)[colnames(filtro) == "configuration"] <- ".ID."
  tabla <- merge(filtro, tabla, by=".ID.")
  
  # adding discretization for numerical variables and replace NA values 
  # FIXME: Add proper ordering for each axis
  # FIXME: add number of bins as an argument (maybe a list?)
  # FIXME: This is surely wrong! It is not using param_names calculated above!
  for (pname in irace_results$parameters$names) {
    n_bins_param <- n_bins
    if (irace_results$parameters$types[pname] %in% c("i", "r", "i,log", "r,log")) {
      not.na <- !is.na(tabla[,pname])
      u_data <- unique(tabla[not.na, pname])
      if (length(u_data) >= n_bins_param) {
        snot.na <- sum(not.na) 
        if(snot.na < nrow(tabla)) {
          n_bins_param <- max(n_bins - 1, 1)
          if (snot.na < nrow(tabla)/3)
            n_bins_param <- 2
        }
        val  <- tabla[not.na, pname]
        bb <- seq(irace_results$parameters$domain[[pname]][1], 
                  irace_results$parameters$domain[[pname]][2], 
                  length.out=(n_bins_param+1))
        if (irace_results$parameters$types[pname] %in% c("i", "i,log"))
          bb <- round(bb)
        #quartile based ranges
        #val  <- c(irace_results$parameters$domain[[pname]], tabla[not.na, pname])
        #bb   <- unique(c(quantile(val, probs=seq(0,1, by=1/n_bins_param))))
        #bins <- as.character(bins[3:length(bins)],scientific = F)
        bins <- cut(val, breaks=bb, include.lowest = TRUE, ordered_result=TRUE)
        bins <- as.character(bins)
        tabla[not.na, pname] <- bins
      } 
    }

    # replace NA values
    rna <- is.na(tabla[,pname])
    if (any(rna)) {
      tabla[rna,pname] <- "NA"
    }
    tabla[, pname] <- factor(tabla[, pname])
  }

  # Column .ID. and .PARENT. are removed
  tabla <- tabla[, !(startsWith(colnames(tabla), "."))]
  tabla$iteration <- factor(tabla$iteration, ordered=TRUE)
  
  n_parts <- ceiling(length(param_names) / by_n_param)
  start_i <- 1
  end_i <- by_n_param
  plot_list <- list()
  # Create plots
  for (i in 1:n_parts) {
    # stop if we reach the end
    if (end_i > length(param_names))
      break;
    
    # add las parameter as we cant  plot 
    # one parameter in the next plot
    if (length(param_names) == (end_i+1))
      end_i <- end_i + 1
    
    params <- param_names[start_i:end_i]
    ctabla <- tabla[,c(params, "iteration")]
    
    # Format data
    ctabla <- ctabla %>%
      group_by(ctabla[1:ncol(ctabla)]) %>%
      summarise(freq = dplyr::n())# %>% filter(freq > 1)
    ctabla <- ggforce::gather_set_data(ctabla, params)
    ctabla <- ctabla[ctabla$x != "iteration", ]
    
    # Create plot
    p <- ggplot(ctabla, aes(x, id = id, split = y, value = freq)) +
      ggforce::geom_parallel_sets(aes(fill = iteration), alpha = 0.8, axis.width = 0.2) +
      ggforce::geom_parallel_sets_axes(axis.width = 0.3, alpha = 0.4, color = "lightgrey", fill = "lightgrey") +
      ggforce::geom_parallel_sets_labels(colour = "black", angle = 90, size = 3) +
      theme_bw() +
      theme(
        axis.text.x = element_text(angle = 90, size = 9),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()
      )
    plot_list[[i]] <- p
    start_i <- start_i + by_n_param 
    end_i   <- min(end_i + by_n_param, length(param_names))
  }
  

  # If the value in filename is added the pdf file is created
  if (!is.null(filename)) {
    if (length(plot_list) == 1) {
      ggsave(filename, plot = plot_list[[1]])
    } else {
      directory <- paste0(dirname(filename), sep = "/")
      base_name <- strsplit(basename(filename),split = '[.]')[[1]][1]
      ext <- strsplit(basename(filename),split = '[.]')[[1]][2]
      for (i in 1:length(plot_list)) {
        part <- paste0("-", i)
        ggsave(paste0(directory, "/", base_name, part,"." ,ext), plot = plot_list[[i]])
      }
    }
  } 
  
  if (length(plot_list) == 1)
    return(plot_list[[1]])
  return(plot_list)
}
