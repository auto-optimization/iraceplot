#' Parallel Coordinates Plot
#'
#' Parallel coordinates plot of a set of selected configurations. Each line in
#' the plot represents a configuration. By default, the final elite
#' configurations are shown. To visualize configurations of other iterations
#' these must be provided setting the argument `iterations`, configurations of
#' different iterations are shown in different colors. Setting the `only_elites`
#' argument to `FALSE` displays all configurations in the selected
#' iterations, specific configurations can be selected providing their ids in
#' the `id_configuration` argument.
#' 
#' The parameters to be included in the plot can be selected with the `param_names`
#' argument. Additionally, the maximum number of parameters to be displayed in one
#' plot. A list of plots is returned by this function if several plots are required
#' to display the selected data.
#' 
#' To export the plot to a file, it is possible to do it so manually using the
#' functionality provided by [plotly] in the plot. If a filename is provided,  
#' an orca server will be used to export the plots and thus, it requires the library
#' to be installed (<https://github.com/plotly/orca>).
#' 
#'
#' @template arg_irace_results
#' @template arg_id_configurations
#' @template arg_param_names
#'
#' @param iterations
#' Numeric vector, iteration number that should be included in the plot
#' (example: `iterations = c(1,4,5)`)
#'
#' @param only_elite
#' logical (default `TRUE`), only print elite configurations (argument ignored when 
#' `id_configurations` is provided)
#'
#' @param by_n_param
#' Numeric (optional), maximum number of parameters to be displayed.
#'
#' @param color_by_instances
#' Logical (default TRUE), choose how to color the lines. TRUE shows the number 
#' of instances evaluated by the configuration in the colores. FALSE to show
#' the iteration number where the configuration was sampled.
#' 
#' @template arg_filename
#' @template orca_required
#'
#' @return parallel coordinates plot
#' @export
#'
#' @examples
#' iraceResults <- read_logfile(system.file(package="irace", "exdata",
#'                                          "irace-acotsp.Rdata", mustWork = TRUE))
#' parallel_coord(iraceResults)
#' \donttest{ 
#' parallel_coord(iraceResults, by_n_param = 5)
#' parallel_coord(iraceResults, only_elite = FALSE)
#' parallel_coord(iraceResults, id_configurations = c(20, 30, 40, 50, 100))
#' parallel_coord(iraceResults, param_names = c("algorithm", "alpha", "rho", "q0", "rasrank"))
#' parallel_coord(iraceResults, iterations = c(1, 4, 6))
#' }
parallel_coord <- function(irace_results, id_configurations = NULL, param_names = NULL,
                           iterations = NULL, only_elite = TRUE, by_n_param = NULL, 
                           color_by_instances = TRUE, filename = NULL)
{
  # Silence CRAN warnings
  configuration <- ieration <- .ID. <- .ITERATION. <- NULL
  # Check iterations
  if (!is.null(iterations)) {
    it <- seq_along(irace_results$allElites)
    if (any(iterations %not_in% it)) {
      cli_abort("The iterations entered are outside the possible range")
    }
  } else {
    iterations <- length(irace_results$allElites)
    if (length(irace_results$allElites[[length(irace_results$allElites)]]) == 1L) {
      cli_alert_info("Note: The final iteration only has one elite configuration\n")
    }
  } 

  # Check configurations
  if (is.null(id_configurations)) {
    if (only_elite) {
      id_configurations <- irace_results$allElites[iterations]
    } else {
      id_configurations <- irace_results$experimentLog[irace_results$experimentLog[,"iteration"] %in% iterations, "configuration"]
    }
  } else {
    # FIXME: This overrides the above setting of iterations!
    iterations <- seq_along(irace_results$allElites)
  }

  id_configurations <- unique(as.character(unlist(id_configurations)))
  if (length(id_configurations) <= 1L) {
    stop("You must provide more than one configuration ID")
  }
  if (any(id_configurations %not_in% irace_results$allConfigurations[, ".ID."])) {
    stop("Unknown configuration IDs: ", paste0(setdiff(id_configurations, irace_results$allConfigurations[, ".ID."]), collapse=", "))
  }

  # Select data
  # FIXME: Use dplyr for all these operations.
  data <- irace_results$allConfigurations[irace_results$allConfigurations[, ".ID."] %in% id_configurations, , drop=FALSE]
  config_iter <- unique(irace_results$experimentLog[, c("iteration", "configuration")])
  colnames(config_iter) <- c(".ITERATION.", ".ID.")
  config_iter <- config_iter[(config_iter[, ".ID."] %in% id_configurations)
    & (config_iter[, ".ITERATION."] %in% iterations), ,drop=FALSE]
  
  experiments <- irace_results$experiments[,as.character(id_configurations),drop=FALSE]
  # FIXME: It says fitness but this is not really fitness. There should be an option to color according to mean fitness value
  fitness <- colSums(!is.na(experiments))
  # Merge iteration and configuration data
  data <- merge(config_iter, data, by=".ID.")
  # Merge fitness measure
  data[,".FITNESS."] <- fitness[as.character(data[,".ID."])]

  # iteration-based plot focused on sampling (first iteration is selected)
  data <- as.data.frame(data %>% group_by(.ID.) %>% slice(which.min(.ITERATION.)))

  # The function get_dimensions creates a list of settings for each vertical axis
  # in the parallel coordinates plot
  get_dimensions <- function(pname) {
    values <- cdata[,pname]
    if (pname == ".ITERATION.") {
      return(list(
        label = "Iteration",
        range = range(values),
        values = values,
        visible = FALSE))
    } else if (pname == ".ID.") {
      # FIXME: We should do this preprocessing before here.
      # FIXME: We may have too many IDs. We probably cannot see more than 10-15.
      values <- as.character(values)
      ticktext <- values
      tickvals <- seq_along(ticktext)
      return(list(
        label = "ID",
        range = c(1L, length(ticktext)),
        tickvals = tickvals,
        ticktext = ticktext,
        values = match(values, ticktext)))
    } else if (pname == ".FITNESS.") {
      return(NULL)
    }
    domain <- parameters$domain[[pname]]
    ptype <- parameters$types[pname]
    if (ptype %in% c("c", "o")) {
          values <- as.character(values)
      ticktext <- as.character(domain)
      if (anyNA(values)) {
        ticktext <- c(ticktext, "<NA>")
        # FIXME: Use collapse or data.table or dplyr
        values[is.na(values)] <- "<NA>"
      }
      range <- c(1L, length(ticktext))
      tickvals <- seq_along(ticktext)
      values <- match(values, ticktext)
    } else { # if the column is of type numeric
      # FIXME: This needs to handle log-transformed parameters.
      lower <- domain[[1L]]
      if (!is.numeric(lower))
        lower <- min(values, na.rm = TRUE)
      upper <- domain[[2L]]
      if (!is.numeric(upper))
        upper <- max(values, na.rm = TRUE)
      nticks <- if (ptype == "i") min(5L, 1L + upper - lower) else 5L
      tickvals <- labeling::extended(lower, upper, nticks)
      lower <- min(tickvals)
      upper <- max(tickvals)
      ticktext <- tickvals
      if (anyNA(values)) {
        upper <- upper + (tickvals[2L] - tickvals[1L])
        # FIXME: Use collapse or data.table or dplyr
        values[is.na(values)] <- upper
        ticktext <- c(tickvals, "<NA>")
        tickvals <- c(tickvals, upper)
      }
      range <- c(lower, upper)
    }
    list(label = pname,
      range = range,
      tickvals = tickvals,
      ticktext = ticktext,
      values = values)
  }

  parameters <- irace_results$parameters
  param_names <- subset_param_names(param_names, parameters$names, parameters$isFixed)
  # Verify that param_names contains more than one parameter
  if (length(param_names) < 2L)
    stop("Data must have at least two parameters")
  by_n_param <- check_by_n_param(by_n_param, length(param_names))
  
  # Create plots
  plot_list <- list()
  i <- 1L
  while (length(param_names) > 0L) {
    start_i <- 1L
    end_i <- min(by_n_param, length(param_names))
    params <- param_names[start_i:end_i]
    param_names <- param_names[-(start_i:end_i)]
    if (length(param_names) == 1L) {
      params <- c(params, param_names)
      param_names <- c()
    }

    # FIXME: If we pass the data to the plot we do not need to pass it to the
    # dimensions. It is enough to pass the column name: https://plotly.com/r/parallel-coordinates-plot/
    cdata <- data[,c(".ID.", params, ".FITNESS.", ".ITERATION."), drop=FALSE]
    dimensions <- lapply(colnames(cdata), get_dimensions)
    
    # plot creation
    p <- plotly::plot_ly(cdata) %>%
      plotly::add_trace(type = "parcoords",
        line = list(
          color = if (color_by_instances) ~.FITNESS. else ~.ITERATION.,
          colorscale = "Viridis",
          colorbar = list(title = list(text= if (color_by_instances) "Instances" else "Iteration")),
          showscale = TRUE,
          reversescale = TRUE,
          cmin = if (color_by_instances) min(data[,".FITNESS."]) else 1L,
          cmax = if (color_by_instances) max(data[,".FITNESS."]) else length(irace_results$allElites)),
        dimensions = dimensions,
        labelangle = -25)
    plot_list[[i]] <- p
    i <- i + 1L
  }
  # Save plot file
  orca_save_plot(plot_list, filename)
  if (length(plot_list) == 1L)
    return(plot_list[[1L]])
  plot_list
}


#' Plot parameter configurations using parallel coordinates
#'
#' Parallel coordinates plot of a set of provided configurations. Each line in
#' the plot represents a configuration.  The parameters to be included in the
#' plot can be selected with the param_names argument. Additionally, the
#' maximum number of parameters to be displayed in one plot. A list of plots is
#' returned by this function in several plots are required to display the
#' selected data.
#' 
#' To export the plot to a file, it is possible to do it so manually using the
#' functionality provided by plotly in the plot. If a filename is provided,  
#' orca server will be used to export the plots and thus, it requires the library
#' to be installed (<https://github.com/plotly/orca>).
#' 
#'
#' @param configurations
#' Data frame, configurations in `irace` format 
#' (example: `configurations = iraceResults$allConfigurations`)
#'
#' @template arg_parameters
#'
#' @template arg_param_names
#'
#' @param by_n_param
#' Numeric (optional), maximum number of parameters to be displayed
#' 
#'
#' @template arg_filename
#' @template orca_required
#'
#' @return parallel coordinates plot
#'
#' @examples
#' iraceResults <- read_logfile(system.file(package="irace", "exdata",
#'                                          "irace-acotsp.Rdata", mustWork = TRUE))
#' plot_configurations(iraceResults$allConfigurations[iraceResults$iterationElites,], 
#'                 iraceResults$parameters)
#' plot_configurations(iraceResults$allConfigurations[iraceResults$iterationElites,], 
#'                 iraceResults$parameters, 
#'                 param_names = c("algorithm", "alpha", "rho", "q0", "rasrank"))
#' plot_configurations(iraceResults$allConfigurations[iraceResults$iterationElites,], 
#'                 iraceResults$parameters, by_n_param = 5)
#' @export
#' @md
plot_configurations <- function(configurations, parameters, param_names = parameters$names,
                                by_n_param = NULL, filename = NULL)
{
  # The function get_dimensions creates a list of settings for each vertical axis
  # in the parallelcoordinates plot
  get_dimensions <- function(pname) {
    values <- cdata[,pname]
    if (pname == ".ID.") return(NULL) # FIXME: Handle this!
    domain <- parameters$domain[[pname]]
    ptype <- parameters$types[pname]
    if (ptype %in% c("c", "o")) {
      values <- as.character(values)
      ticktext <- as.character(domain)
      if (anyNA(values)) {
        ticktext <- c(ticktext, "<NA>")
        # FIXME: Use collapse or data.table or dplyr
        values[is.na(values)] <- "<NA>"
      }
      range <- c(1L, length(ticktext))
      tickvals <- seq_along(ticktext)
      values <- match(values, ticktext)
    } else { # if the column is of type numeric
      # FIXME: This needs to handle log-transformed parameters.
      lower <- domain[[1L]]
      if (!is.numeric(lower))
        lower <- min(values, na.rm = TRUE)
      upper <- domain[[2L]]
      if (!is.numeric(upper))
        upper <- max(values, na.rm = TRUE)
      nticks <- if (ptype == "i") min(5L, 1L + upper - lower) else 5L
      tickvals <- labeling::extended(lower, upper, nticks)
      lower <- min(tickvals)
      upper <- max(tickvals)
      ticktext <- tickvals
      if (anyNA(values)) {
        upper <- upper + (tickvals[2L] - tickvals[1L])
        # FIXME: Use collapse or data.table or dplyr
        values[is.na(values)] <- upper
        ticktext <- c(tickvals, "<NA>")
        tickvals <- c(tickvals, upper)
      }
      range <- c(lower, upper)
    }
    list(label = pname,
      range = range,
      tickvals = tickvals,
      ticktext = ticktext,
      values = values)
  }

  param_names <- subset_param_names(param_names, parameters$names, parameters$isFixed)
  # Verify that param_names contains more than one parameter.
  # FIXME: Why? We want to plot even with 1 parameter.
  if (length(param_names) < 2L)
    stop("Data must have at least two parameters")
  # FIXME: Handle .ID.
  configurations <- configurations[, param_names, drop=FALSE]
  by_n_param <- check_by_n_param(by_n_param, length(param_names))
  
  # Create plots
  plot_list <- list()
  i <- 1L
  while (length(param_names) > 0L) {
    start_i <- 1L
    end_i <- min(by_n_param, length(param_names))
    params <- param_names[start_i:end_i]
    param_names <- param_names[-(start_i:end_i)]
    if (length(param_names) == 1L) {
      params <- c(params, param_names)
      param_names <- c()
    }
    
    cdata <- configurations[,params, drop=FALSE]
    dimensions <- lapply(colnames(cdata), get_dimensions)
    # plot creation
    p <- plotly::plot_ly(cdata) %>%
      plotly::add_trace(type = "parcoords",
        line = list(
          color = "#60D0E1"
        ),
        dimensions = dimensions,
        labelangle = -25
      ) %>% plotly::layout(margin = list(r=40))
    plot_list[[i]] <- p
    i <- i + 1L
  }
  # Save plot file
  orca_save_plot(plot_list, filename)
  if (length(plot_list) == 1L)
    return(plot_list[[1L]])
  plot_list
}

check_by_n_param <- function(by_n_param, length_param_names)
{
  if (is.null(by_n_param)) return(length_param_names)
  if (!is.numeric(by_n_param)){
    stop("Argument by_n_param must be numeric")
  } else if (by_n_param < 2) {
    stop("Number of parameters and argument by_n_param must > 1")
  }
  min(length_param_names, by_n_param)
}
