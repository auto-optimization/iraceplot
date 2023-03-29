#' Parallel Coordinates Plot
#'
#' Parallel coordinates plot of a set of selected configurations. Each line in
#' the plot represents a configuration. By default, the final elite
#' configurations are shown. To visualize configurations of other iterations
#' these must be provided setting the argument iterations, configurations of
#' different iterations are shown in different colors. Setting the only_elites
#' argument to FALSE allows to display all configurations in the selected
#' iterations, specific configurations can be selected providing their ids in
#' the id_configuration argument.
#' 
#' The parameters to be included in the plot can be selected with the param_names
#' argument. Additionally, the maximum number of parameters to be displayed in one
#' plot. A list of plots is returned by this function in several plots are required
#' to display the selected data.
#' 
#' To export the plot to a file, it is possible to do it so manually using the
#' functionality provided by plotly in the plot. If a filename is provided,  
#' orca server will be used to export the plots and thus, it requires the library
#' to be installed (<https://github.com/plotly/orca>).
#' 
#'
#' @template arg_irace_results
#' @template arg_id_configurations
#' @template arg_param_names
#'
#' @param iterations
#' Numeric vector, iteration number that should be included in the plot
#' (example: iterations = c(1,4,5))
#'
#' @param only_elite
#' logical (default TRUE), only print elite configurations (argument ignored when 
#' id_configurations is provided)
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
  parameters <- irace_results$parameters
  ptypes <- parameters$types
  pdomain <- parameters$domain
  # The function get_dimensions creates a list of settings for each vertical axis
  # in the parallel coordinates plot
  get_dimensions <- function(data) {
    dimensions <- list()
    # FIXME: This function can be simplified a lot!
    # Create plot dimensions
    for (i in 1:ncol(data)) {
      pname <- colnames(data)[i]
      if (pname == "iteration") {
        dimensions[[i]] <- list(
          label = pname,
          range = c(1L, length(irace_results$allElites)),
          values = data[,pname],
          visible = FALSE
        )
      } else if (pname == ".ID.") {
        # FIXME: We should do this preprocessing before here.
        data[,pname] <- as.character(data[,pname])
        # FIXME: Encoding as integers can be done by using factor() and levels()
        tickT <- data[,pname]
        tickT <- tickT[order(suppressWarnings(as.numeric(tickT)), tickT)]
        tickV <- seq_along(tickT)
        rdata <- rep(NA, nrow(data)) 
        for (v in tickV)
          rdata[data[,pname] == tickT[v]] <- v

        # FIXME: We may have too many IDs. We probably cannot see more than 10-15.
        dimensions[[i]] <- list(
          range = range(tickV),
          label = "ID",
          tickvals = tickV,
          ticktext = tickT,
          values = rdata
        )
        
      } else if (ptypes[pname] %in% c("c", "o")) {
        # FIXME: Encoding as integers can be done by using factor() and levels() and being careful with NAs (replacing them with an "NA" string)
        tickT <- as.character(pdomain[[pname]])
        # FIXME: Can you have NA here? Haven't we remove them already in na_data_processing?
        if (anyNA(data[,pname])) tickT <- c(tickT, "NA")
        tickV <- seq_along(tickT)
        # FIXME: We should do this preprocessing before here.
        data[,pname] <- as.character(data[,pname])
        rdata <- rep(NA, nrow(data)) 
        for (v in tickV) {
          rdata[data[,pname] == tickT[v]] <- v
        }
        # FIXME: We may have too many tickmarks to see. We probably cannot see more than 10-15. 
        dimensions[[i]] <- list(
          range = c(1L, max(tickV)),
          label = pname,
          tickvals = tickV,
          ticktext = tickT,
          values = rdata
        )
        # FIXME: I don't think this will work with log transform
      } else if (ptypes[pname] %in% c("i", "i,log", "r", "r,log")) {
        # This is detecting that na_data_preprocessing has encoded NA values.
        ## FIXME: There must be a better way to do this.
        if ((as.numeric(pdomain[[pname]][2]) + 1) %in% data[,pname]) {
          minimo <- pdomain[[pname]][1]
          maximo <- pdomain[[pname]][2] + 1
          medio <- round(((maximo - 1) / 4), 1)
          medio2 <- round(((maximo - 1) / 2), 1)
          medio3 <- round(((maximo - 1) * (3 / 4)), 1)
          dimensions[[i]] <- list(
            range = c(pdomain[[pname]][1], pdomain[[pname]][2] + 1),
            tickvals = c(minimo, medio, medio2, medio3, maximo),
            ticktext = c(minimo, medio, medio2, medio3, "<NA>"),
            values = data[,pname],
            label = pname
          )
        } else {
          minimo <- pdomain[[pname]][1]
          maximo <- pdomain[[pname]][2]
          medio <- round((maximo / 4), 1)
          medio2 <- round((maximo / 2), 1)
          medio3 <- round(maximo * (3 / 4), 1)
          dimensions[[i]] <- list(
            range = c(pdomain[[pname]][1], pdomain[[pname]][2]),
            tickvals = c(minimo, medio, medio2, medio3, maximo),
            ticktext = c(minimo, medio, medio2, medio3, maximo),
            values = data[,pname],
            label = pname
          )
        }
      }
    }
    return(dimensions)
  }
  
  param_names <- subset_param_names(param_names, parameters$names, parameters$isFixed)
  # Verify that param_names contains more than one parameter
  if (length(param_names) < 2) stop("Data must have at least two parameters")
  by_n_param <- check_by_n_param(by_n_param, length(param_names))
  
  # Check iterations
  if (!is.null(iterations)) {
    it <- 1:length(irace_results$allElites)
    if (any(!(iterations %in% it))) {
      cli_abort("The iterations entered are outside the possible range")
    }
  } else {
    iterations <- length(irace_results$allElites)
    if (length(irace_results$allElites[[length(irace_results$allElites)]]) == 1) {
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
    iterations <- 1:length(irace_results$allElites)
  }

  id_configurations <- unique(as.character(unlist(id_configurations)))
  if (length(id_configurations) <= 1) {
    stop("You must provide more than one configuration ID")
  }
  if (any(!(id_configurations %in% irace_results$allConfigurations[, ".ID."]))) {
    stop("Unknown configuration IDs: ", paste0(setdiff(id_configurations, irace_results$allConfigurations[, ".ID."]), collapse=", "))
  }
    
  # Select data
  data <- irace_results$allConfigurations[irace_results$allConfigurations[, ".ID."] %in% id_configurations, ,drop=FALSE]
  config_iter <- unique(irace_results$experimentLog[, c("iteration", "configuration")])
  config_iter <- config_iter[config_iter[, "configuration"] %in% id_configurations, ,drop=FALSE]
  config_iter <- config_iter[config_iter[, "iteration"] %in% iterations, ,drop=FALSE]
  
  experiments <- irace_results$experiments[,as.character(id_configurations),drop=FALSE]
  # FIXME: It says fitness but this is not really fitness. There should be an option to color according to mean fitness value
  fitness     <- colSums(!is.na(experiments))
  
  # Merge iteration and configuration data
  colnames(config_iter)[colnames(config_iter) == "configuration"] <- ".ID."
  data <- merge(config_iter, data, by=".ID.")
  
  # Merge fitness measure
  data[,"fitness"] <- fitness[as.character(data[,".ID."])]
  # FIXME: This is not correct because we are passing data after expanding it with fitness and iterations. We should do any preprocessing before adding columns
  data <- na_data_processing(data, parameters)

  # Silence CRAN warnings
  iteration <- .ID. <- NULL
  
  # iteration-based plot focused on sampling (first iteration is selected)
  data <- as.data.frame(data %>% group_by(.ID.) %>% slice(which.min(iteration)))
  
  plot_list <- list()
  plot_params <- param_names
  # Create plots
  i <- 1
  while (length(plot_params) > 0) {
    start_i <- 1
    end_i <- min(by_n_param, length(plot_params))
    params <- plot_params[start_i:end_i]
    plot_params <- plot_params[-(start_i:end_i)]
    if (length(plot_params) == 1) {
      params <- c(params, plot_params)
      plot_params <- c()
    }

    # FIXME: If we pass the data to the plot we do not need to pass it to the
    # dimensions. It is enough to pass the column name: https://plotly.com/r/parallel-coordinates-plot/
    cdata <- data[,c(".ID.", params, "fitness", "iteration"), drop=FALSE]
    dimensions <- get_dimensions(cdata)
    color_col <- if (color_by_instances) "Instances" else "Iteration"
    
    # plot creation
    p <- plotly::plot_ly(cdata) %>%
      plotly::add_trace(type = "parcoords",
                line = list(
                  color = if (color_by_instances) ~fitness else ~iteration,
                  colorscale = "Viridis",
                  colorbar = list(title = list(text=color_col)),
                  showscale = TRUE,
                  reversescale = TRUE,
                  cmin = if (color_by_instances) min(data[,"fitness"]) else 1L,
                  cmax = if (color_by_instances) max(data[,"fitness"]) else length(irace_results$allElites)),
                dimensions = dimensions,
                labelangle = -25)
    plot_list[[i]] <- p
    i <- i + 1
  }
  
  # Save plot file
  orca_save_plot(plot_list, filename)
  if (length(plot_list) == 1)
    return(plot_list[[1]])
  return(plot_list)
}


#' Parallel Coordinates Plot (configurations)
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
#' parallel_coord2(iraceResults$allConfigurations[iraceResults$iterationElites,], 
#'                 iraceResults$parameters)
#' parallel_coord2(iraceResults$allConfigurations[iraceResults$iterationElites,], 
#'                 iraceResults$parameters, 
#'                 param_names = c("algorithm", "alpha", "rho", "q0", "rasrank"))
#' parallel_coord2(iraceResults$allConfigurations[iraceResults$iterationElites,], 
#'                 iraceResults$parameters, by_n_param = 5)
#' @export
#' @md
parallel_coord2 <- function(configurations, parameters, param_names = parameters$names,
                            by_n_param = NULL, filename = NULL) {
  
  # The function get_dimensions creates a list of settings for each vertical axis
  # in the parallelcoordinates plot
  get_dimensions <- function(data) {
    # Create plot dimensions
    for (i in 1:ncol(data)) {
      pname <- colnames(data)[i]
      if (pname == ".ID.") next # FIXME: Handle this!
      if (parameters$types[pname] %in% c("c", "o")) {
        if (any(is.na(data[,pname]))) {
          tickT <- c(as.character(parameters$domain[[pname]]), "<NA>")
          tickV <- 1:(length(parameters$domain[[pname]]) + 1)
        } else {
          tickT <- as.character(parameters$domain[[pname]])
          if (length(tickT) == 1) {
            # handle fixed parameters
            tickT <- c("", tickT)
            tickV <- 1:2
          } else {
            tickV <- 1:length(parameters$domain[[pname]])
          }
        }
        
        data[,pname] <- as.character(data[,pname])
        rdata <- rep(NA, nrow(data)) 
        for (v in 1:length(tickT)){
          rdata[data[,pname] == tickT[v]] <- v
        }
        
        dim[[i]] <- list(
          range = c(1, max(tickV)) ,
          label = pname,
          tickvals = tickV,
          ticktext = tickT,
          values = rdata
        )
        # if the column is of type numeric
      } else if ((as.numeric(parameters$domain[[pname]][2]) + 1) %in% data[,pname]) {
        minimo <- parameters$domain[[pname]][1]
        maximo <- parameters$domain[[pname]][2] + 1
        medio <- round(((maximo - 1) / 4), 1)
        medio2 <- round(((maximo - 1) / 2), 1)
        medio3 <- round(((maximo - 1) * (3 / 4)), 1)
          
        dim[[i]] <- list(
          range = c(parameters$domain[[pname]][1], parameters$domain[[pname]][2] + 1),
          tickvals = c(minimo, medio, medio2, medio3, maximo),
          ticktext = c(minimo, medio, medio2, medio3, "<NA>"),
          values = data[,pname],
          label = pname
        )
      } else {
        minimo <- parameters$domain[[pname]][1]
        maximo <- parameters$domain[[pname]][2]
        medio <- round((maximo / 4), 1)
        medio2 <- round((maximo / 2), 1)
        medio3 <- round(maximo * (3 / 4), 1)
        dim[[i]] <- list(
          range = c(parameters$domain[[pname]][1], parameters$domain[[pname]][2]),
          tickvals = c(minimo, medio, medio2, medio3, maximo),
          ticktext = c(minimo, medio, medio2, medio3, maximo),
          values = data[,pname],
          label = pname
        )
      }
    } 
    return(dim)
  }

  param_names <- subset_param_names(param_names, parameters$names, parameters$isFixed)
  # Verify that param_names contains more than one parameter
  if (length(param_names) < 2) stop("Data must have at least two parameters")
  by_n_param <- check_by_n_param(by_n_param, length(param_names))
  configurations <- na_data_processing(configurations, parameters)
    
  # Variable assignment
  configuration <- dim <- tickV <- vectorP <- NULL
  
  plot_list <- list()
  plot_params <- param_names
  # Create plots
  i <- 1
  while(length(plot_params) > 0) {
    start_i <- 1
    end_i <- min(by_n_param, length(plot_params))
    params <- plot_params[start_i:end_i]
    plot_params <- plot_params[-(start_i:end_i)]
    if (length(plot_params) == 1) {
      params <- c(params, plot_params)
      plot_params <- c()
    }
    
    ctabla <- configurations[,params, drop=FALSE]
    dim <- get_dimensions(ctabla)

    # plot creation
    p <- ctabla %>% plotly::plot_ly()
    p <- p %>% plotly::add_trace(
      type = "parcoords",
      line = list(
        color = "#60D0E1"
      ),
      dimensions = dim,
      labelangle = -25
    )
    p <- p %>% plotly::layout(margin = list(r=40))
    plot_list[[i]] <- p
    i <- i + 1
  }
  
  # Save plot file
  orca_save_plot(plot_list, filename)
  if (length(plot_list) == 1)
    return(plot_list[[1]])
  return(plot_list)
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


na_data_processing <- function(data, parameters)
{
  pnames <- colnames(data)[!startsWith(colnames(data), ".")]
  # NA data processing
  for (pname in pnames) {
    # FIXME: This can be done by selecting all columns of each type.
    if (parameters$types[pname] %in% c("i", "i,log", "r", "r,log")) {
      ina <- is.na(data[,pname])
      if (any(ina)) data[ina,pname] <- parameters$domain[[pname]][2] + 1
      
    } else if (parameters$types[pname] %in% c("c", "o")) {
      ina <- is.na(data[,pname])
      if (any(ina)) data[ina,pname] <- "<NA>"
    }
  }
  # Column .PARENT. is removed
  data[, c(".ID.", pnames), drop=FALSE]
}
