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
#' load(system.file(package="irace", "exdata", "irace-acotsp.Rdata", mustWork = TRUE))
#' parallel_coord(iraceResults)
#' \dontrun{ 
#' parallel_coord(iraceResults, by_n_param = 5)
#' parallel_coord(iraceResults, only_elite = FALSE)
#' parallel_coord(iraceResults, id_configurations = c(20, 50, 100, 300, 500, 600, 700))
#' parallel_coord(iraceResults, param_names = c("algorithm", "alpha", "rho", "q0", "rasrank"))
#' parallel_coord(iraceResults, iterations = c(1, 4, 6))
#' }
parallel_coord <- function(irace_results, id_configurations = NULL, param_names = NULL,
                           iterations = NULL, only_elite = TRUE, by_n_param = NULL, 
                           color_by_instances = TRUE, filename = NULL)
{
  parameters <- irace_results$parameters
  
  # The function get_dimensions creates a list of settings for each vertical axis
  # in the parallel coordinates plot
  get_dimensions <- function(data) {
    # Create plot dimensions
    for (i in 1:ncol(data)) {
      pname <- colnames(data)[i]
      if (pname == "iteration") {
        dim[[i]] <- list(
          range = c(1, length(irace_results$allElites)),
          values = data[,pname],
          label = pname,
          visible = FALSE
        )
        # if the column is of type character
      } else if (parameters$types[pname] %in% c("c", "o")) {
        if (any(is.na(data[,pname]))) {
          tickT <- c(as.character(parameters$domain[[pname]]), "NA")
          tickV <- 1:(1 + length(parameters$domain[[pname]]))
        } else {
          tickT <- as.character(parameters$domain[[pname]])
          tickV <- 1:length(parameters$domain[[pname]])
        }
        
        data[,pname] <- as.character(data[,pname])
        rdata <- rep(NA, nrow(data)) 
        for (v in 1:length(tickT)){
          rdata[data[,pname] == tickT[v]] <- v
        }
        
        dim[[i]] <- list(
          range = c(1, max(tickV)),
          label = pname,
          tickvals = tickV,
          # ticktext = unique(data[[i]]),
          ticktext = tickT,
          values = rdata
        )
        # if the column is of type numeric
      } else if (parameters$types[pname] %in% c("i", "i,log", "r", "r,log")) {
        if ((as.numeric(parameters$domain[[pname]][2]) + 1) %in% data[,pname]) {
          minimo <- parameters$domain[[pname]][1]
          maximo <- parameters$domain[[pname]][2] + 1
          medio <- round(((maximo - 1) / 4), 1)
          medio2 <- round(((maximo - 1) / 2), 1)
          medio3 <- round(((maximo - 1) * (3 / 4)), 1)
          
          dim[[i]] <- list(
            range = c(parameters$domain[[pname]][1], parameters$domain[[pname]][2] + 1),
            tickvals = c(minimo, medio, medio2, medio3, maximo),
            ticktext = c(minimo, medio, medio2, medio3, "NA"),
            values = data[,pname],
            label = pname
          )
        } else {
          minimo <- parameters$domain[[pname]][1]
          maximo <- parameters$domain[[pname]][2]
          medio <- round((maximo / 4), 1)
          medio2 <- round((maximo / 2), 1)
          medio3 <- round(maximo * (3 / 4), 1)
          # max(data[[i]] cambio maximo
          dim[[i]] <- list(
            range = c(parameters$domain[[pname]][1], parameters$domain[[pname]][2]),
            tickvals = c(minimo, medio, medio2, medio3, maximo),
            ticktext = c(minimo, medio, medio2, medio3, maximo),
            values = data[,pname],
            label = pname
          )
        }
      }
    }
    return(dim)
  }
  
  # Variable assignment
  configuration <- iteration <- dim <- tickV <- vectorP <- NULL
  id_configurations <- unlist(id_configurations)
  
  # set parameter values 
  if (is.null(param_names)) {
    param_names <- parameters$names
  } else {
    param_names <- unlist(param_names)
    if (any(!(param_names %in% parameters$names))) {
      stop("Unknown parameter names were encountered")
    }
  }
  # Verify that param_names contain more than one parameter
  if (length(param_names) < 2) {
    stop("Data must have at least two parameters")
  }
  
  by_n_param <- check_by_n_param(by_n_param, length(param_names))
  
  # Check iterations
  if (!is.null(iterations)) {
    it <- 1:length(irace_results$allElites)
    if (any(!(iterations %in% it))) {
      stop("The iterations entered are outside the possible range")
    }
  } else {
    iterations <- length(irace_results$allElites)
    if (length(irace_results$allElites[[length(irace_results$allElites)]]) == 1) {
      cat("Note: The final iteration only has one elite configuration\n")
    }
  } 
  
  # Check configurations
  if (!is.null(id_configurations)) {
    # Verify that the entered id are within the possible range
    if (any(id_configurations[id_configurations < 1]) || any(id_configurations[id_configurations > nrow(irace_results$allConfigurations)])) {
      stop("IDs provided are outside the range of settings")
    }
    # Verify that the id entered are more than 1 or less than the possible total
    if (length(id_configurations) <= 1 || length(id_configurations) > nrow(irace_results$allConfigurations)) {
      stop("You must provide more than one configuration id")
    }
    iterations <- 1:length(irace_results$allElites)
  } else if (only_elite) {
    id_configurations <- unlist(unique(irace_results$allElites[iterations]))
  } else {
    id_configurations <- unique(irace_results$experimentLog[irace_results$experimentLog[,"iteration"] %in% iterations, "configuration"])
  }
  
  # Select data 
  data <- irace_results$allConfigurations[irace_results$allConfigurations[, ".ID."] %in% id_configurations, ,drop=FALSE]
  config_iter <- unique(irace_results$experimentLog[, c("iteration", "configuration")])
  config_iter <- config_iter[config_iter[, "configuration"] %in% id_configurations, ,drop=FALSE]
  config_iter <- config_iter[config_iter[, "iteration"] %in% iterations, ,drop=FALSE]
  
  experiments <- irace_results$experiments[,as.character(id_configurations),drop=FALSE]
  fitness     <- colSums(!is.na(experiments))
  
  # Merge iteration and configuration data
  colnames(config_iter)[colnames(config_iter) == "configuration"] <- ".ID."
  data <- merge(config_iter, data, by=".ID.")
  
  # Merge fitness measure
  data[,"fitness"] <- fitness[as.character(data[,".ID."])]

  data <- na_data_processing(data, parameters)
    

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
    
    cdata <- data[,c(params, "fitness", "iteration"),drop=FALSE]
    dim <- get_dimensions(cdata)

    # plot creation
    p <- cdata %>% plot_ly()
    if (color_by_instances) {
      p <- p %>% add_trace(
        type = "parcoords",
        line = list(
          color = ~fitness,
          colorscale = "Viridis",
          showscale = TRUE,
          reversescale = TRUE,
          cmin = min(data[,"fitness"]),
          cmax = max(data[,"fitness"])
        ),
        dimensions = dim,
        labelangle = -25
      )
    } else {
      p <- p %>% add_trace(
        type = "parcoords",
        line = list(
          color = ~iteration,
          colorscale = "Viridis",
          showscale = TRUE,
          reversescale = TRUE,
          cmin = 1,
          cmax = length(irace_results$allElites)
        ),
        dimensions = dim,
        labelangle = -25
      )
    }
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
#' @param parameters
#' List, parameter object in irace format
#' (example: `parameters = iraceResults$parameters`)
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
#' load(system.file(package="irace", "exdata", "irace-acotsp.Rdata", mustWork = TRUE))
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
      if (parameters$types[pname] %in% c("c", "o")) {
        if (any(is.na(data[,pname]))) {
          tickT <- c(as.character(parameters$domain[[pname]]), "NA")
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
          ticktext = c(minimo, medio, medio2, medio3, "NA"),
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
  
  # Variable assignment
  configuration <- dim <- tickV <- vectorP <- NULL

  # set parameter values 
  if (is.null(param_names)) {
    param_names <- parameters$names
  } else {
    param_names <- unlist(param_names)
    if (any(!(param_names %in% parameters$names))) {
      stop("Unknown parameter names were encountered")
    }
  }
  # Verify that param_names contain more than one parameter
  if (length(param_names) < 2) {
    stop("Data must have at least two parameters")
  }
  by_n_param <- check_by_n_param(by_n_param, length(param_names))

  configurations <- na_data_processing(configurations, parameters)
    
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
    p <- ctabla %>% plot_ly()
    p <- p %>% add_trace(
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
  return(min(length_param_names, by_n_param))
}


na_data_processing <- function(data, parameters)
{
  # Column .ID. and .PARENT. are removed
  data <- data[, !startsWith(colnames(data), "."), drop=FALSE]
  # NA data processing
  for (k in 1:ncol(data)) {
    # FIXME: This can be done by selecting all columns of each type.
    pname <- colnames(data)[k]
    if (parameters$types[pname] %in% c("i", "i,log", "r", "r,log")) {
      ina <- is.na(data[,pname])
      if (any(ina)) data[ina,pname] <- parameters$domain[[pname]][2] + 1
      
    } else if (parameters$types[pname] %in% c("c", "o")) {
      ina <- is.na(data[,pname])
      if (any(ina)) data[ina,pname] <- "NA"
    }
  }
  return(data)
}
