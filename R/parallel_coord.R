#' Parallel Coordinates Plot
#'
#' @description
#' The `parallel_coord` function creates a parallel coordinates plot of a set of 
#' selected configurations. Each line in the plot represents a configuration. By
#' default, the final elite configurations are shown. To visualize configurations 
#' of other iterations these must be provided setting the argument
#' iterations, configurations of different iterations are shown in different 
#' colors. Setting the only_elites argument to FALSE allows to display all 
#' configurations in the selected iterations, specific configurations can
#' be selected providing their ids in the id_configuration argument. 
#' 
#' The parameters to be included in the plot can be selected with the param_names
#' argument. Additionally, the maximum number of parameters to be displayed in one
#' plot. A list of plots is returned by this function in several plots are required
#' to display the selected data.
#' 
#' To export the plot to a file, it is possible to do it so manually using the
#' functionality provided by plotly in the plot. If a file_name is provided,  
#' orca server will be used to export the plots and thus, it requires the library
#' to be installed (https://github.com/plotly/orca).
#' 
#'
#' @template arg_irace_results
#'
#' @param id_configuration
#' Numeric vector, configurations ids whose performance should be included in the plot
#' (example: id_configuration = c(20,50,100,300,500,600,700))
#'
#' @param param_names
#' String vector, names of the parameters that should be included in the plot
#' (example: param_names = c("algorithm","alpha","rho","q0","rasrank"))
#'
#' @param iterations
#' Numeric vector, iteration number that should be included in the plot
#' (example: iterations = c(1,4,5))
#'
#' @param only_elite
#' logical (default TRUE), only print elite configurations (argument ignored when 
#' id_configuration is provided)
#'
#' @param by_n_param
#' Numeric (optional), maximum number of parameters to be displayed.
#'
#' @param file_name
#' String, file name to save plot (example: "~/patch/example/file_name.png"). 
#' Orca is required. See more details in: https://github.com/plotly/orca.
#'
#' @return parallel coordinates plot
#' @export
#'
#' @examples
#' parallel_coord(iraceResults)
#' parallel_coord(iraceResults, by_n_param = 5)
#' parallel_coord(iraceResults, only_elite = FALSE)
#' parallel_coord(iraceResults, id_configuration = c(20, 50, 100, 300, 500, 600, 700))
#' parallel_coord(iraceResults, param_names = c("algorithm", "alpha", "rho", "q0", "rasrank"))
#' parallel_coord(iraceResults, iterations = c(1, 4, 6))
parallel_coord <- function(irace_results, id_configuration = NULL, param_names = NULL,
                           iterations = NULL, only_elite = TRUE, by_n_param = NULL, 
                           file_name = NULL) {
  
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
      } else if (irace_results$parameters$types[pname] %in% c("c", "o")) {
        if (any(is.na(data[,pname]))) {
          tickT <- c(as.character(irace_results$parameters$domain[[pname]]), "NA")
          tickV <- 1:(1 + length(irace_results$parameters$domain[[pname]]))
        } else {
          tickT <- as.character(irace_results$parameters$domain[[pname]])
          tickV <- 1:length(irace_results$parameters$domain[[pname]])
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
      } else if (irace_results$parameters$types[pname] %in% c("i", "i,log", "r", "r,log")) {
        if ((as.numeric(irace_results$parameters$domain[[pname]][2]) + 1) %in% data[,pname]) {
          minimo <- irace_results$parameters$domain[[pname]][1]
          maximo <- irace_results$parameters$domain[[pname]][2] + 1
          medio <- round(((maximo - 1) / 4), 1)
          medio2 <- round(((maximo - 1) / 2), 1)
          medio3 <- round(((maximo - 1) * (3 / 4)), 1)
          
          dim[[i]] <- list(
            range = c(irace_results$parameters$domain[[pname]][1], irace_results$parameters$domain[[pname]][2] + 1),
            tickvals = c(minimo, medio, medio2, medio3, maximo),
            ticktext = c(minimo, medio, medio2, medio3, "NA"),
            values = data[,pname],
            label = pname
          )
        } else {
          minimo <- irace_results$parameters$domain[[pname]][1]
          maximo <- irace_results$parameters$domain[[pname]][2]
          medio <- round((maximo / 4), 1)
          medio2 <- round((maximo / 2), 1)
          medio3 <- round(maximo * (3 / 4), 1)
          # max(data[[i]] cambio maximo
          dim[[i]] <- list(
            range = c(irace_results$parameters$domain[[pname]][1], irace_results$parameters$domain[[pname]][2]),
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
  id_configuration <- unlist(id_configuration)
  
  # set parameter values 
  if (is.null(param_names)) {
    param_names <- irace_results$parameters$names
  } else {
    param_names <- unlist(param_names)
  }
  
  # Check parameter values
  if (any(!(param_names %in% irace_results$parameters$names))) {
    cat("Error: Unknown parameter names were encountered\n")
    stop()
    # verify that param_names contain more than one parameter
  } else if (length(param_names) < 2) {
    cat("Error: Data must have at least two parameters\n")
    stop()
  }
  
  # Check by_n_param
  if (is.null(by_n_param))
    by_n_param <- length(param_names)
  if (!is.numeric(by_n_param)){
    cat("Error: argument by_n_param must be numeric\n")
    stop()
  } else if (by_n_param < 2) {
    stop("Number of parameters and argument by_n_param must > 1")
  }
  by_n_param <- min(length(param_names), by_n_param)
  
  # Check iterations
  if (!is.null(iterations)) {
    it <- 1:length(irace_results$allElites)
    if (any(!(iterations %in% it))) {
      stop("The interactions entered are outside the possible range")
    }
  } else {
    iterations <- length(irace_results$allElites)
    if (length(irace_results$allElites[[length(irace_results$allElites)]]) == 1) {
      cat("Note: The final iteration only has one elite configuration\n")
    }
  } 
  
  # Check configurations
  if (!is.null(id_configuration)) {
    # Verify that the entered id are within the possible range
    if (any(id_configuration[id_configuration < 1]) || any(id_configuration[id_configuration > nrow(irace_results$allConfigurations)])) {
      stop("Error: IDs provided are outside the range of settings")
    }
    # Verify that the id entered are more than 1 or less than the possible total
    if (length(id_configuration) <= 1 || length(id_configuration) > nrow(irace_results$allConfigurations)) {
      stop("Error: You must provide more than one configuration id")
    }
    iterations <- 1:length(irace_results$allElites)
  } else {
    if (only_elite)
      id_configuration <- unlist(unique(irace_results$allElites[iterations]))
    else
      id_configuration <- unique(irace_results$experimentLog[irace_results$experimentLog[,"iteration"] %in% iterations, "configuration"])
  } 
  
  # Select data 
  tabla <- irace_results$allConfigurations[irace_results$allConfigurations[, ".ID."] %in% id_configuration, ,drop=FALSE]
  filtro <- unique(irace_results$experimentLog[, c("iteration", "configuration")])
  filtro <- filtro[filtro[, "configuration"] %in% id_configuration, ,drop=FALSE]
  filtro <- filtro[filtro[, "iteration"] %in% iterations, ,drop=FALSE]
  
  # Merge iteration and configuration data
  colnames(filtro)[colnames(filtro) == "configuration"] <- ".ID."
  tabla <- merge(filtro, tabla, by=".ID.")
  
  # Column .ID. and .PARENT. are removed
  tabla <- tabla[, !(colnames(tabla) %in% c(".ID.", ".PARENT.")),drop=FALSE]
  
  # NA data processing
  for (k in 1:ncol(tabla)) {
    pname <- colnames(tabla)[k]
    if (irace_results$parameters$types[pname] %in% c("i", "i,log", "r", "r,log")) {
      ina <- is.na(tabla[,pname])
      if (any(ina)) tabla[ina,pname] <- irace_results$parameters$domain[[pname]][2] + 1
      
    } else if (irace_results$parameters$types[pname] %in% c("c", "o")) {
      ina <- is.na(tabla[,pname])
      if (any(ina)) tabla[ina,pname] <- "NA"
    }
  }
  

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
    
    ctabla <- tabla[,c(params, "iteration"),drop=FALSE]
    dim <- get_dimensions(ctabla)

    # plot creation
    p <- ctabla %>% plot_ly()
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
    plot_list[[i]] <- p
    i <- i + 1
  }
  
  
  # Save plot file
  if (!is.null(file_name)) {
    directory <- paste0(dirname(file_name), sep = "/")
    if (length(plot_list)==1) {
      orca(plot_list[[1]], path_rel2abs(file_name))
    } else {
      base_name <- strsplit(basename(file_name),split = '[.]')[[1]][1]
      ext <- strsplit(basename(file_name),split = '[.]')[[1]][2]
      for (i in 1:length(plot_list)) {
        part <- paste0("-", i)
        cfile <- path_rel2abs(paste0(directory, "/", base_name, part,"." ,ext))
        orca(plot_list[[i]], cfile)
      }
    }
  }

  if (length(plot_list) == 1)
    return(plot_list[[1]])
  return(plot_list)
}

#' Parallel Coordinates Plot (configurations)
#'
#' @description
#' The `parallel_coord2` function creates a parallel coordinates plot of a set of 
#' provided configurations. Each line in the plot represents a configuration. 
#' 
#' The parameters to be included in the plot can be selected with the param_names
#' argument. Additionally, the maximum number of parameters to be displayed in one
#' plot. A list of plots is returned by this function in several plots are required
#' to display the selected data.
#' 
#' To export the plot to a file, it is possible to do it so manually using the
#' functionality provided by plotly in the plot. If a file_name is provided,  
#' orca server will be used to export the plots and thus, it requires the library
#' to be installed (https://github.com/plotly/orca).
#' 
#'
#' @param configurations
#' Data frame, configurations in irace format 
#' (example: configurations = iraceResults$allConfigurations)
#'
#' @param parameters
#' List, parameter object in irace format
#' (example: configurations = iraceResults$parameters)
#'
#' @param param_names
#' String vector, names of the parameters that should be included in the plot
#' (example: param_names = c("algorithm","alpha","rho","q0","rasrank"))
#'
#' @param by_n_param
#' Numeric (optional), maximum number of parameters to be displayed
#'
#' @param file_name
#' String, file name to save plot (example: "~/patch/example/file_name.png"). 
#' Orca is required. See more details in: https://github.com/plotly/orca
#'
#' @return parallel coordinates plot
#' @export
#'
#' @examples
#' parallel_coord2(iraceResults$allConfigurations[iraceResults$iterationElites,], 
#'                 iraceResults$parameters)
#' parallel_coord2(iraceResults$allConfigurations[iraceResults$iterationElites,], 
#'                 iraceResults$parameters, 
#'                 param_names = c("algorithm", "alpha", "rho", "q0", "rasrank"))
#' parallel_coord2(iraceResults$allConfigurations[iraceResults$iterationElites,], 
#'                 iraceResults$parameters, by_n_param = 5)
parallel_coord2 <- function(configurations, parameters, param_names = parameters$names,
                            by_n_param = NULL, file_name = NULL) {
  
  # The function get_dimensions creates a list of settings for each vertical axis
  # in the parallel coordinates plot
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
          ticktext = tickT,
          values = rdata
        )
        # if the column is of type numeric
      } else {
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
  configuration <- dim <- tickV <- vectorP <- NULL
  
  # set parameter values 
  if (is.null(param_names)) {
    param_names <- parameters$names
  } else {
    param_names <- unlist(param_names)
  }
  
  # Check parameter values
  if (any(!(param_names %in% parameters$names))) {
    cat("Error: Unknown parameter names were encountered\n")
    stop()
    # verify that param_names contain more than one parameter
  } else if (length(param_names) < 2) {
    cat("Error: Data must have at least two parameters\n")
    stop()
  }
  
  # Check by_n_param
  if (is.null(by_n_param))
    by_n_param <- length(param_names)
  if (!is.numeric(by_n_param)){
    cat("Error: argument by_n_param must be numeric\n")
    stop()
  } else if (by_n_param < 2) {
    cat("Error: number of parameters and argument by_n_param must > 1\n")
    stop()
  }
  by_n_param <- min(length(param_names), by_n_param)

  # Column .ID. and .PARENT. are removed
  configurations <- configurations[, !(colnames(configurations) %in% c(".ID.", ".PARENT.")), drop=FALSE]
  
  # NA data processing
  for (k in 1:ncol(configurations)) {
    pname <- colnames(configurations)[k]
    if (parameters$types[pname] %in% c("i", "i,log", "r", "r,log")) {
      ina <- is.na(configurations[,pname])
      if (any(ina)) configurations[ina,pname] <- (parameters$domain[[pname]][2] + 1)
      
    } else if (parameters$types[pname] %in% c("c", "o")) {
      ina <- is.na(configurations[,pname])
      if (any(ina)) configurations[ina,pname] <- "NA"
    }
  }

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
  if (!is.null(file_name)) {
    directory <- paste0(dirname(file_name), sep = "/")
    if (length(plot_list)==1) {
      orca(plot_list[[1]], file_name)
    } else {
      base_name = strsplit(basename(file_name),split = '[.]')[[1]][1]
      ext <- strsplit(basename(file_name),split = '[.]')[[1]][2]
      for (i in 1:length(plot_list)) {
        part <- paste0("-", i)
        cfile <- paste0(directory, "/", base_name, part,"." ,ext)
        orca(plot_list[[i]], cfile)
      }
    }
  }
  
  if (length(plot_list) == 1)
    return(plot_list[[1]])
  return(plot_list)
}

