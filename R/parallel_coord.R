#' Parallel Coordinates Plot static
#'
#' @description
#' The parallel_coord function returns a parallel cordinates plot
#' of a set of configurations
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
#' logical (default TRUE), only print elite configurations (argument ignored when id_configuration is provided)
#'
#' @param plot_all_parameters
#' logical (default FALSE), TRUE if all parameter should be included or 
#' FALSE if the first set of 15 parameters provided or in the data file should
#' be displayed.
#'
#' @param file_name
#' String, file name to save plot (example: "~/patch/example/file_name.png")
#'
#' @return parallel coordinates plot
#' @export
#'
#' @examples
#' parallel_coord(iraceResults)
#' parallel_coord(iraceResults, id_configuration = c(20, 50, 100, 300, 500, 600, 700))
#' parallel_coord(iraceResults, param_names = c("algorithm", "alpha", "rho", "q0", "rasrank"))
#' parallel_coord(iraceResults, iterations = c(1, 4, 6))
parallel_coord <- function(irace_results, id_configuration = NULL, param_names = NULL, iterations = NULL, only_elite = TRUE, plot_all_parameters = FALSE, file_name = NULL) {

  # Variable assignment
  memo <- configuration <- dim <- tickV <- vectorP <- NULL
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
  if (plot_all_parameters) {
    if (length(param_names) > 15) {
      cat("Note: There are too many parameters to display in a single coordinated parallel plot. The first 15 parameters will be displayed\n")
      param_names <- param_names[1:15]
    }
  }
  
  
  # Check iterations
  if (!is.null(iterations)) {
    it <- c(1:length(irace_results$allElites))
    if (any(!(iterations %in% it))) {
      cat("Error: The interactions entered are outside the possible range\n")
      stop()
    }
  }
  
  # Check configurations
  if (!is.null(id_configuration)) {
    # Verify that the entered id are within the possible range
    if (any(id_configuration[id_configuration < 1]) || any(id_configuration[id_configuration > nrow(irace_results$allConfigurations)])) {
      cat("Error: IDs provided are outside the range of settings\n")
      stop()
    }
    
    # Verify that the id entered are more than 1 or less than the possible total
    if (length(id_configuration) <= 1 || length(id_configuration) > nrow(irace_results$allConfigurations)) {
      cat("Error: You must provide more than one configuration id\n")
      stop()
    }
  }

  if (is.null(id_configuration)) {
    if (is.null(iterations)) {
      iterations <- c(length(irace_results$allElites))
      if (length(irace_results$allElites[[length(irace_results$allElites)]]) == 1) {
        cat("Note: The final iteration only has one elite configuration\n")
      }
    } 
    if (only_elite)
      id_configuration <- unlist(unique(irace_results$allElites[iterations]))
    else
      id_configuration <- unique(irace_results$experimentLog[irace_results$experimentLog[,"iteration"] %in% iterations, "configuration"])
  }


  # the table to be used and the filter with the iterations and configuration is created
  tabla <- irace_results$allConfigurations[irace_results$allConfigurations[, ".ID."] %in% id_configuration, ]
  filtro <- unique(irace_results$experimentLog[, c("iteration", "configuration")])
  filtro <- filtro[filtro[, "configuration"] %in% id_configuration, ]
  if (!is.null(iterations)) 
    filtro <- filtro[filtro[, "iteration"] %in% iterations, ]
  
  
  # merge iteration and configuration data
  colnames(filtro)[colnames(filtro) == "configuration"] <- ".ID."
  tabla <- merge(filtro, tabla, by=".ID.")

  # Column .ID. and .PARENT. are removed
  tabla <- tabla[, !(colnames(tabla) %in% c(".ID.", ".PARENT."))]
  if (!is.null(param_names)) {
    param_names <- c(param_names, "iteration")
    tabla <- tabla[, (colnames(tabla) %in% param_names)]
  }
  
  # NA data processing
  for (k in 1:ncol(tabla)) {
    pname <- colnames(tabla)[k]
    if (irace_results$parameters$types[pname] %in% c("i", "i,log", "r", "r,log")) {
      ina <- is.na(tabla[,pname])
      if (any(ina)) tabla[ina,pname] <- (irace_results$parameters$domain[[pname]][2] + 1)
      
    } else if (irace_results$parameters$types[pname] %in% c("c", "o")) {
      ina <- is.na(tabla[,pname])
      if (any(ina)) tabla[ina,pname] <- "NA"
    }
  }

  # create plot dimensions
  for (i in 1:ncol(tabla)) {
    pname <- colnames(tabla)[i]
    if (pname == "iteration") {
      dim[[i]] <- list(
        range = c(1, length(irace_results$allElites)),
        values = tabla[,pname],
        label = pname,
        visible = FALSE
      )
      # if the column is of type character
    } else if (irace_results$parameters$types[pname] %in% c("c", "o")) {
      if ("NA" %in% tabla[,pname]) {
        tickT <- c(irace_results$parameters$domain[[pname]], "NA")
        tickV <- c(1:length(irace_results$parameters$domain[[pname]]) + 1)
      } else {
        tickT <- irace_results$parameters$domain[[pname]]
        tickV <- c(1:length(irace_results$parameters$domain[[pname]]))
      }
      for (v in 1:length(tickT)){
        tabla[tabla[,pname] == tickT[v], pname] <- v
      }
      factor_tab <- NULL
      factor_tab <- factor(tabla[,pname])
      
      
      dim[[i]] <- list(
        range = c(1, max(tickV)),
        label = colnames(tabla)[i],
        tickvals = tickV,
        # ticktext = unique(tabla[[i]]),
        ticktext = tickT,
        values = factor_tab
      )
      # if the column is of type numeric
    } else if (irace_results$parameters$types[pname] %in% c("i", "i,log", "r", "r,log")) {
      if ((as.numeric(irace_results$parameters$domain[[pname]][2]) + 1) %in% tabla[,pname]) {
        minimo <- irace_results$parameters$domain[[pname]][1]
        maximo <- irace_results$parameters$domain[[pname]][2] + 1
        medio <- round(((maximo - 1) / 4), 1)
        medio2 <- round(((maximo - 1) / 2), 1)
        medio3 <- round(((maximo - 1) * (3 / 4)), 1)

        dim[[i]] <- list(
          range = c(irace_results$parameters$domain[[pname]][1], irace_results$parameters$domain[[pname]][2] + 1),
          tickvals = c(minimo, medio, medio2, medio3, maximo),
          ticktext = c(minimo, medio, medio2, medio3, "NA"),
          values = tabla[,pname],
          label = pname
        )
      } else {
        minimo <- irace_results$parameters$domain[[pname]][1]
        maximo <- irace_results$parameters$domain[[pname]][2]
        medio <- round((maximo / 4), 1)
        medio2 <- round((maximo / 2), 1)
        medio3 <- round(maximo * (3 / 4), 1)
        # max(tabla[[i]] cambio maximo
        dim[[i]] <- list(
          range = c(irace_results$parameters$domain[[pname]][1], irace_results$parameters$domain[[pname]][2]),
          tickvals = c(minimo, medio, medio2, medio3, maximo),
          ticktext = c(minimo, medio, medio2, medio3, maximo),
          values = tabla[,pname],
          label = pname
        )
      }
    }
  }

  iteration_f <- factor(as.character(tabla$iteration), ordered = TRUE)
  levels(iteration_f) <- c(1:length(unique(tabla$iteration)))
  tabla <- cbind(tabla, iteration_f)
  
  if (length(get_parameters_names(irace_results)) > 15 & !is.null(file_name) & plot_all_parameters == TRUE) {
    inicio <- 1
    final <- 15
    for (i in 1:ceiling(length(get_parameters_names(irace_results)) / 15)) {
      n_parameters <- length(get_parameters_names(irace_results))
      params <- get_parameters_names(irace_results)[inicio:final]
      params <- params[!is.na(params)]
      q <- parallel_coord(irace_results, id_configuration, param_names = params, iterations)
      vectorP[i] <- list(q)
      inicio <- final + 1
      final <- (i + 1) * 15
    }
  }

  # plot creation
  p <- tabla %>%
    plot_ly(width = 600, height = 550)
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


  # If the value in file_name is added the pdf file is created
  if (!is.null(file_name) & plot_all_parameters == FALSE) {
    # The file_name value is worked to separate it and assign it to new values.

    nameFile <- basename(file_name)
    ext <- strsplit(basename(file_name),split = '[.]')[[1]][2]
    directory <- paste0(dirname(file_name), sep = "/")
    withr::with_dir(directory, orca(p, paste0(nameFile,"." ,ext), width = 550, height = 550))

    # If you do not add the value of file_name, the plot is displayed
  } else if (!is.null(file_name) & plot_all_parameters == TRUE) {
    server <- plotly::orca_serve()
    for (i in 1:length(vectorP)) {
      part <- paste0("_plot-", i)
      ext <- strsplit(basename(file_name),split = '[.]')[[1]][2]
      server$export(vectorP[[i]], paste0(file_name, part,"." ,ext))
    }
    server$close()
  } else {
    p
  }
  return(p)
}
