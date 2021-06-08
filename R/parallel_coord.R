#' Parallel Coordinates Plot static
#'
#' @description
#' The iparallelcoord function will return a parallel cordinates plot
#' allowing the analysis of the set of parameters
#'
#' @template arg_irace_results
#'
#' @param id_configuration
#' Numeric vector, you need to put the configurations you want to analyze
#' (example: id_configuration = c(20,50,100,300,500,600,700))
#'
#' @param param_names
#' String vector, you need to put the parameters you want to analyze
#' (example: param_names = c("algorithm","alpha","rho","q0","rasrank"))
#'
#' @param iterations
#' NUmeric vector, you need to put the iterations you want to analyze
#' (example: iterations = c(1,4,5))
#'
#' @param only_elite
#' logical (default FALSE),
#'
#' @param pdf_all_parameters
#' logical (default FALSE), If I want to create a pdf with all the parameters,
#' I must put TRUE, otherwise it will be created only with the default
#' parameters (15 or less) or those entered.
#'
#' @param file_name
#' A pdf will be created in the location and with
#' the assigned name (example: "~/patch/example/file_name")
#'
#' @return parallel cordinates plot
#' @export
#'
#' @examples
#' parallel_coord(iraceResults)
#' parallel_coord(iraceResults, id_configuration = c(20, 50, 100, 300, 500, 600, 700))
#' parallel_coord(iraceResults, param_names = c("algorithm", "alpha", "rho", "q0", "rasrank"))
#' parallel_coord(iraceResults, iterations = c(1, 4, 6))
parallel_coord <- function(irace_results, id_configuration = NULL, param_names = NULL, iterations = NULL, only_elite = TRUE, pdf_all_parameters = FALSE, file_name = NULL) {

  # Variable assignment
  memo <- configuration <- dim <- tickV <- vectorP <- NULL
  id_configuration <- unlist(id_configuration)
  param_names <- unlist(param_names)

  if (is.null(param_names) & is.null(file_name)) {
    if (length(get_parameters_names(irace_results)) == 16) {
      print("There are too many parameters to display in a single coordinated parallel plot. It will select relevant parameters")
      print("The first 16 parameters will be displayed")
      param_names <- get_parameters_names(irace_results)[1:16]
    } else if (length(get_parameters_names(irace_results)) > 15) {
      print("There are too many parameters to display in a single coordinated parallel plot. It will select relevant parameters")
      print("The first 15 parameters will be displayed")
      param_names <- get_parameters_names(irace_results)[1:15]
    }
  }

  if (is.null(iterations) & is.null(id_configuration)) {
    iterations <- c(length(irace_results$allElites))

    if (length(irace_results$allElites[[length(irace_results$allElites)]]) == 1) {
      print("The final iteration only has an elite configuration")
    }
  }

  if (!is.null(iterations)) {
    it <- c(1:length(irace_results$allElites))
    if (FALSE %in% (iterations %in% it)) {
      return("The interactions entered are outside the possible range")
    }
  }

  # verify that param_names is other than null
  if (!is.null(param_names)) {
    # verify that param_names contain the data entered
    if ("FALSE" %in% names(table(param_names %in% irace_results$parameters$names))) {
      return("Some wrong parameter entered")
      # verify that param_names contain more than one parameter
    } else if (length(param_names) < 2) {
      return("You must enter at least two parameters")
    }
  }

  if (!is.null(id_configuration)) {

    # Verify that the entered id are within the possible range
    if (length(id_configuration[id_configuration < 1]) >= 1 || length(id_configuration[id_configuration > dim(irace_results$allConfigurations)[1]]) >= 1) {
      return("IDs entered are outside the range of settings")
    }

    # Verify that the id entered are more than 1 or less than the possible total
    if (length(id_configuration) <= 1 || length(id_configuration) > dim(irace_results$allConfigurations)[1]) {
      return("You must enter more than one id")
    }

    # the table to be used and the filter with the iterations and configuration is created
    selection <- irace_results$allConfigurations[, ".ID."] %in% id_configuration
    tabla <- irace_results$allConfigurations[selection, ]
    filtro <- unique(irace_results$experimentLog[, c("iteration", "configuration")])
    selection2 <- filtro[, "configuration"] %in% id_configuration
    filtro <- filtro[selection2, ]
    only_elite <- FALSE
    # table is created with all settings
  } else {
    tabla <- irace_results$allConfigurations
    filtro <- unique(irace_results$experimentLog[, c("iteration", "configuration")])
  }

  # The filter table is created and ordered according to the configurations
  filtro <- as.data.frame(filtro)
  filtro <- arrange(filtro, configuration)

  # An iteration table is created and added to the table
  iteration <- sample(NA, size = dim(tabla)[1], replace = TRUE)
  tabla <- cbind(tabla, iteration)

  # The NA of the first row of the table is replaced in the iteration column
  if (tabla$.ID.[1] == filtro$configuration[1]) {
    tabla$iteration[1] <- filtro$iteration[1]
  }

  # memo is assigned the value of the filter table configuration
  memo <- filtro$configuration[1]

  # The NAs of the table are replaced in the iteration column
  for (i in 2:dim(filtro)[1]) {

    # if the same configuration has more than one iteration, a new row is created
    if (memo == filtro$configuration[i]) {
      add <- tabla[tabla$.ID. == memo, ]
      add$iteration <- filtro$iteration[i]
      tabla <- rbind(tabla, add)
      # The iteration is assigned to the configuration
    } else {
      tabla$iteration[tabla$.ID. == filtro$configuration[i]] <- filtro$iteration[i]
    }
    memo <- filtro$configuration[i]
  }

  if (only_elite == TRUE) {
    for (i in 1:length(irace_results$allElites)) {
      if (i == 1) {
        tabla_new <- tabla[tabla$.ID. %in% irace_results$allElites[[i]] & tabla$iteration %in% i, ]
      } else {
        tabla_new <- rbind(tabla_new, tabla[tabla$.ID. %in% irace_results$allElites[[i]] & tabla$iteration %in% i, ])
      }
    }
    tabla <- tabla_new
  }

  # Column .ID. and .PARENT. are removed
  tabla <- tabla[, !(names(tabla) %in% c(".ID.", ".PARENT."))]
  if (!is.null(param_names)) {
    param_names <- c(param_names, "iteration")
    tabla <- tabla[, (names(tabla) %in% param_names)]
  }

  # NA data processing
  for (k in 1:(length(tabla))) {
    if (class(tabla[[k]]) == "numeric" && NA %in% tabla[[k]]) {
      tabla[[k]][is.na(tabla[[k]])] <- (irace_results$parameters$domain[[colnames(tabla)[k]]][2] + 1)
    } else if (class(tabla[[k]]) == "character" && NA %in% tabla[[k]]) {
      tabla[[k]][is.na(tabla[[k]])] <- "NA"
    }
  }

  if (!is.null(iterations)) {
    tabla <- tabla[tabla$iteration %in% iterations, ]
  }

  # create plot dimensions
  for (i in 1:length(tabla)) {
    if (colnames(tabla)[i] == "iteration") {
      dim[[i]] <- list(
        range = c(1, length(irace_results$allElites)),
        values = tabla[[i]],
        label = colnames(tabla)[i],
        visible = FALSE
      )
      # if the column is of type character
    } else if (class(tabla[[i]]) == "character") {
      factor_tab <- NULL
      factor_tab <- factor(tabla[[i]])
      levels(factor_tab) <- c(1:length(unique(tabla[[i]])))
      if ("NA" %in% tabla[[i]]) {
        tickT <- c(irace_results$parameters$domain[[colnames(tabla)[i]]], "NA")
        tickV <- c(1:length(irace_results$parameters$domain[[colnames(tabla)[i]]]) + 1)
      } else {
        tickT <- irace_results$parameters$domain[[colnames(tabla)[i]]]
        tickV <- c(1:length(irace_results$parameters$domain[[colnames(tabla)[i]]]))
      }
      dim[[i]] <- list(
        range = c(1, max(tickV)),
        label = colnames(tabla)[i],
        tickvals = tickV,
        # ticktext = unique(tabla[[i]]),
        ticktext = tickT,
        values = factor_tab
      )
      # if the column is of type numeric
    } else if (class(tabla[[i]]) == "numeric") {
      if ((as.numeric(irace_results$parameters$domain[[colnames(tabla)[i]]][2]) + 1) %in% tabla[[i]]) {
        minimo <- irace_results$parameters$domain[[colnames(tabla)[i]]][1]
        maximo <- irace_results$parameters$domain[[colnames(tabla)[i]]][2] + 1
        medio <- round(((maximo - 1) / 4), 1)
        medio2 <- round(((maximo - 1) / 2), 1)
        medio3 <- round(((maximo - 1) * (3 / 4)), 1)

        dim[[i]] <- list(
          range = c(irace_results$parameters$domain[[colnames(tabla)[i]]][1], irace_results$parameters$domain[[colnames(tabla)[i]]][2] + 1),
          tickvals = c(minimo, medio, medio2, medio3, maximo),
          ticktext = c(minimo, medio, medio2, medio3, "NA"),
          values = tabla[[i]],
          label = colnames(tabla)[i]
        )
      } else {
        minimo <- irace_results$parameters$domain[[colnames(tabla)[i]]][1]
        maximo <- irace_results$parameters$domain[[colnames(tabla)[i]]][2]
        medio <- round((maximo / 4), 1)
        medio2 <- round((maximo / 2), 1)
        medio3 <- round(maximo * (3 / 4), 1)
        # max(tabla[[i]] cambio maximo
        dim[[i]] <- list(
          range = c(irace_results$parameters$domain[[colnames(tabla)[i]]][1], irace_results$parameters$domain[[colnames(tabla)[i]]][2]),
          tickvals = c(minimo, medio, medio2, medio3, maximo),
          ticktext = c(minimo, medio, medio2, medio3, maximo),
          values = tabla[[i]],
          label = colnames(tabla)[i]
        )
      }
    }
    # other types
    else {
      dim[[i]] <- list(
        range = c(min(tabla[[i]], na.rm = TRUE), max(tabla[[i]], na.rm = TRUE)),
        values = tabla[[i]],
        label = colnames(tabla)[i]
      )
    }
  }

  iteration_f <- factor(as.character(tabla$iteration), ordered = TRUE)
  levels(iteration_f) <- c(1:length(unique(tabla$iteration)))
  tabla <- cbind(tabla, iteration_f)

  if (length(get_parameters_names(irace_results)) > 15 & !is.null(file_name) & pdf_all_parameters == TRUE) {
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
    plot_ly(width = 1000, height = 600)
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
  if (!is.null(file_name) & pdf_all_parameters == FALSE) {
    # The file_name value is worked to separate it and assign it to new values.

    nameFile <- basename(file_name)
    directory <- paste0(dirname(file_name), sep = "/")
    withr::with_dir(directory, orca(p, paste0(nameFile, ".pdf")))

    # If you do not add the value of file_name, the plot is displayed
  } else if (!is.null(file_name) & pdf_all_parameters == TRUE) {
    server <- plotly::orca_serve()
    for (i in 1:length(vectorP)) {
      part <- paste0("_plot-", i)
      server$export(vectorP[[i]], paste0(file_name, part, ".pdf"))
    }
    server$close()
  } else {
    p
  }
  return(p)
}
