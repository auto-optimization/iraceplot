#' Sunburst Plot
#'
#' @description
#' The isunburst function will return a sunburst plot of the categorical parameters
#'
#' @template arg_irace_results
#' @param parameters
#' String vector, a set of categorical type parameters
#' (example: parameters = c("algorithm","dlb"))
#' @param file_name
#' String, A pdf will be created in the location and with the assigned
#' name (example: "~/patch/example/file_name")
#' @return sunburst plot
#' @export
#'
#' @examples
#' sampling_pie(iraceResults)
#' sampling_pie(iraceResults, parameters = c("algorithm", "dlb"))
sampling_pie <- function(irace_results, parameters = NULL, file_name = NULL) {

  # variable assignment
  param_c <- parents <- labels <- values <- ids <- depend <- NULL

  dependency <- FALSE
  # Logical (default FALSE) that allows to verify if the parameters
  # are dependent on others, modifying the visualization of the plot

  # assigns categorical type parameters to param_c
  for (i in 1:length(irace_results$parameters$types)) {
    if (irace_results$parameters$types[[i]] == "c") {
      param_c <- c(param_c, names(irace_results$parameters$types)[i])
    }
  }

  if (!is.null(parameters)) {
    if (FALSE %in% (parameters %in% param_c)) {
      print("Only categorical data can be used")
      return(paste("The following parameters are not found:", parameters[!(parameters %in% param_c)]))
    } else {
      param_c <- unique(parameters)
    }
  }

  # the table is generated only with categorical parameters
  data <- as.data.frame(irace_results$allConfigurations[param_c])

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

  # the graph is created
  p <- plot_ly(
    type = "sunburst",
    ids = data_f$ids,
    labels = data_f$labels,
    parents = data_f$parents,
    values = data_f$values,
    branchvalues = "total"
  )

  # If the value in file_name is added the pdf file is created
  if (!is.null(file_name)) {
    # The file_name value is worked to separate it and assign it to new values.
    nameFile <- basename(file_name)
    directory <- paste0(dirname(file_name), sep = "/")
    withr::with_dir(directory, orca(p, paste0(nameFile, ".pdf")))

    # If you do not add the value of file_name, the plot is displayed
  } else {
    p
    return(p)
  }
}
