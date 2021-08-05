#' Box Plot
#'
#' Create a graphic of box plot of irace using the best quality id
#' By default the graph of the last iteration is shown
#'
#' @template arg_irace_results
#'
#' @param number_iteration
#' Numeric, iteration number that should be included in the plot (example: number_iteration = 5)
#'
#' @param id_configurations
#' Numeric vector, configurations ids whose performance should be included in the plot
#' (example: id_configurations = c(20,50,100,300,500,600,700))
#'
#' @param rpd
#' Logical (default TRUE) to plot performance as the relative percentage deviation to best results
#'
#' @param file_name
#' String, File name to save plot (example: "~/patch/example/file_name.png")
#'
#' @return box plot
#'
#' @export
#'
#' @examples
#' boxplot_training(iraceResults)
#' boxplot_training(iraceResults, number_iteration = 5)
#' boxplot_training(iraceResults, id_configurations = c(20, 50, 100, 300, 500, 600, 700))
boxplot_training <- function(irace_results, number_iteration = NULL, id_configurations = NULL, rpd = TRUE, file_name = NULL) {

  # Variable assignment
  Performance <- Elite_configuration <- NULL
  long <- length(irace_results$allElites)

  if (!is.null(number_iteration) & !is.null(id_configurations)) {
    cat("Error: cannot use id_configurations and number_iteration at the same time\n")
    stop()
  }

  # It is checked if the file_name argument was added
  if (!is.null(number_iteration)) {
    # We verify that number_iteration is within the range of values it can take
    if (number_iteration > 0 && number_iteration <= long) {
      long <- number_iteration
      # If number_iteration is out of range it delivers a message per screen
    } else {
      cat("Error: iteration number out of range\n")
      stop()
    }
  }

  # A vector is created with the id of all elite configurations from the iteration entered
  id <- irace_results$allElites[[long]]

  if (!is.null(id_configurations)) {
    n_conf <- c(1:dim(irace_results$experiments)[2])
    if (FALSE %in% (id_configurations %in% n_conf)) {
      cat(paste("Error: the following settings are out of range:", id_configurations[!(id_configurations %in% n_conf)],"\n"))
      stop()
    } else {
      id <- id_configurations
    }
  }

  # A table is created with the values of all elite configurations of the id of the requested iteration
  experiments <- irace_results$experiments

  if (rpd == TRUE) {
    experiments <- 100 * (experiments - apply(experiments, 1, min, na.rm = TRUE)) / apply(experiments, 1, min, na.rm = TRUE)
  }

  matriz <- as.data.frame(experiments[, id])

  # If the length of id is one, a different value must be added to the column
  if (length(id) == 1) {
    colnames(matriz)[colnames(matriz) == "experiments[, id]"] <- id
  }

  # value of elements that the matrix contains
  n_row_col <- as.numeric(dim(matriz)[1] * dim(matriz)[2])

  # A restructured table is created
  tabla <- reshape(matriz,
    varying = c(as.character(id)),
    v.names = "Performance",
    timevar = "Elite_configuration",
    times = c(as.character(id)),
    new.row.names = 1:n_row_col,
    direction = "long"
  )
  y_lab <- "Performance"
  if (rpd) y_lab <- "RPD performance"
  
  # The plot scatter is created and assigned to p
  p <- ggplot(tabla, aes(x = Elite_configuration, y = Performance, color = Elite_configuration)) +
    geom_boxplot(na.rm = TRUE) +
    geom_jitter(shape = 16, position = position_jitter(0.2), na.rm = TRUE) +
    theme(legend.position = "none") +
    labs(x = "Elite Configurations", y=y_lab)

  # If the value in file_name is added the pdf file is created
  if (!is.null(file_name)) {
    ggsave(file_name, plot = p)
    # If you do not add the value of file_name, the plot is displayed
  } else {
    p
    return(p)
  }
}
