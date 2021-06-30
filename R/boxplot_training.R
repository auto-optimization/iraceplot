#' Box Plot Training
#'
#' The `boxplot_training` function creates a box plot that displays the performance
#' of a set of configurations on the training instances. Performance data is obtained 
#' from the evaluations performed by irace during the execution process. This implies 
#' that the number of evaluations can differ between configurations. 
#' 
#'
#' @template arg_irace_results
#'
#' @param iteration
#' Numeric, iteration number that should be included in the plot (example: iteration = 5)
#' When no iteration and no id_condigurations are provided, the iterations is assumed to be
#' the last one performed by irace. 
#' 
#' The performance data is obtained from the evaluations performed by irace 
#' during the execution process. This implies that the number of evaluations 
#' can differ between configurations due to the elimination process applied by 
#' irace. This plot, consequently, does not provide a complete compaarison of
#' two configurations, for a fair comparison use the test data plot.
#'
#' @param id_configurations
#' Numeric vector, configurations ids whose performance should be included in the plot.
#' If no ids are provided, the configurations ids are set as the elite configuration ids 
#' of the selected iteration (last iteration by default) (example: id_configurations = c(20,50,100,300,500,600,700)).
#'
#' @template arg_rpd
#' 
#' @param show_points
#' Logical, (default TRUE) TRUE to plot performance points together with the box plot.
#'
#' @param file_name
#' String, file name to save plot (example: "~/patch/example/file_name.png")
#'
#' @return box plot
#'
#' @export
#'
#' @examples
#' boxplot_training(iraceResults)
#' boxplot_training(iraceResults, rpd = FALSE)
#' boxplot_training(iraceResults, iteration = 5)
#' boxplot_training(iraceResults, id_configurations = c(20, 50, 100, 300, 500, 600, 700))
boxplot_training <- function(irace_results, iteration = NULL, id_configurations = NULL, rpd = TRUE, show_points=TRUE, file_name = NULL) {

  # Variable assignment
  Performance <- Elite_configuration <- NULL
  long <- length(irace_results$allElites)

  if (!is.null(iteration) & !is.null(id_configurations)) {
    cat("Error: cannot use id_configurations and iteration at the same time\n")
    stop()
  }

  # It is checked if the file_name argument was added
  if (!is.null(iteration)) {
    # We verify that iteration is within the range of values it can take
    if (iteration > 0 && iteration <= long) {
      long <- iteration
      # If iteration is out of range it delivers a message per screen
    } else {
      cat("Error: iteration number out of range\n")
      stop()
    }
  }

  # A vector is created with the id of all elite configurations from the iteration entered
  id <- irace_results$allElites[[long]]

  if (!is.null(id_configurations)) {
    n_conf <- 1:dim(irace_results$experiments)[2]
    if (FALSE %in% (id_configurations %in% n_conf)) {
      cat(paste("Error: the following settings are out of range:", id_configurations[!(id_configurations %in% n_conf)],"\n"))
      stop()
    } else {
      id <- id_configurations
    }
  }

  # A table is created with the values of all elite configurations of the id of the requested iteration
  experiments <- irace_results$experiments

  if (rpd) {
    experiments <- calculate_rpd(experiments)
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
    theme(legend.position = "none") +
    labs(x = "Elite Configurations", y=y_lab)
  
  if (show_points) 
    p <- p + geom_jitter(shape = 16, position = position_jitter(0.2), alpha=0.2, na.rm = TRUE) 

  # If the value in file_name is added the pdf file is created
  if (!is.null(file_name)) {
    ggsave(file_name, plot = p)
    # If you do not add the value of file_name, the plot is displayed
  } else {
    p
    return(p)
  }
}

calculate_rpd <- function(x)
{
  100 * (x - apply(x, 1, min, na.rm = TRUE)) / apply(x, 1, min, na.rm = TRUE)
}
