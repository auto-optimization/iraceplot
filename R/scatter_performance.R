#' Performance Scatter Plot of Two Configurations 
#'
#' Create a scatter plot that displays the performance of two configurations on
#' a provided experiment matrix. Each point in the plot represents an instance
#' and the color of the points indicates if one configuration is better than
#' the other.
#' 
#' The performance matrix is assumed to be provided in the format of the irace
#' experiment matrix thus, NA values are allowed. Consequently the number of
#' evaluations can differ between configurations due to the elimination process
#' applied by irace. This plot only shows performance data only for instances
#' in which both configurations are executed.
#'
#' @param experiments
#' Experiment matrix obtained from irace training or testing data. Configurations 
#' in columns and instances in rows. As in irace, column names (configurations ids) 
#' should be characters. Row names will be used as instance names.
#'
#' @param x_id,y_id Configuration IDs for x-axis and y-axis, respectively.
#' 
#' @template arg_rpd
#' @template arg_filename
#' @template arg_interactive
#'
#' @param instance_names Either a character vector of instance names in the
#'   same order as `rownames(experiments)` or a function that takes
#'   `rownames(experiments)` as input.
#'
#' @return [ggplot2::ggplot()] object
#'
#' @examples
#' iraceResults <- read_logfile(system.file(package="irace", "exdata",
#'                                          "irace-acotsp.Rdata", mustWork = TRUE))
#' best_id <- iraceResults$iterationElites[length(iraceResults$iterationElites)]
#' scatter_performance(iraceResults$experiments, x_id = 1, y_id = best_id)
#' @export
scatter_performance <- function(experiments, x_id, y_id, rpd = TRUE, 
                               filename = NULL, interactive = base::interactive(),
                               instance_names = NULL)
{
  x_id <- as.character(x_id)
  y_id <- as.character(y_id)
  # Verify that the entered id are within the possible range
  if (!(x_id %in% colnames(experiments))) stop("x_id out of range", x_id)
  if (!(y_id %in% colnames(experiments))) stop("y_id out of range", y_id)
  orig_instance_names <- rownames(experiments)
  if (is.null(orig_instance_names))
    orig_instance_names <- as.character(1:nrow(experiments))

  if (rpd) {
    experiments <- calculate_rpd(experiments)
    xlab <- paste0("RPD (%) of configuration ", x_id)
    ylab <- paste0("RPD (%) of configuration ", y_id)
  } else {
    xlab <- paste0("Cost of configuration ", x_id)
    ylab <- paste0("Cost of configuration ", y_id)
  }
  x_data <- experiments[, x_id]
  y_data <- experiments[, y_id]
  instances <- which(!is.na(x_data) & !is.na(y_data)) 
  # Select only rows that have data for both configurations.
  if (length(instances) == 0) stop("No instance has data for both configurations")
  x_data <- x_data[instances]
  y_data <- y_data[instances]
  best <- rep("equal", length(instances))
  best[x_data < y_data] <- "conf1"
  best[x_data > y_data] <- "conf2"
  
  if (is.null(instance_names)) {
    instance_names <- orig_instance_names[instances]
  } else if (is.function(instance_names)) {
    instance_names <- sapply(orig_instance_names[instances], instance_names)
  } else {
    if (length(instance_names) != nrow(x_data))
      stop("`instance_names` must have the same length as `nrow(experiments)`")
    instance_names <- instance_names[instances]
  }
  # A table is created with only the paired values
  data <- data.frame(conf1 = x_data, conf2 = y_data,
                     instance = instance_names, best = best)
  # Variable assignment
  conf1 <- conf2 <- instance <- best <- point_text <- NULL
  data <- data %>% mutate(point_text = paste0(instance, "\nx: ", conf1, "\ny: ", conf2))

  # The plot scatter is created and assigned to p
  q <- ggplot(data, aes(x = conf1, y = conf2, color = best, text = point_text)) +
    geom_abline(intercept = 0, slope = 1, color = "lightgray") +
    geom_point(show.legend=FALSE) +
    scale_color_manual(values=c(conf1="#6600CC", conf2="#00BFC4", equal="darkgray")) +
    theme(legend.position = 'none') +
    labs(color = " ", x = xlab, y = ylab)
  
  if (interactive) {
    q <- plotly::ggplotly(p=q, tooltip = "point_text")
    return(q)
  }
  
  # If the value in filename is added the pdf file is created
  if (!is.null(filename)) {
    ggsave(filename, plot = q)
    # If you do not add the value of filename, the plot is displayed
  } else {
    q
  }
  return(q)
}

#' @rdname scatter_performance
#'
#' @details
#' [scatter_training()] compares the performance of two configurations on the
#' training instances. The performance data is obtained from the evaluations
#' performed by irace during the execution process.
#'
#' @template arg_irace_results
#' @param ... Other arguments passed to [scatter_performance()]
#'
#'
#' @examples
#' iraceResults <- read_logfile(system.file(package="iraceplot", "exdata",
#'                                          "guide-example.Rdata", mustWork = TRUE))
#' scatter_training(iraceResults, x_id = 806, y_id = 809)
#' \donttest{ 
#' scatter_training(iraceResults, x_id = 806, y_id = 809, rpd = FALSE)
#' }
#' @export
scatter_training <- function(irace_results, ...)
{
  experiments <- irace_results$experiments
  rownames(experiments) <- get_instanceID_seed_pairs(irace_results, index = 1:nrow(experiments), instances=TRUE)[, "instance"]
  scatter_performance(experiments, ...)
}

#' @rdname scatter_performance
#'
#' @details
#' [scatter_test()] compares the performance of two configurations on the test
#' instances. The performance data is obtained from the test evaluations
#' performed by irace.  Note that testing is not enabled by default in irace
#' and should be enabled in the scenario setup. Moreover, configuration ids
#' provided in `x_id` and `y_id` should belong to elite configuration set
#' evaluated in the test (see the irace package user guide for more details).
#'
#' @template arg_irace_results
#' @param ... Other arguments passed to [scatter_performance()].
#'
#'
#' @examples
#' iraceResults <- read_logfile(system.file(package="iraceplot", "exdata",
#'                                          "guide-example.Rdata", mustWork = TRUE))
#' scatter_test(iraceResults, x_id = 92, y_id = 119)
#' \donttest{ 
#' scatter_test(iraceResults, x_id = 92, y_id = 119, rpd=FALSE)
#' }
#' @export
scatter_test <- function(irace_results, ...)
{
  if (!has_testing_data(irace_results))
    stop("irace_results does not contain the testing data")
  experiments <- irace_results$testing$experiments
  rownames(experiments) <- irace_results$scenario$testInstances
  scatter_performance(experiments, ...)
}
