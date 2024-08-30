#' Box Plot of the performance of a set of configurations
#'
#' Creates a box plot that displays the performance of a set of configurations
#' which can be displayed by iteration.
#' 
#' The performance data is obtained from the experiment matrix provided in the 
#' experiments argument. The configurations can be selected using the allElites
#' argument and this argument can be also used to define the iteration of each
#' elite configuration was evaluated.
#'
#' @param experiments `matrix()`\cr Experiment matrix obtained from irace
#'   training or testing data. Configurations in columns and instances in
#'   rows. As in irace, column names (configurations IDs) should be characters.
#' 
#' @param allElites
#' List or vector of configuration ids, (default NULL). These configurations
#' should be included in the plot. If the argument is not provided all configurations
#' in experiments are included. If allElites is a vector all configurations are
#' assumed without iteration unless argument `type="ibest"` is provided, in which case
#' each configuration is assumed to be from a different iteration. If `allElites` 
#' is a list, each element of the  list is assumed as an iteration.
#'
#' @param type
#' String, (default "all") possible values are "all" or "ibest". "all" 
#' shows all the selected configurations showing iterations if the information 
#' is provided. "ibest" shows the elite configurations of each iteration, note
#' that the best configuration is always assumed to be first in the vector of 
#' each iteration.
#' 
#' @param first_is_best
#' Boolean (default TRUE) Enables the display in a different color the best configuration
#' identified as the first one in a vector. If FALSE, all configurations are shown
#' in the same color.
#' 
#' @template arg_rpd
#'
#' @template arg_show_points
#' 
#' @param best_color 
#' String, (default `"#08bfaa"`) color to display best configurations.
#' 
#' @param xlab 
#' String, (default `"Configurations"`) label for the x axis.
#'
#' @param boxplot By default, display a violin plot ([ggplot2::geom_violin()]).
#' If `TRUE`, show a classical boxplot.
#'
#' @template arg_filename
#' @template arg_interactive
#' 
#' @template ret_boxplot
#'
#' @seealso [boxplot_test()] [boxplot_training()]
#' @examples
#' iraceResults <- read_logfile(system.file(package="irace", "exdata",
#'                                          "irace-acotsp.Rdata", mustWork = TRUE))
#' boxplot_performance(iraceResults$experiments, iraceResults$allElites)
#' \donttest{ 
#' boxplot_performance(iraceResults$testing$experiments, iraceResults$iterationElites)
#' }
#' @export
boxplot_performance <- function(experiments, allElites= NULL, type = c("all", "ibest"),
                                first_is_best = TRUE, rpd = TRUE, show_points=TRUE, 
                                best_color = "#08bfaa", xlab ="Configurations", boxplot = FALSE, 
                                filename = NULL, interactive = base::interactive())
{
  type <- match.arg(type)
  if (!is.matrix(experiments) && !is.data.frame(experiments)) {
    cli_abort("'{.field experiments}' must be a matrix or a data frame")
  }
  inst_ids <- rownames(experiments)
  if (is.null(inst_ids)) inst_ids <- as.character(1:nrow(experiments))
  
  if (type == "ibest" && !first_is_best) {
    cli_alert_info("Note: The setting {.code 'type=ibest'} only supports {.code 'first_is_best=TRUE'}, ignoring this setting.\n")
    first_is_best <- TRUE
  }
  
  # Get the order of configurations
  if (is.null(allElites)) {
    cli_alert_info("Note: {.field allElites} not provided, assumming all configurations in experiments as elites.\n")
    allElites <- list()
    allElites[[1L]] <- get_ranked_ids(experiments)
    if (type == "ibest") {
      cli_abort("The {.field type} argument provided ({type}) is not supported when no {.field allElites} value provided")
    }
  } else if (type == "ibest" && !is.list(allElites)) {
    cli_alert_info("Note: Since {.code type=ibest}, assuming vector best configuration by iteration in {.field allElites}.\n")
    allElites <- as.list(allElites)
  }

  if (rpd) experiments <- calculate_rpd(experiments)
   
  # Generate iteration and final elite vector
  iterationElites <- finalElites <- allElites
  if (is.list(allElites)) 
    iterationElites <- unlist(lapply(allElites, function(x) x[1]))
  
  # Select experiments that will be used
  if (type == "all") {
    v_allElites <- as.character(unique(unlist(allElites)))
    if (!all(v_allElites %in% colnames(experiments))) 
      stop("Missing elite data in experiments matrix:", paste0(setdiff(v_allElites, colnames(experiments)), collapse=", "))
  } else {
    stopifnot(type == "ibest")
    if (!all(iterationElites %in% colnames(experiments)))
      stop("Missing iteration elites data in experiments matrix")
    v_allElites <- as.character(iterationElites)
  }
  # FIXME: It doesn't make sense to rename experiments to data. Just use either
  # of the names.
  experiments <- as.data.frame(experiments[,v_allElites, drop=FALSE])
  names_col <- colnames(experiments)
  # If we only have one row, then don't try to plot boxplots.
  plot_points <- (nrow(experiments) == 1L)

  # FIXME: This is too complicated and unclear.
  reshape_data <- function(x, elites) {
    out <- NULL
    for (i in seq_along(elites)) {
      d <- x[, as.character(elites[[i]]), drop=FALSE]
      d <- reshape(d,
                   varying = as.vector(colnames(d)),
                   v.names = "performance",
                   timevar = "ids",
                   times = as.vector(colnames(d)),
                   new.row.names = 1:(nrow(d) * ncol(d)),
                   direction = "long")
      d <- d[!is.na(d[,"performance"]),]
      d[,"iteration"] <- i
      out <- rbind(out, d)
    }
    out
  }
  # Get the experiment data together with the iterations
  if (is.list(allElites)) {
    experiments <- reshape_data(experiments, if (type == "all") allElites else iterationElites)
    experiments$iteration_f <- factor(experiments$iteration, levels = unique(experiments$iteration))
  } else {
    experiments <- reshape(experiments,
                    varying = as.character(colnames(experiments)),
                    v.names = "performance",
                    timevar = "ids",
                    times = as.character(colnames(experiments)),
                    new.row.names = 1:(nrow(experiments) * ncol(experiments)),
                    direction = "long")
    experiments <- experiments[!is.na(experiments[,"performance"]),]
    experiments$iteration <- 0
  }
  
  # Mark best configurations
  if (type == "all") {
    experiments$best_conf <- rep("none", nrow(experiments))
    if (first_is_best) {
      if (is.list(allElites)) {
        for (i in 1:length(allElites)) {
          experiments$best_conf[experiments$iteration == i & experiments$ids == as.character(iterationElites[i])] <- "best" 
        }
      } else {
        experiments$best_conf[experiments$ids == as.character(iterationElites[1])] <- "best" 
      }
    } 
  } 
  experiments$ids_f <- factor(experiments$ids, levels = unique(experiments$ids))
  # FIXME: This should include the instance and seed.
  # experiments$label <- paste0("Iteration: ", experiments$iteration_f, "\nValue: ", experiments$performance, "\n")
  # FIXME: Simplify these conditions to avoid repetitions.
  if (type == "ibest") {
    p <- ggplot(experiments, aes(x = .data$ids_f, y = .data$performance, colour = .data$iteration_f)) +
      labs(subtitle = "Iterations") +
      theme(plot.subtitle = element_text(hjust = 0.5))
  } else {
    # type="all"
    if (first_is_best) {
      p <- ggplot(experiments, aes(x = .data$ids_f, y = .data$performance, colour = .data$best_conf)) +
        scale_color_manual(values=c(best_color, "#999999"))
    } else if (is.list(allElites)){
      p <- ggplot(experiments, aes(x = .data$ids_f, y = .data$performance, colour = .data$iteration_f)) 
    } else {
      p <- ggplot(experiments, aes(x = .data$ids_f, y = .data$performance, colour = .data$ids_f)) 
    }
    
    if (is.list(allElites)) {
      p <- p + labs(subtitle = "Iterations")
      p <- p + theme(plot.subtitle = element_text(hjust = 0.5),
                     axis.text.x = element_text(size = 6.4, angle = 90)) 
    } else {
      p <- p + theme(plot.subtitle = element_text(hjust = 0.5)) 
    }
  }

  if (plot_points) {
    p <- p + geom_point(shape = 16, na.rm = TRUE)
  } else {
    # FIXME: Plot mean as a dashed line
    if (boxplot)
      p <- p + geom_boxplot()
    else
      p <- p + geom_violin(draw_quantiles = c(0.25, 0.5, 0.75))
    if (show_points)
      p <- p + geom_jitter(shape = 16, position = position_jitter(0.2), alpha=0.2, na.rm = TRUE)
  }
  ylab <- if (rpd) "RPD (%)" else "Cost (raw)"
  p <- p + theme(legend.position = "none") + labs(x = xlab, y = ylab)
      
  # each box plot is divided by iteration
  if (is.list(allElites)) {
    p <- p + facet_grid(cols = ggplot2::vars(experiments$iteration_f), scales = "free")
  }
  
  # If the value in filename is added the pdf file is created
  if (!is.null(filename)) ggsave(filename, plot = p)
 
  if (interactive) {
    # FIXME: ggplotly does not work well with geom_violin(): https://github.com/plotly/plotly.R/issues/1400
    # We need to create the violin plot directly in plotly: https://plotly.com/r/violin/
    if (!boxplot)
      p <- p + geom_boxplot(color="gray20", fill=NA, outlier.shape = NA)
    p <- plotly::ggplotly(p)
  }
  p
}

get_ranked_ids <- function(experiments)
{
  allElites <- c()
  naExp <- sort(colSums(!is.na(experiments)), decreasing = TRUE)
  for (r in unique(naExp)) {
    confId <- names(naExp[naExp == r])
    confMean <- colMeans(experiments[, confId, drop=FALSE], na.rm = TRUE)
    allElites <- c(allElites, names(sort(confMean)))
  }
  allElites
}
