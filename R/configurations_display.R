#' The configurations by iteration and instance
#'
#' This is a simplified version of the visualization you can obtain with
#' [`acviz`](https://github.com/souzamarcelo/acviz). This function is currently
#' VERY SLOW.
#'
#' @template arg_irace_results
#'
#' @template arg_rpd
#' 
#' @template arg_filename
#' 
#' @template arg_interactive
#'
#' @return [ggplot2::ggplot()] object
#'
#' @examples
#' \donttest{
#' iraceResults <- read_logfile(system.file(package="iraceplot", "exdata",
#'                                          "guide-example.Rdata", mustWork = TRUE))
#' configurations_display(iraceResults)
#' }
#' @export
configurations_display <- function(irace_results, rpd = TRUE, filename = NULL, interactive = base::interactive())
{
  # FIXME: This function takes a long time.
  # variable assignment
  time <- bound <- instance <- configuration <- iteration <- nconfig <- cont_exe <- NULL
  nconfig <- 0L
  experiments <- irace_results$experiments
  if (rpd) experiments <- calculate_rpd(experiments)
  experiments <- as.data.frame(irace_results$experiments)

  exp_log <- irace_results$state$experiment_log
  exp_log[, let(time=NULL,bound=NULL,value=as.numeric(NA),execution=as.numeric(NA))]

  # the values of each configuration are added to the table
  cont_exe <- 0L
  # FIXME: This loop is too slow. What is it doing?
  # FIXME: Use data.table
  for (i in seq_nrow(exp_log)) {
    for (j in seq_nrow(irace_results$experiments)) {
      if (!is.na(experiments[[exp_log$configuration[i]]][j])) {
        cont_exe <- cont_exe + 1L
        if (is.na(exp_log$value[i])) {
          exp_log$value[i] <- experiments[[exp_log$configuration[i]]][j]
          exp_log$execution[i] <- cont_exe
        } else {
          add <- exp_log[i, ]
          add$value <- experiments[[exp_log$configuration[i]]][j]
          add$execution <- cont_exe
          exp_log <- rbind(exp_log, add)
        }
      }
    }
  }

  # new columns are created and added to the table
  exp_log <- exp_log[order(exp_log$execution), ]
  exp_log <- cbind(exp_log, type = NA, conf_it = NA, instance_it = NA, media_regular = NA, media_elite = NA,
                 regular_color = "median iteration", elite_color = "median elites")

  # FIXME: This code needs to be revised.
  # the data is added to the conf_it, instance_it and type columns
  for (j in seq_along(irace_results$allElites)) {
    nconfig <- max(exp_log$execution[exp_log$iteration == j])
    exp_log$conf_it[exp_log$iteration == j] <- nconfig
    exp_log$instance_it[exp_log$iteration == j] <- max(unique(exp_log$instance[exp_log$iteration == j]))
    is_elite <- exp_log$configuration %in% irace_results$allElites[[j]]
    exp_log$type[exp_log$iteration == j & !is_elite] <- "regular config."
    if (j == length(irace_results$allElites)) {
      exp_log$type[exp_log$iteration == j & is_elite] <- "final elite config."
      exp_log$type[exp_log$iteration == j & (exp_log$configuration %in% irace_results$allElites[[j]][1])] <- "best found config."
    } else {
      exp_log$type[exp_log$iteration == j & is_elite] <- "elite config."
    }
  }

  # The mean values are calculated in the configurations by iteration
  for (k in seq_along(irace_results$allElites)) {
    # FIXME: This is not the median but the mean???
    exp_log$media_regular[exp_log$iteration == k] <- mean(exp_log$value[exp_log$iteration == k])
    exp_log$media_elite[exp_log$iteration == k] <- mean(exp_log$value[exp_log$iteration == k & (exp_log$type == "elite config." | exp_log$type == "final elite config." | exp_log$type == "best found config.")])
  }

  # Instance and configuration columns are converted to character
  exp_log$instance[1] <- as.character(exp_log$instance[1])
  exp_log$configuration[1] <- as.character(exp_log$configuration[1])

  execution <- NULL # CRAN warning
  # the text column is generated
  exp_log <- exp_log %>%
    mutate(text = paste0("execution: ", execution, "\n", "instance: ", instance, "\n", "configuration: ", configuration, "\n"))

  # the execution column is passed to factor and added to the table
  exe_factor <- factor(exp_log$execution)
  levels(exe_factor) <- exp_log$execution
  exp_log <- cbind(exp_log, exe_factor)
  value <- text <- type <- media_elite <- elite_color <- media_regular <- regular_color <- NULL # Silence CRAN warning

  # point plot creation
  p <- ggplot(exp_log, aes(x = exe_factor, y = value, color = instance, text = text)) +
    geom_point(aes(shape = type, size = type, alpha = type)) +
    facet_grid(cols = ggplot2::vars(exp_log$instance_it), scales = "free_x", space = "free_x") +
    scale_shape_manual(values = c(22, 21, 24, 4)) +
    scale_color_manual(values = c(rainbow(n_distinct(exp_log$instance)), "red", "orange"), breaks = c("median elites", "median iteration")) +
    scale_size_manual(values = c(2, 2, 2, 0.5)) +
    scale_alpha_manual(values = c(0.8, 0.6, 1, 0.2)) +
    scale_x_discrete(breaks = c(1, unique(exp_log$conf_it))) +
    labs(
      x = "Candidate evaluations",
      y = "RPD",
      subtitle = "Instances evaluated"
    ) +
    theme(
      axis.text.x = element_text(angle = 90),
      axis.ticks.x = element_blank(),
      plot.subtitle = element_text(hjust = 0.5),
      strip.text.x = element_text(size = 8),
      legend.position = "right",
      legend.title = element_blank()
    ) +
    geom_point(mapping = aes(y = media_elite, color = elite_color), size = 0.1, ) +
    geom_point(mapping = aes(y = media_regular, color = regular_color), size = 0.1)

  if (interactive)
    p <- plotly::ggplotly(p, tooltip = "text")
 
  # If the value in filename is added the pdf file is created
  if (!is.null(filename)) {
    ggsave(filename, plot = p)
    # If you do not add the value of filename, the plot is displayed
  } else {
    p
  }
  return(p)
}
