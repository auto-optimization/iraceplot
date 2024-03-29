#' The configurations by iteration and instance
#'
#' This is a simplified version of the visualization you can obtain with [`acviz`](https://github.com/souzamarcelo/acviz). This function is currently VERY SLOW.
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
  nconfig <- 0
  experiments <- as.data.frame(irace_results$experiments)

  if (rpd) experiments <- calculate_rpd(experiments)
  
  exp_log <- select(as.data.frame(irace_results$experimentLog), -time, -bound)
  value <- sample(NA, size = nrow(exp_log), replace = TRUE)
  execution <- sample(NA, size = nrow(exp_log), replace = TRUE)
  tabla <- cbind(exp_log, value, execution)

  # the values of each configuration are added to the table
  cont_exe <- 0
  # FIXME: This loop is too slow. What is it doing?
  for (i in 1:nrow(exp_log)) {
    for (j in 1:nrow(irace_results$experiments)) {
      if (!is.na(experiments[[tabla$configuration[i]]][j])) {
        cont_exe <- cont_exe + 1
        if (is.na(tabla$value[i])) {
          tabla$value[i] <- experiments[[tabla$configuration[i]]][j]
          tabla$execution[i] <- cont_exe
        } else {
          add <- tabla[i, ]
          add$value <- experiments[[tabla$configuration[i]]][j]
          add$execution <- cont_exe
          tabla <- rbind(tabla, add)
        }
      }
    }
  }

  # new columns are created and added to the table
  tabla <- tabla[order(tabla$execution), ]
  tabla <- cbind(tabla, type = NA, conf_it = NA, instance_it = NA, media_regular = NA, media_elite = NA,
                 regular_color = "median iteration", elite_color = "median elites")

  # FIXME: This code needs to be revised.
  # the data is added to the conf_it, instance_it and type columns
  for (j in 1:length(irace_results$allElites)) {
    nconfig <- max(tabla$execution[tabla$iteration == j])
    tabla$conf_it[tabla$iteration == j] <- nconfig
    tabla$instance_it[tabla$iteration == j] <- max(unique(tabla$instance[tabla$iteration == j]))
    is_elite <- tabla$configuration %in% irace_results$allElites[[j]]
    tabla$type[tabla$iteration == j & !is_elite] <- "regular config."
    if (j == length(irace_results$allElites)) {
      tabla$type[tabla$iteration == j & is_elite] <- "final elite config."
      tabla$type[tabla$iteration == j & (tabla$configuration %in% irace_results$allElites[[j]][1])] <- "best found config."
    } else {
      tabla$type[tabla$iteration == j & is_elite] <- "elite config."
    }
  }

  # The mean values are calculated in the configurations by iteration
  for (k in 1:length(irace_results$allElites)) {
    # FIXME: This is not the median but the mean???
    tabla$media_regular[tabla$iteration == k] <- mean(tabla$value[tabla$iteration == k])
    tabla$media_elite[tabla$iteration == k] <- mean(tabla$value[tabla$iteration == k & (tabla$type == "elite config." | tabla$type == "final elite config." | tabla$type == "best found config.")])
  }

  # Instance and configuration columns are converted to character
  tabla$instance[1] <- as.character(tabla$instance[1])
  tabla$configuration[1] <- as.character(tabla$configuration[1])

  # the text column is generated
  tabla <- tabla %>%
    mutate(text = paste0("execution: ", execution, "\n", "instance: ", instance, "\n", "configuration: ", configuration, "\n"))

  # the execution column is passed to factor and added to the table
  exe_factor <- factor(tabla$execution)
  levels(exe_factor) <- tabla$execution
  tabla <- cbind(tabla, exe_factor)
  text <- type <- media_elite <- elite_color <- media_regular <- regular_color <- NULL # Silence CRAN warning

  # point plot creation
  p <- ggplot(tabla, aes(x = exe_factor, y = value, color = instance, text = text)) +
    geom_point(aes(shape = type, size = type, alpha = type)) +
    facet_grid(cols = ggplot2::vars(tabla$instance_it), scales = "free_x", space = "free_x") +
    scale_shape_manual(values = c(22, 21, 24, 4)) +
    scale_color_manual(values = c(rainbow(n_distinct(tabla$instance)), "red", "orange"), breaks = c("median elites", "median iteration")) +
    scale_size_manual(values = c(2, 2, 2, 0.5)) +
    scale_alpha_manual(values = c(0.8, 0.6, 1, 0.2)) +
    scale_x_discrete(breaks = c(1, unique(tabla$conf_it))) +
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
