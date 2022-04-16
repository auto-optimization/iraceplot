#' Heat Map Plot
#'
#' Creates a heatmap plot that shows all performance data seen by irace.
#' Configurations are shown in the x axis in the order in which are created in
#' the configuration process. Instances are shown in the y axis in the order in
#' which where executed in the configuration process. The colors show the
#' overall rank (across all performance data) of the performance evaluation
#' displayed.
#' 
#' This plot gives a general idea of the configuration process progression, the
#' number of evaluations of each configuration show how long they survived in the 
#' iterated racing procedure.
#'
#' @template arg_irace_results
#'
#' @template arg_filename
#'
#' @template arg_interactive
#'
#' @return heatmap plot
#'
#' @examples
#' plot_experiments_matrix(iraceResults)
#' @export
plot_experiments_matrix <- function(irace_results, filename = NULL, interactive = base::interactive())
{
  # Variable assignment
  C <- Rank <- text <- i_id <- union <- NULL

  # The values of the experiments are assigned to the variable experiments
  experiments <- irace_results$experiments
  experiments[] <- rank(experiments, na.last = "keep")

  # he table is created and organized for ease of use
  tabla <- experiments %>%
    as.data.frame() %>%
    rownames_to_column("i_id") %>%
    pivot_longer(-c(i_id), names_to = "C", values_to = "Rank") %>%
    mutate(C = fct_relevel(C, as.character(1:ncol(experiments)))) %>%
    mutate(i_id = fct_relevel(i_id, as.character(1:nrow(experiments))))

  # The text field was added to the table to show it in the interactive plot
  tabla <- tabla %>%
    mutate(text = paste0("x: ", C, "\n", "y: ", i_id, "\n", "Value: ", round(Rank, 2), "\n"))

  # Heat map plot is created
  p <- ggplot(tabla, aes(x = C, y = i_id, fill = Rank, text = text)) +
    geom_tile() +
    scale_fill_viridis_c(na.value = "white") +
    labs(x = "Configurations", y = "Instances") +
    theme(axis.text.x = element_blank(), axis.ticks = element_blank())

  # The plot becomes interactive
  if (interactive) {
    p <- plotly::ggplotly(p, tooltip = "text")
    # If the value in filename is added the pdf file is created
    if (!is.null(filename)) {
      # The filename value is worked to separate it and assign it to new values.
      nameFile <- basename(filename)
      directory <- paste0(dirname(filename), sep = "/")
      withr::with_dir(directory, orca(p, paste0(nameFile, ".pdf")))
    }
  } else if (!is.null(filename)) {
    ggsave(filename, plot = p)
  }
  return(p)
}
