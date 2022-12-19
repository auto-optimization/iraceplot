#' Heat Map Plot
#'
#' Creates a heatmap plot that shows all performance data seen by irace.
#' Configurations are shown in the x-axis in the order in which they are
#' created in the configuration process. Instances are shown in the y-axis in
#' the order in which they where seen during the configuration run. This plot
#' gives a general idea of the configuration process progression, the number of
#' evaluations of each configuration show how long they survived in the
#' iterated racing procedure.
#'
#' @template arg_irace_results
#'
#' @template arg_filename
#'
#' @param metric Cost metric shown in the plot: `"raw"` shows the raw
#'   values, `"rpd"` shows relative percentage deviation per instance and
#'   `"rank"` shows rank per instance.
#'
#' @param show_conf_ids If `TRUE`, it shows the configuration IDs in the x-axis. Usually there are too many configurations, thus the default is `FALSE`.
#'
#' @template arg_interactive
#' 
#' @return [ggplot2::ggplot()] object
#'
#' @examples
#' iraceResults <- read_logfile(system.file(package="irace", "exdata",
#'                                          "irace-acotsp.Rdata", mustWork = TRUE))
#' plot_experiments_matrix(iraceResults)
#' @export
plot_experiments_matrix <- function(irace_results, filename = NULL, metric = c("raw", "rpd", "rank"),
                                    show_conf_ids = FALSE, interactive = base::interactive())
{
  metric <- match.arg(metric)
  experiments <- irace_results$experiments
  conf_ids <- colnames(experiments)
  if (is.null(conf_ids)) conf_ids <- as.character(1:ncol(experiments))
  inst_ids <- rownames(experiments)
  if (is.null(inst_ids)) inst_ids <- as.character(1:nrow(experiments))
  
  if (metric == "rank") {
    experiments[] <- matrixStats::rowRanks(experiments, ties.method = "average")
    metric_lab <- "Rank"
    transform <- "log10"
  } else if (metric == "rpd") {
    experiments[] <- calculate_rpd(experiments)
    metric_lab <- "RPD"
    transform <- "log1p"
  } else { # == raw
    metric_lab <- "Cost"
    transform <- "identity"
  }

  conf_id <- inst_id <- cost <- text <- NULL
  # The table is created and organized for ease of use
  experiments <- tibble::as_tibble(experiments) %>%
    rownames_to_column("inst_id") %>%
    tidyr::pivot_longer(-c("inst_id"), names_to = "conf_id", values_to = "cost") %>%
    # We need to relevel so that they appear in the correct order
    mutate(conf_id = forcats::fct_relevel(conf_id, conf_ids)) %>%
    mutate(inst_id = forcats::fct_relevel(inst_id, inst_ids))

  # The text field is added to the table to show it in the interactive plot.
  if (interactive) {
    experiments <- experiments %>%
      mutate(text = paste0("Configuration: ", conf_id, "\nInstance: ", inst_id, "\nValue: ", cost, "\n"))
  } else {
    experiments <- experiments %>% mutate(text = "")
  }

  p <- ggplot(experiments, aes(x = conf_id, y = inst_id, fill = cost, text = text)) +
    geom_tile() + # Heatmap style
    scale_fill_viridis_c(na.value = "white", direction=-1L, trans = transform,
                         guide = guide_colourbar(barheight=grid::unit(0.8,"npc"))) +
    scale_y_discrete(expand=c(0L, 0L)) +
    labs(x = "Configuration IDs", y = "Instance IDs", fill = metric_lab) +
    # theme(legend.position = "top") + # This doesn't work because numbers often overlap
    theme(axis.ticks = element_blank(),
          panel.border = element_rect(colour="gray80", fill = NA, size=0.75),
          plot.background = element_blank(),
          panel.background = element_blank())

  if (!show_conf_ids) p <- p + theme(axis.text.x = element_blank())

  if (interactive) {
    p <- plotly::ggplotly(p, tooltip = "text")
    if (!is.null(filename)) orca_pdf(filename, p)
  } else if (!is.null(filename)) {
    ggsave(filename, plot = p)
  }
  p
}
