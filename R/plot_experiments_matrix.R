#' Heat Map Plot
#'
#' Creates a heatmap plot that shows all performance data seen by irace.
#' Configurations are shown in the x-axis in the order in which they are
#' created in the configuration process. Instances are shown in the y-axis in
#' the order in which they where seen during the configuration run. This plot
#' gives a general idea of the configuration process progression, the number of
#' evaluations of each configuration show how long they survived in the
#' iterated racing procedure.  Rejected configurations are shown with a red `X`.
#'
#' @inheritParams boxplot_performance 
#'
#' @param metric Cost metric shown in the plot: `"raw"` shows the raw
#'   values, `"rpd"` shows relative percentage deviation per instance and
#'   `"rank"` shows rank per instance.
#'
#' @param show_conf_ids (`logical(1)`)\cr  If `TRUE`, it shows the configuration IDs in the x-axis. The default `NA`,
#' only shows them if there are no more than 25.
#'
#' @note
#'
#' Alternatively, `experiments` could be the data generated when loading the
#' `.Rdata` file created by `irace` (or the filename of that file), from which
#' the experiments matrix will be loaded.
#' 
#' @return [ggplot2::ggplot()] object
#'
#' @examples
#' iraceResults <- read_logfile(system.file(package="irace", "exdata",
#'                                          "irace-acotsp.Rdata", mustWork = TRUE))
#' plot_experiments_matrix(iraceResults)
#'
#' plot_experiments_matrix(read_logfile(system.file(package="iraceplot", "exdata",
#'                                          "dummy-reject.Rdata", mustWork = TRUE)))
#' @export
plot_experiments_matrix <- function(experiments, filename = NULL, metric = c("raw", "rpd", "rank"),
                                    show_conf_ids = FALSE, interactive = base::interactive())
{
  metric <- match.arg(metric)
  if (is.character(experiments)) {
    experiments <- read_logfile(experiments)$experiments
  } else if (!is.matrix(experiments)) {
    experiments <- experiments$experiments
  }
  conf_ids <- colnames(experiments)
  if (is.null(conf_ids)) conf_ids <- as.character(seq_ncol(experiments))
  inst_ids <- rownames(experiments)
  if (is.null(inst_ids)) inst_ids <- as.character(seq_nrow(experiments))

  if (is.na(show_conf_ids))
    show_conf_ids <- length(conf_ids) <= 25
  
  has_inf <- any(is.infinite(experiments))
  # FIXME: These transformations remove Inf, so we cannot highlight them
  # later. we should save the location of Inf, then restore them.
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

  # The table is created and organized for ease of use
  experiments <- tibble::as_tibble(experiments) %>%
    rownames_to_column("inst_id") %>%
    tidyr::pivot_longer(-c("inst_id"), names_to = "conf_id", values_to = "cost") %>%
    # We need to relevel so that they appear in the correct order
    mutate(conf_id = forcats::fct_relevel(.data$conf_id, conf_ids)) %>%
    mutate(inst_id = forcats::fct_relevel(.data$inst_id, inst_ids)) %>%
    # Replace negative infinity to help with plotting
    mutate(rejected = is.infinite(.data$cost))
  
  # The text field is added to the table to show it in the interactive plot.
  if (interactive) {
    experiments <- experiments %>%
      mutate(text = paste0("Configuration: ", .data$conf_id, "\nInstance: ", .data$inst_id, "\nValue: ", .data$cost, "\n"))
  } else {
    experiments <- experiments %>% mutate(text = "")
  }
  # Remove Inf before plotting.
  experiments <- experiments %>% mutate(cost = ifelse(is.infinite(.data$cost), NA, .data$cost))

  p <- ggplot(experiments, aes(x = .data$conf_id, y = .data$inst_id, fill = .data$cost, text = .data$text)) +
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

  # Show an X for rejected configurations.
  if (has_inf) {
    p <- p + geom_point(data = experiments %>% dplyr::filter(.data$rejected), show.legend = FALSE,
                        aes(x = .data$conf_id, y = .data$inst_id), size=2, shape=4, fill=NA, color="red") +
      guides(size= "none") # This is needed because plotly ignores guide="none".
  }
      
  if (!show_conf_ids)
    p <- p + theme(axis.text.x = element_blank())

  if (interactive) {
    p <- plotly::ggplotly(p, tooltip = "text")
    if (!is.null(filename)) orca_pdf(filename, p)
  } else if (!is.null(filename)) {
    ggsave(filename, plot = p)
  }
  p
}
