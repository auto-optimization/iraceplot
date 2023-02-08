#' @keywords internal
"_PACKAGE"

#' The iraceplot package: \packageTitle{iraceplot}
#' @description
#'
#' Graphical Visualization Tools for Analysing the Data Produced by irace.
#'
#' boxplot_performance;
#' boxplot_test;
#' boxplot_training;
#' parallel_cat;
#' parallel_coord2;
#' parallel_coord;
#' plot_experiments_matrix;
#' plot_model;
#' report;
#' sampling_distance;
#' sampling_frequency;
#' sampling_frequency_iteration;
#' sampling_heatmap2;
#' sampling_heatmap;
#' sampling_pie;
#' scatter_performance;
#' scatter_test;
#' scatter_training;
#'
#' If you need information about any function you can write:
#' ?name_function
#'
#' If you need more information, go to the following page:
#' https://auto-optimization.github.io/iraceplot/
#'
#' @name iraceplot-package
#' @docType package
#'
#' @details  License: MIT + file LICENSE
#'
#' @author Maintainers: Pablo Oñate Marín and Leslie Pérez Cáceres and Manuel López-Ibañez
#'         \email{leslie.perez@pucv.cl}
#'
#' @keywords package plot automatic configuration
#'
#' @import stats
#' @import tibble
#' @import irace
#' @importFrom cli cli_warn cli_inform cli_abort cli_alert_info cli_alert_warning
#' @importFrom dplyr mutate %>% group_by summarise select arrange count n_distinct slice
#' @importFrom ggplot2 aes after_stat coord_cartesian element_blank element_rect element_text facet_grid geom_abline geom_bar geom_blank geom_boxplot geom_density geom_histogram geom_jitter geom_line geom_point geom_tile geom_violin ggplot ggsave ggtitle guides guide_axis guide_colourbar guide_legend labs position_jitter rel scale_alpha_manual scale_color_hue scale_color_manual scale_color_viridis_c scale_color_viridis_d scale_fill_manual scale_fill_viridis_c scale_shape_manual scale_size_manual scale_x_continuous scale_x_discrete scale_y_continuous scale_y_discrete stat_summary theme theme_bw
#' @importFrom grDevices rainbow nclass.Sturges dev.off pdf
#' @importFrom gridExtra grid.arrange marrangeGrob
# Use .data$col within aes() to silence CRAN warnings: https://ggplot2.tidyverse.org/articles/ggplot2-in-packages.html#using-aes-and-vars-in-a-package-function
#' @importFrom rlang .data
NULL
