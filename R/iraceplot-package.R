#' @keywords internal
"_PACKAGE"

#' The iraceplot package: \packageTitle{iraceplot}
#' @description
#'
#' Graphical Visualization Tools for Analysing the Data Produced by Irace.
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
#' @importFrom dplyr mutate %>% group_by summarise select arrange count n_distinct
#' @importFrom ggforce geom_parallel_sets geom_parallel_sets_axes geom_parallel_sets_labels gather_set_data
#' @importFrom ggplot2 geom_tile scale_fill_viridis_c element_blank scale_color_viridis_c ggsave scale_shape_manual theme_bw scale_x_discrete scale_y_discrete scale_color_manual scale_size_manual scale_alpha_manual geom_point scale_fill_manual vars guide_legend facet_grid geom_bar geom_density geom_histogram ggtitle scale_y_continuous rel geom_line scale_x_continuous element_text scale_color_viridis_d ggplot geom_boxplot geom_violin geom_jitter position_jitter aes theme labs scale_color_hue xlab ylab geom_abline guide_colourbar element_rect
#' @importFrom grDevices rainbow nclass.Sturges dev.off pdf
#' @importFrom gridExtra grid.arrange marrangeGrob
#' @importFrom plotly ggplotly orca plot_ly add_trace
NULL
