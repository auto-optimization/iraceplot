#' @keywords internal
"_PACKAGE"

#' The iraceplot package: \packageTitle{iraceplot}
#' @description
#'
#' The different functions used in iraceplot will allow the creation of different graphics. The functions are as follows:
#'
#' boxplot_test;
#' scatter_test;
#' parallel_coord;
#' parallel_cat;
#' sampling_frequency;
#' sampling_frequency_iteration;
#' sampling_pie;
#' plot_experiments_matrix;
#' sampling_distance;
#' boxplot_training;
#' scatter_training;
#' report;
#' plot_model;
#'
#' If you need information about any function you can put:
#' ?name_function
#'
#' If you need more information, go to the following page:
#' https://pabloonate.github.io/iraceplot/
#'
#' @name iraceplot-package
#' @docType package
#'
#' @details  License: MIT + file LICENSE
#'
#' @author Maintainers: Pablo Oñate Marín and Leslie Pérez Cáceres
#'         \email{pablo.onate.m@gmail.com}
#'
#' @keywords package plot automatic configuration
#'
#' @importFrom plotly ggplotly orca plot_ly add_trace
#' @importFrom tidyr pivot_longer
#' @importFrom forcats fct_relevel
#' @importFrom tibble rownames_to_column
#' @importFrom ggplot2 geom_tile scale_fill_viridis_c element_blank scale_color_viridis_c ggsave scale_shape_manual theme_bw scale_x_discrete scale_color_manual scale_size_manual scale_alpha_manual geom_point scale_fill_manual vars guide_legend facet_grid geom_bar geom_density geom_histogram ggtitle scale_y_continuous rel geom_line scale_x_continuous element_text scale_color_viridis_d ggplot geom_boxplot geom_jitter position_jitter aes theme labs scale_color_hue
#' @importFrom graphics text
#' @importFrom stats C na.omit reshape
#' @importFrom dplyr mutate %>% tibble group_by summarise n select arrange
#' @importFrom stringr str_split
#' @importFrom grDevices rainbow nclass.Sturges dev.off pdf
#' @importFrom viridis viridis
#' @importFrom ggridges geom_density_ridges
#' @importFrom gridExtra grid.arrange
#' @importFrom gridExtra marrangeGrob
#' @importFrom utils browseURL
#' @importFrom GGally ggparcoord
#' @importFrom ggforce geom_parallel_sets geom_parallel_sets_axes geom_parallel_sets_labels gather_set_data
#' @importFrom truncnorm dtruncnorm
#'
#'
NULL
