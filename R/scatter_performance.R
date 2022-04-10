#' Scatter Performance Plot 
#'
#' @description
#' The `scatter_performance` function creates a scatter plot that displays the performance of two
#' configurations on a provided experiment matrix. Each point in the plot represents an 
#' instance and the color of the points indicates if one configuration is better 
#' than the other.
#' 
#' The performance matrix is assumed to be provided in the format of the irace 
#' experiment matrix thus, NA values are allowed. Consequently the number of evaluations 
#' can differ between configurations due to the elimination process applied by 
#' irace. This plot only shows performance data only for instances in which both
#' configurations are executed.
#'
#' @param experiments
#' Experiment matrix obtained from irace training or testing data. Configurations 
#' in columns and instances in rows. As in irace, column names (configurations ids) 
#' should be characters. Row names will be used as instance names.
#'
#' @param id_configurations
#' Numeric vector, configuration ids whose performance should be displayed 
#' (example: id_configurations = c("92","119"))
#'
#' @template arg_rpd
#'
#' @template arg_filename
#'
#' @template arg_interactive
#'
#' @return `ggplot()` object
#'
#' @examples
#' scatter_performance(iraceResults$experiments, id_configurations = c(806, 809))
#' scatter_performance(iraceResults$testing$experiments, id_configurations = c(803, 809), rpd = FALSE)
#' @export
scatter_performance<- function(experiments, id_configurations, rpd = TRUE, 
                               filename = NULL, interactive = base::interactive()) {
  
  # Variable assignment
  instances <- conf1 <- conf2<- instance <- best <- point_text <- NULL
  
  # Verify that a vector of length 2 is entered
  if (length(id_configurations) != 2) {
    stop("Error: You must provide a vector with 2 values\n")
  }
  
  # Verify that the entered id are within the possible range
  if (!(id_configurations[1] %in% colnames(experiments))) {
    stop(paste0("Error: id out of range", id_configurations[1], "\n"))
  } else if (!(id_configurations[2] %in% colnames(experiments))) {
    stop(paste("Error: id out of range", id_configurations[2], "\n"))
  }
  
  
  if (rpd) {
    experiments <- calculate_rpd(experiments)
  }
  
  # An array of true and/or false to display if the field has data
  not_na <- !is.na(experiments[, as.character(id_configurations)])
  
  # A vector is created only with paired instances (both true)
  for (i in 1:nrow(not_na)) {
    if (all(!is.na(not_na[i, ]))) {
      instances <- c(instances, i)
    }
  }
  
  # A table is created with only the paired values
  data <- as.data.frame(experiments[instances, as.character(id_configurations), drop=FALSE])
  
  # If we can't find paired data
  if (length(instances) == 0) {
    stop("Error: The entered configurations do not have paired data\n")
  } 
  
  # Instances
  instances.ids <- instances
  instances.names <- rownames(data)
  data <- cbind(data, instances.names)
  best <- rep(NA, nrow(data))
  best[data[,1] >data[,2]] <- "conf2"
  best[data[,1] < data[,2]] <- "conf1"
  best[data[,1] == data[,2]] <- "equal"
  data <- cbind(data, best)
  
  # column names are changed
  colnames(data) <- c("conf1", "conf2", "instance","best")
 
  
  data <- data %>%
    mutate(point_text = paste0(instance,"\nx: ", conf1, "\n", "y: ", conf2, "\n"))
  
  # The plot scatter is created and assigned to p
  q <- ggplot(data, aes(x = conf1, y = conf2, color = best, text = point_text)) +
    geom_point(show.legend=FALSE) +
    scale_color_manual(values=c("#6600CC", "#00BFC4")) +
    theme(legend.position = 'none') +
    if (rpd == TRUE) {
      labs(color = " ", x = paste("Configuration", id_configurations[1], "RPD"), 
           y = paste("Configuration", id_configurations[2], "RPD"))
    } else {
      labs(color = " ", x = paste("Configuration", id_configurations[1], "Performance"), 
           y = paste("Configuration", id_configurations[2], "Performance"))
    }
  
  if (interactive) {
    q <- ggplotly(p=q, tooltip = "point_text")
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