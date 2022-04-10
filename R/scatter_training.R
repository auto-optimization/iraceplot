#' Scatter Plot Training
#'
#' @description
#' The `scatter_training` function creates a scatter plot that displays the performance of two
#' configurations on the training performance. Each point in the plot represents an 
#' instance and the color of the points indicates if one configuration is better 
#' than the other.
#' 
#' The performance data is obtained from the evaluations performed by irace 
#' during the execution process, consequently the number of evaluations 
#' can differ between configurations due to the elimination process applied by 
#' irace. This plot only shows performance data only for instances in which both
#' configurations are executed.
#'
#' @template arg_irace_results
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
#' scatter_training(iraceResults, id_configurations = c(806, 809))
#' scatter_training(iraceResults, id_configurations = c(806, 809), rpd = FALSE)
#' @export
#' 
scatter_training <- function(irace_results, id_configurations, rpd = TRUE, 
                             filename = NULL, interactive = base::interactive()) {
  
  # Verify that a vector of length 2 is entered
  if (length(id_configurations) != 2) {
    stop("Error: You must provide a vector with 2 values\n")
  }
  # Verify that the entered id are within the possible range
  if (!(id_configurations[1] %in% irace_results$allConfigurations[,".ID."])) {
    stop(paste0("Error: id out of range", id_configurations[1], "\n"))
  } else if (!(id_configurations[2] %in% irace_results$allConfigurations[,".ID."])) {
    stop(paste("Error: id out of range", id_configurations[2], "\n"))
  }
  
  experiments <- irace_results$experiments
  
  instances.ids <- irace_results$state$.irace$instancesList[1:nrow(experiments), "instance"]
  instances.names <- basename(irace_results$scenario$instances[instances.ids])
  rownames(experiments) <- instances.names
  
  scatter_performance(experiments,
                      id_configurations=id_configurations,
                      rpd=rpd,
                      filename=filename, 
                      interactive = interactive)

}


# scatter_training <- function(irace_results, id_configurations, rpd = TRUE, filename = NULL, interactive = base::interactive()) {
# 
#   # Variable assignment
#   iteracionFiltrada <- conf1 <- conf2<- instance <- best <- point_text <- NULL
# 
#   # Verify that a vector of length 2 is entered
#   if (length(id_configurations) != 2) {
#     stop("Error: You must provide a vector with 2 values\n")
#   }
#   # Verify that the entered id are within the possible range
#   if (!(id_configurations[1] %in% irace_results$allConfigurations[,".ID."])) {
#     stop(paste0("Error: id out of range", id_configurations[1], "\n"))
#   } else if (!(id_configurations[2] %in% irace_results$allConfigurations[,".ID."])) {
#     stop(paste("Error: id out of range", id_configurations[2], "\n"))
#   }
# 
#   distance <- irace_results$experiments
# 
#   if (rpd) {
#     distance <- calculate_rpd(distance)
#   }
# 
#   # An array of true and/or false to display if the field has data
#   filtro <- !is.na(distance[, id_configurations])
# 
#   # A vector is created only with paired iterations (both true)
#   for (i in 1:nrow(filtro)) {
#     if (all(!is.na(filtro[i, ]))) {
#       iteracionFiltrada <- c(iteracionFiltrada, i)
#     }
#   }
#   
#   # A table is created with only the paired values
#   tabla <- as.data.frame(distance[iteracionFiltrada, id_configurations, drop=FALSE])
#   
# 
#   # If we can't find paired data
#   if (length(iteracionFiltrada) == 0) {
#     cat("Error: The entered configurations do not have paired data\n")
#     stop()
#   } 
#   
#   # instances
#   instances.ids <- irace_results$state$.irace$instancesList[iteracionFiltrada, "instance"]
#   instances.names <- basename(irace_results$scenario$instances[instances.ids])
#   tabla <- cbind(tabla, instances.names)
#   best <- rep(NA, nrow(tabla))
#   best[tabla[,1]>tabla[,2]] <- "conf2"
#   best[tabla[,1]<tabla[,2]] <- "conf1"
#   best[tabla[,1]==tabla[,2]] <- "equal"
#   tabla <- cbind(tabla, best)
#   
#   # column names are changed
#   colnames(tabla)[1] <- "conf1"
#   colnames(tabla)[2] <- "conf2"
#   colnames(tabla)[3] <- "instance"
#   colnames(tabla)[4] <- "best"
#   
#   tabla <- tabla %>%
#     mutate(point_text = paste0(instance,"\nx: ", conf1, "\n", "y: ", conf2, "\n"))
# 
#   # The plot scatter is created and assigned to p
#   q <- ggplot(tabla, aes(x = conf1, y = conf2, color = best, text = point_text)) +
#     geom_point(show.legend=FALSE) +
#     scale_color_manual(values=c("#6600CC", "#00BFC4")) +
#     theme(legend.position = 'none') +
#     if (rpd == TRUE) {
#       labs(color = " ", x = paste("Configuration", id_configurations[1], "RPD"), y = paste("Configuration", id_configurations[2], "RPD"))
#     } else {
#       labs(color = " ", x = paste("Configuration", id_configurations[1], "Performance"), y = paste("Configuration", id_configurations[2], "Performance"))
#     }
# 
#   if (interactive) {
#     q <- ggplotly(p=q, tooltip = "point_text")
#     return(q)
#   }
#   
#   # If the value in filename is added the pdf file is created
#   if (!is.null(filename)) {
#     ggsave(filename, plot = q)
#     # If you do not add the value of filename, the plot is displayed
#   } else {
#     q
#   }
#   return(q)
# }
