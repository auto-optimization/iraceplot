#' Scatter Plot Training
#'
#' @description
#' The function will return a scatter plot
#' comparing two configurations in the training performance data
#'
#' @template arg_irace_results
#'
#' @param id_configurations
#' Numeric vector,  configuration ids whose performance should be displayed (example: id_configurations = c("92","119"))
#'
#' @param rpd
#' logical(default TRUE) TRUE to plot performance as the relative percentage deviation to 
#' best results, FALSE to plot raw performance
#'
#' @param file_name
#' String, File name to save plot (example: "~/patch/example/file_name.png")
#'
#' @param .interactive
#' Logical (Default interactive()), TRUE if the plot is generated interactively (plotly package) which
#' is the default option, or FALSE it is generated statically.
#'
#' @return scatter plot
#' @export
#'
#' @examples
#' scatter_training(iraceResults, id_configurations = c(806, 809))
scatter_training <- function(irace_results, id_configurations, rpd = TRUE, file_name = NULL, .interactive = interactive()) {

  # Variable assignment
  iteracionFiltrada <- NULL

  # Verify that a vector of length 2 is entered
  if (length(id_configurations) != 2) {
    cat("Error: You must provide a vector with 2 values\n")
    stop()
  }
  # Verify that the entered id are within the possible range
  if (!(id_configurations[1] %in% irace_results$allConfigurations[,".ID."])) {
    cat(paste("Error: id out of range", id_configurations[1], "\n"))
    stop()
  } else if (!(id_configurations[2] %in% irace_results$allConfigurations[,".ID."])) {
    cat(paste("Error: id out of range", id_configurations[2], "\n"))
    stop()
  }

  distance <- irace_results$experiments

  if (rpd) {
    distance <- 100 * (distance - apply(distance, 1, min, na.rm = TRUE)) / apply(distance, 1, min, na.rm = TRUE)
  }

  # An array of true and/or false to display if the field has data
  filtro <- !is.na(distance[, id_configurations])

  # A vector is created only with paired iterations (both true)
  for (i in 1:nrow(filtro)) {
    if (all(!is.na(filtro[i, ]))) {
      iteracionFiltrada <- c(iteracionFiltrada, i)
    }
  }
  
  # A table is created with only the paired values
  tabla <- as.data.frame(distance[iteracionFiltrada, id_configurations, drop=FALSE])
  

  # If we can't find paired data
  if (length(iteracionFiltrada) == 0) {
    cat("Error: The entered configurations do not have paired data\n")
    stop()
  } 
  
  # instances
  instances.ids <- iraceResults$state$.irace$instancesList[iteracionFiltrada, "instance"]
  instances.names <- basename(iraceResults$scenario$instances[instances.ids])
  tabla <- cbind(tabla, instances.names)
  tabla <- cbind(tabla, tabla[,1]>tabla[,2])
  
  # column names are changed
  colnames(tabla)[1] <- "conf1"
  colnames(tabla)[2] <- "conf2"
  colnames(tabla)[3] <- "instance"
  colnames(tabla)[4] <- "best"
  
  tabla <- tabla %>%
    mutate(point_text = paste0(instance,"\nx: ", conf1, "\n", "y: ", conf2, "\n"))

  # The plot scatter is created and assigned to p
  q <- ggplot(tabla, aes(x = conf1, y = conf2, color = best, text = point_text)) +
    geom_point(show.legend=FALSE) +
    scale_color_manual(values=c("#6600CC", "#00BFC4")) +
    theme(legend.position = 'none') +
    if (rpd == TRUE) {
      labs(color = " ", x = paste("Configuration", id_configurations[1], "RPD"), y = paste("Configuration", id_configurations[2], "RPD"))
    } else {
      labs(color = " ", x = paste("Configuration", id_configurations[1], "Performance"), y = paste("Configuration", id_configurations[2], "Performance"))
    }

  if (.interactive) {
    q <- ggplotly(p=q, tooltip = "point_text")
    return(q)
  }
  
  # If the value in file_name is added the pdf file is created
  if (!is.null(file_name)) {
    ggsave(file_name, plot = q)
    return(q)
    # If you do not add the value of file_name, the plot is displayed
  } else {
    q
    return(q)
  }
}
