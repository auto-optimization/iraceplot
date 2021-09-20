#' Scatter Plot Testing
#'
#' @description
#' The function creates a scatter plot comparing the performance of two 
#' configurations in the test instances. Each point in the plot represents an instance
#' and the color of the points indicates if one configuration is better than the other.
#' 
#' The performance data is obtained from the test evaluations performed by irace. 
#' Note that the testing is not a default feature in irace and should be enabled in 
#' the setup. 
#' 
#' Configuration ids provided in `id_configurations` should belong to 
#' elite configuration set evaluated in the test (see the irace package user 
#' guide for more details).
#'
#' @template arg_irace_results
#' @param id_configurations
#' String vector, configuration ids whose performance should be displayed 
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
#' @export
#'
#' @examples
#' scatter_test(iraceResults, id_configurations = c("92", "119"))
#' scatter_test(iraceResults, id_configurations = c("92", "119"), rpd=FALSE)
  scatter_test <- function(irace_results, id_configurations, rpd = TRUE, filename = NULL, interactive = base::interactive()) {

  conf1 <- conf2 <- best <- instance <- x_val <- y_val <- point_text <- NULL
    
  # verify that test this in irace_results
  if (!("testing" %in% names(irace_results))) {
    cat("Error: irace_results does not contain the testing data\n")
    stop()
  }
  # verify that the data is correct
  id_configurations <- as.character(id_configurations)
  if (length(id_configurations) != 2) {
    cat("Error: You must enter a vector with 2 values \n")
    stop()
  } else if (!(id_configurations[1] %in% colnames(irace_results$testing$experiments))) {
    cat(paste("Error: Configuration", id_configurations[1], "not found\n"))
    stop()
  } else if (!(id_configurations[2] %in% colnames(irace_results$testing$experiments))) {
    cat(paste("Error: Configuration", id_configurations[2], "not found\n"))
    stop()
  }

  # the table is created with all the data from testing experiments
  tabla <- irace_results$testing$experiments

  # the table values are modified
  if (rpd) {
    tabla <- calculate_rpd(tabla)
  }

  # the table is created based on the entered values
  datos <- as.data.frame(tabla[,id_configurations])
  datos <- cbind(datos, basename(irace_results$scenario$testInstances))
  best <- rep(NA, nrow(datos))
  best[datos[,1]>datos[,2]] <- "conf2"
  best[datos[,1]<datos[,2]] <- "conf1"
  best[datos[,1]==datos[,2]] <- "equal"
  datos <- cbind(datos, best)

  # column names are changed
  colnames(datos)[1] <- "x_val"
  colnames(datos)[2] <- "y_val"
  colnames(datos)[3] <- "instance"
  colnames(datos)[4] <- "best"
  

  datos <- datos %>%
    mutate(point_text = paste0(instance,"\nx: ", x_val, "\n", "y: ", y_val, "\n"))

  # the scatter graphics is created
  q <- ggplot(datos, aes(x = x_val, y = y_val, color = best, text = point_text)) +
    #viridis::scale_color_viridis(discrete=TRUE) +
    scale_color_manual(values=c("#6600CC", "#00BFC4", "#FF9900")) +
    geom_point(show.legend = FALSE) +
    theme(legend.position = 'none') +
    if (rpd) {
      labs(color = "", x = paste("Configuration", id_configurations[1], "RPD"), y = paste("Configuration", id_configurations[2], "RPD"))
    } else {
      labs(color = "", x = paste("Configuration", id_configurations[1], "Performance"), y = paste("Configuration", id_configurations[2], "Performance"))
    }

  if (interactive) {
    q <- ggplotly(p=q, tooltip = "point_text")
    return(q)
  }

  # If the value in filename is added the pdf file is created
  if (!is.null(filename)) {
    ggsave(filename, plot = q)
    return(q)
    # If you do not add the value of filename, the plot is displayed
  } else {
    q
    return(q)
  }
}
