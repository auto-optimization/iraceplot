#' Scatter Plot Testing
#'
#' @description
#' The function will return a scatter plot
#' comparing two configurations in testing
#'
#' @template arg_irace_results
#' @param id_configurations
#' String vector, configuration ids whose performance should be displayed (example: id_configurations = c("92","119"))
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
#'
#' @export
#'
#' @examples
#' scatter_test(iraceResults, id_configurations = c("92", "119"), .interactive = interactive())
  scatter_test <- function(irace_results, id_configurations, rpd = TRUE, file_name = NULL, .interactive = interactive()) {

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
    tabla <- 100 * (tabla - apply(tabla, 1, min)) / apply(tabla, 1, min)
  }

  # the table is created based on the entered values
  datos <- as.data.frame(tabla[,id_configurations])
  datos <- cbind(datos, basename(irace_results$scenario$testInstances))
  datos <- cbind(datos, datos[,1]>datos[,2])

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
    scale_color_manual(values=c("#6600CC", "#00BFC4")) +
    geom_point(show.legend = FALSE) +
    theme(legend.position = 'none') +
    if (rpd) {
      labs(color = "", x = paste("Configuration", id_configurations[1], "RPD"), y = paste("Configuration", id_configurations[2], "RPD"))
    } else {
      labs(color = "", x = paste("Configuration", id_configurations[1], "Performance"), y = paste("Configuration", id_configurations[2], "Performance"))
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
