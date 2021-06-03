#' Scatter Plot Testing
#'
#' @description
#' The function will return a scatter plot
#' comparing two elite configurations in testing
#'
#' @template arg_irace_results
#' @param id_configurations
#' String vector, you need to put the elite settings you
#' want to compare, only 2 values are allowed (example: idVector = c("92","119"))
#' @param rpd
#' logical(default TRUE) to fit through an equation of minimum percentage distance
#' between the values of each row of all configurations
#' @param file_name
#' String, A pdf will be created in the location and with the assigned
#' name (example: "~/patch/example/file_name")
#' @param interactive
#' Logical (Default interactive() ), Allows you to decide when generating the graph, i
#' t is generated interactively (It is created with the plotly package) which
#' is the default option or it is generated statically (It is created with the ggplot2
#' package). You must set interactive = FALSE.
#'
#' @return scatter plot
#'
#' @export
#'
#' @examples
#' scatter_test(iraceResults, id_configurations = c("92","119"), interactive = interactive())

scatter_test <- function(irace_results,id_configurations,rpd = TRUE ,file_name = NULL, interactive = interactive() ){

  # verify that test this in irace_results
  if(!("testing" %in% names(irace_results))){
    return("irace_results does not contain the testing data")
  }
  # verify that the data is correct
  id_configurations <- as.character(id_configurations)
  if(length(id_configurations) != 2){
    return("You must enter a vector with 2 values")
  }else if(!(id_configurations[1] %in% colnames(irace_results$testing$experiments))){
    return(paste("Configuration",id_configurations[1],"not found"))
  }else if(!(id_configurations[2] %in% colnames(irace_results$testing$experiments))){
    return(paste("Configuration",id_configurations[2],"not found"))
  }

  x <- y <- NULL
  # the table is created with all the data from testing experiments
  tabla <- as.data.frame(irace_results$testing$experiments)

  # the table values are modified
  if(rpd == TRUE){
    tabla <- 100*(tabla - apply(tabla,1,min))/apply(tabla,1,min)
  }

  # the table is created based on the entered values
  datos <- tabla[id_configurations]

  # column names are changed
  colnames(datos)[1] <- "x"
  colnames(datos)[2] <- "y"

  datos <- datos %>%
    mutate(text = paste0("x: ", x, "\n", "y: ", y, "\n"))

  # the scatter graphics is created
  p <- ggplot(datos, aes(x=x,y=y, color=y, text = text)) +
      geom_point() +
      scale_color_viridis_c() +
  if(rpd == TRUE){
    labs(color = "",x = paste("Configuration",id_configurations[1],"RPD"), y = paste("Configuration",id_configurations[2],"RPD"))
  }else{
    labs(color = "",x = paste("Configuration",id_configurations[1],"Performance"), y = paste("Configuration",id_configurations[2],"Performance"))
  }

  if(interactive == TRUE){
    p <- plotly::ggplotly(p, tooltip="text")
  }

  #If the value in file_name is added the pdf file is created
  if(!is.null(file_name)){
    ggsave(file_name,plot = p)
    return(p)
    #If you do not add the value of file_name, the plot is displayed
  }else{
    p
    return(p)
  }


}
