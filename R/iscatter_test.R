#' Scatter Plot Testing
#'
#' @description
#' The function will return a scatter plot
#' comparing two elite configurations in testing
#'
#' @param iraceResults
#' The data generated when loading the Rdata file created by irace
#' @param idVector
#' String vector, you need to put the elite settings you
#' want to compare, only 2 values are allowed (example: idVector = c("92","119"))
#' @param rpd
#' logical(default FALSE) to fit through an equation of minimum percentage distance
#' between the values of each row of all configurations
#' @param fileName
#' String, A pdf will be created in the location and with the assigned
#' name (example: "~/patch/example/filename")
#' @return scatter plot
#'
#' @importFrom ggplot2 scale_color_viridis_c
#'
#' @export
#'
#' @examples
#' NULL

iscatter_test <- function(iraceResults,idVector,rpd = FALSE ,fileName = NULL){

  # verify that the data is correct

  if(length(idVector) != 2){
    return("You must enter a vector with 2 values")
  }else if(!(idVector[1] %in% colnames(iraceResults$testing$experiments))){
    return(paste("Configuration",idVector[1],"not found"))
  }else if(!(idVector[2] %in% colnames(iraceResults$testing$experiments))){
    return(paste("Configuration",idVector[2],"not found"))
  }

  x <- y <- NULL
  # the table is created with all the data from testing experiments
  tabla <- as.data.frame(iraceResults$testing$experiments)

  # the table values are modified
  if(rpd == TRUE){
    tabla <- 100*(tabla - apply(tabla,1,min))/apply(tabla,1,min)
  }

  # the table is created based on the entered values
  datos <- tabla[idVector]

  # column names are changed
  colnames(datos)[1] <- "x"
  colnames(datos)[2] <- "y"

  datos <- datos %>%
    mutate(text = paste0("x: ", x, "\n", "y: ", y, "\n"))

  # the scatter graphics is created
  q <- ggplot(datos, aes(x=x,y=y, color=y, text = text)) +
      geom_point() +
      scale_color_viridis_c() +
  if(rpd == TRUE){
    labs(color = "",x = paste("Configuration",idVector[1],"RPD"), y = paste("Configuration",idVector[2],"RPD"))
  }else{
    labs(color = "",x = paste("Configuration",idVector[1],"Performance"), y = paste("Configuration",idVector[2],"Performance"))
  }
  p <- plotly::ggplotly(q, tooltip="text")

  #If the value in fileName is added the pdf file is created
  if(!is.null(fileName)){
    pdf(paste0(fileName,".pdf"))
    plot(q)
    dev.off()
    #If you do not add the value of fileName, the plot is displayed
  }else{
    p
  }


}
