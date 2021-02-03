#' Scatter Plot Testing
#'
#' @param iraceResults
#' The data generated when loading the Rdata file created by irace
#' @param idVector
#' It is a vector with 2 id values that you want to graph
#' @param distance_min
#' The data is treated referring to the minimum distance of each percentage row
#' @param fileName
#' It's of type string
#' You must place the path where you want to save the file and its name without the extension pdf
#'
#' @return scatter plot
#'
#' @importFrom ggplot2 scale_color_viridis_c
#'
#' @export
#'
#' @examples
#' NULL

iscatter_test <- function(iraceResults,idVector,distance_min = FALSE ,fileName = NULL){

  # verify that the data is correct
  if("FALSE" %in% (idVector %in% colnames(iraceResults$testing$experiments))){
    return("Some of the settings are not in testing")
  }else if(length(idVector) != 2){
    return("You must enter a vector with 2 values")
  }

  x <- y <- NULL
  # the table is created with all the data from testing experiments
  tabla <- as.data.frame(iraceResults$testing$experiments)

  # the table values are modified
  if(distance_min == TRUE){
    tabla <- 100*(tabla - apply(tabla,1,min))/apply(tabla,1,min)
  }

  # the table is created based on the entered values
  datos <- tabla[idVector]

  # column names are changed
  colnames(datos)[1] <- "x"
  colnames(datos)[2] <- "y"

  # the scatter graphics is created
  p <- ggplot(datos, aes(x=x,y=y, color=y)) +
      geom_point() +
      scale_color_viridis_c() +
      labs(color = "",x = paste("Configuration",idVector[1]), y = paste("Configuration",idVector[2]))

  #If the value in fileName is added the pdf file is created
  if(!is.null(fileName)){
    pdf(paste0(fileName,".pdf"))
    plot(p)
    dev.off()
    #If you do not add the value of fileName, the plot is displayed
  }else{
    p
  }


}
