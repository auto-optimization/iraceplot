#' Scatter Plot
#'
#'Create a graphic of scatter of irace using a vector with 2 values referring to the id
#'
#' @param iraceResults
#'
#'The data generated when loading the Rdata file created by irace
#'
#' @param idVector
#'
#'It is a vector with 2 id values that you want to graph
#'
#'@param fileName
#'
#'It's of type string
#'You must place the path where you want to save the file and its name without the extension pdf
#'
#' @return plot
#' @export
#'
#'@importFrom ggplot2 geom_point
#'
#' @examples
#' NULL

iscatter <- function(iraceResults, idVector, fileName = NULL){

  #Variable assignment
  iteracionFiltrada <- NULL

  #Verify that the entered id are within the possible range
  if(idVector[1] <= 0 || idVector[1] > dim(iraceResults$experiments)[2]){
    return(paste("id out of range",idVector[1]))
  }else if(idVector[2] <= 0 || idVector[2] > dim(iraceResults$experiments)[2]){
    return(paste("id out of range",idVector[2]))
  }
  #Verify that a vector of length 2 is entered
  if(length(idVector) == 1 || length(idVector) > 2){
    return("You must enter a vector with 2 values")
  }


  #An array of true and/or false to display if the field has data
  filtro <- !is.na(iraceResults$experiments[,idVector])

  #A vector is created only with paired iterations (both true)
  for(i in 1:dim(filtro)[1]){
    if(filtro[i,][1] && filtro[i,][2]){
      iteracionFiltrada <- c(iteracionFiltrada,i)
    }
  }

  #A table is created with only the paired values
  tabla <- as.data.frame((iraceResults$experiments[,idVector])[iteracionFiltrada,])

  #If you can't find paired data
  if(length(iteracionFiltrada) == 0){
    return("The entered configurations do not have paired data")
  #If it only finds a paired data, the table must be restructured
  }else if(length(iteracionFiltrada) == 1){
    conf1 <- tabla[1,]
    conf2 <- tabla[2,]
    tabla <- as.data.frame(conf1)
    tabla$conf2 <- conf2
  }

  #The names of the columns are changed otherwise they would be shown in the graph as they are numbers
  colnames(tabla)[colnames(tabla) == idVector[1]] <- "conf1"
  colnames(tabla)[colnames(tabla) == idVector[2]] <- "conf2"

  #The plot scatter is created and assigned to p
  p <- ggplot(tabla, aes(x=conf1, y=conf2, color=conf1)) +
    geom_point() +
    labs(color=" ",x = paste("Configuration",idVector[1]), y = paste("Configuration",idVector[2]))

  #If the value in fileName is added the pdf file is created
  if(!is.null(fileName)){
    pdf(paste0(fileName,".pdf"),width = 6.79,height = 2.32)
    plot(p)
    dev.off()
  #If you do not add the value of fileName, the plot is displayed
  }else{
    p
  }
}
