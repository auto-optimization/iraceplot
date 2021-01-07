#' Graphic Scatter
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
#' @examples
#' NULL
iscatter <- function(iraceResults, idVector, fileName = NULL){

  iteracionFiltrada <- NULL
  filtro <- !is.na(iraceResults$experiments[,idVector])

  for(i in 1:dim(filtro)[1]){
    if(filtro[i,][1] && filtro[i,][2]){
      iteracionFiltrada <- c(iteracionFiltrada,i)
    }
  }

  tabla <- as.data.frame((iraceResults$experiments[,idVector])[iteracionFiltrada,])

  if(length(iteracionFiltrada) == 0){
    return("The entered configurations do not have paired data")
  }else if(length(iteracionFiltrada) == 1){
    conf1 <- tabla[1,]
    conf2 <- tabla[2,]
    tabla <- as.data.frame(conf1)
    tabla$conf2 <- conf2
  }

  colnames(tabla)[colnames(tabla) == idVector[1]] <- "conf1"
  colnames(tabla)[colnames(tabla) == idVector[2]] <- "conf2"
  p <- ggplot(tabla, aes(x=conf1, y=conf2, color=conf1)) + geom_point() + labs(color=" ",x = paste("Configuration",idVector[1]), y = paste("Configuration",idVector[2]))

  if(!is.null(fileName)){
    pdf(paste0(fileName,".pdf"),width = 6.79,height = 2.32)
    plot(p)
    dev.off()
  }else{
    p
  }
}
