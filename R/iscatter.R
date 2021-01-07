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
#' @return plot
#' @export
#'
#' @examples
#' NULL
iscatter <- function(iraceResults, idVector){
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
  p
}
