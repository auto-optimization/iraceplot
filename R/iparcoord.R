#' Parallel Coordinates Plot
#'
#' @param iraceResults
#' The data generated when loading the Rdata file created by irace
#'
#' @param idIteration
#'It is a vector with id values that you want to graph
#'
#' @param fileName
#'
#' It's of type string
#'You must place the path where you want to save the file and its name without the extension pdf
#'
#' @return plot
#' @export
#'
#'@importFrom dplyr arrange
#'@importFrom parcoords parcoords
#'
#' @examples
#' NULL

iparcoord <- function(iraceResults, idIteration = NULL,fileName = NULL){

  memo  <- configuration <- NULL

  tabla <-iraceResults$allConfigurations
  filtro <- unique(iraceResults$experimentLog[,c("iteration","configuration")])
  filtro <- as.data.frame(filtro)
  filtro <- arrange(filtro,configuration)
  iteration <- sample(NA,size=dim(tabla)[1],replace = TRUE)
  tabla <- cbind(tabla,iteration)


  if(tabla$.ID.[1] == filtro$configuration[1] ){
    tabla$iteration[1] = filtro$iteration[1]
  }

  for(i in 2:dim(filtro)[1]){
    memo = filtro$configuration[i-1]
    if(memo == filtro$configuration[i]){
      add <- tabla[memo,]
      add$iteration = filtro$iteration[i]
      tabla <- rbind(tabla,add)
    }else{
      tabla$iteration[filtro$configuration[i]] = filtro$iteration[i]
    }
  }
  tabla <- tabla[, !(names(tabla) %in% c(".ID.",".PARENT."))]
  p<-parcoords::parcoords(tabla[1:9],rownames = FALSE,reorder=TRUE,brushMode = "1D")
  p
}

