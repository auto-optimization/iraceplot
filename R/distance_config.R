#' Distance between configurations
#'
#' @param iraceResults
#' The data generated when loading the Rdata file created by irace
#' @param idConfigurations
#' Numeric vector, you need to put the settings you
#' want to compare, only 2 values are allowed (example: idVector = c(806,809))
#' @param t
#' Numeric, value to determine the range of difference between settings
#'
#' @importFrom dplyr select
#' @return numeric
#' @export
#'
#' @examples
#' NULL

distance_config <- function(iraceResults, idConfigurations, t = 0.05){

  if(length(idConfigurations) != 2){
    return("You must enter two settings")
  }else if(FALSE %in% (idConfigurations[1] %in% iraceResults$allConfigurations[[".ID."]])){
    return(paste("Configuration",idConfigurations[1],"does not exist",sep = " "))
  }else if(FALSE %in% (idConfigurations[2] %in% iraceResults$allConfigurations[[".ID."]])){
    return(paste("Configuration",idConfigurations[2],"does not exist",sep = " "))
  }

  distance <- .ID. <- .PARENT. <-NULL
  datos <- select(iraceResults$allConfigurations[idConfigurations,],-.ID.,-.PARENT.)
  tipos <- iraceResults$parameters$types

  for (i in 1:length(datos)) {
    if(tipos[[colnames(datos)[i]]] == "c"){
      if(is.na(datos[[i]][1]) && is.na(datos[[i]][2])){
        distance <- c(distance,0)
      }else if(is.na(datos[[i]][1]) || is.na(datos[[i]][2])){
        distance <- c(distance,1)
      }else if(datos[[i]][1] == datos[[i]][2]){
        distance <- c(distance,0)
      }else{
        distance <- c(distance,1)
      }
    }else if(tipos[[colnames(datos)[i]]] == "r" || tipos[[colnames(datos)[i]]] == "i"){

      if(is.na(datos[[i]][1]) && is.na(datos[[i]][2])){
        distance <- c(distance,0)
      }else if( is.na(datos[[i]][1]) || is.na(datos[[i]][2])){
        distance <- c(distance,1)
      }else if(datos[[i]][1] == datos[[i]][2]){
        distance <- c(distance,0)
      }else{
        min = iraceResults$parameters$domain[[colnames(datos)[i]]][1]
        max = iraceResults$parameters$domain[[colnames(datos)[i]]][2]
        dt = (max - min)*t
        if(abs(datos[[i]][1]-datos[[i]][2]) <= dt){
          distance <- c(distance,0)
        }else{
          distance <- c(distance,1)
        }
      }
    }
  }
  return(sum(distance))
}
