#' Distance between configurations
#'
#' @description
#' Calculate the difference between two settings. Calculate the difference between two configurations. The greater
#' the number, the greater the difference
#'
#' @param iraceResults
#' The data generated when loading the Rdata file created by irace
#' @param idConfigurations
#' Numeric vector, you need to put the settings you
#' want to compare, only 2 values are allowed (example: idVector = c(806,809))
#' @param t
#' Numeric, It is a percentage factor that will determine the range of difference
#' between settings (example: t = 0.05 is equivalent to 5 percent)
#'
#' @importFrom dplyr select
#' @return numeric
#' @export
#'
#' @examples
#' NULL

distance_config <- function(iraceResults, idConfigurations, t = 0.05){

  #restrictions
  if(length(idConfigurations) != 2){
    return("You must enter two settings")
  }else if(FALSE %in% (idConfigurations[1] %in% iraceResults$allConfigurations[[".ID."]])){
    return(paste("Configuration",idConfigurations[1],"does not exist",sep = " "))
  }else if(FALSE %in% (idConfigurations[2] %in% iraceResults$allConfigurations[[".ID."]])){
    return(paste("Configuration",idConfigurations[2],"does not exist",sep = " "))
  }

  #variable assignment
  distance <- .ID. <- .PARENT. <-NULL
  datos <- select(iraceResults$allConfigurations[idConfigurations,],-.ID.,-.PARENT.)
  tipos <- iraceResults$parameters$types

  #the sum of the distance between parameters according to their type
  for (i in 1:length(datos)) {
    # c = category
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
    # r and i = numeric
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
  #the total difference between two configurations
  return(sum(distance))
}