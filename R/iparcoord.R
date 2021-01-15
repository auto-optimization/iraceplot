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
  idIteration <- unlist(idIteration)

  if(!is.null(idIteration)){
    selection <- iraceResults$allConfigurations[, ".ID."] %in% idIteration
    tabla <- iraceResults$allConfigurations[selection,]
    filtro <- unique(iraceResults$experimentLog[,c("iteration","configuration")])
    selection2 <- filtro[, "configuration"] %in% idIteration
    filtro <- filtro[selection2,]
  }else{
    tabla <-iraceResults$allConfigurations
    filtro <- unique(iraceResults$experimentLog[,c("iteration","configuration")])
  }

  filtro <- as.data.frame(filtro)
  filtro <- arrange(filtro,configuration)
  iteration <- sample(NA,size=dim(tabla)[1],replace = TRUE)
  tabla <- cbind(tabla,iteration)


  if(tabla$.ID.[1] == filtro$configuration[1] ){
    tabla$iteration[1] = filtro$iteration[1]
  }
  memo = filtro$configuration[1]
  for(i in 2:dim(filtro)[1]){

    if(memo == filtro$configuration[i]){
      add <- tabla[tabla$.ID. == memo,]
      add$iteration = filtro$iteration[i]
      tabla <- rbind(tabla,add)
    }else{
      tabla$iteration[tabla$.ID. == filtro$configuration[i]] = filtro$iteration[i]
    }
    memo = filtro$configuration[i]
  }

  tabla <- tabla[, !(names(tabla) %in% c(".ID.",".PARENT."))]

  for(k in 1:length(tabla)){
    tabla[[k]][is.na(tabla[[k]])] <- "NA"
  }
  p<-parcoords::parcoords(tabla,
    rownames = FALSE,
    reorder=TRUE,
    brushMode = "1D",
    color = list(
    colorBy = "iteration",
    colorScale = "scaleOrdinal",
    colorScheme = "schemeCategory10"),
    withD3 = TRUE
    )

  #If the value in fileName is added the pdf file is created
  if(!is.null(fileName)){
    #The fileName value is worked to separate it and assign it to new values.
    nameFile = basename(fileName)
    directory = paste0(dirname(fileName),sep="/")
    withr::with_dir(directory, orca(p, paste0(nameFile,".pdf")))

    #If you do not add the value of fileName, the plot is displayed
  }else{
    p
  }
}

