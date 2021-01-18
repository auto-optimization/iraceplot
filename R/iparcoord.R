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

  #Variable assignment
  memo  <- configuration <- NULL
  idIteration <- unlist(idIteration)

  if(!is.null(idIteration)){

    # Verify that the entered id are within the possible range
    if(length(idIteration[idIteration < 1]) >= 1 || length(idIteration[idIteration > dim(iraceResults$allConfigurations)[1]]) >= 1){
      return("IDs entered are outside the range of settings")
    }

    # Verify that the id entered are more than 1 or less than the possible total
    if(length(idIteration) <= 1 || length(idIteration) > dim(iraceResults$allConfigurations)[1] ){
      return("You must enter more than one id")
    }

    # the table to be used and the filter with the iterations and configuration is created
    selection <- iraceResults$allConfigurations[, ".ID."] %in% idIteration
    tabla <- iraceResults$allConfigurations[selection,]
    filtro <- unique(iraceResults$experimentLog[,c("iteration","configuration")])
    selection2 <- filtro[, "configuration"] %in% idIteration
    filtro <- filtro[selection2,]
  }
  # table is created with all settings
  else{
    tabla <-iraceResults$allConfigurations
    filtro <- unique(iraceResults$experimentLog[,c("iteration","configuration")])
  }

  # The filter table is created and ordered according to the configurations
  filtro <- as.data.frame(filtro)
  filtro <- arrange(filtro,configuration)

  # An iteration table is created and added to the table
  iteration <- sample(NA,size=dim(tabla)[1],replace = TRUE)
  tabla <- cbind(tabla,iteration)

  # The NA of the first row of the table is replaced in the iteration column
  if(tabla$.ID.[1] == filtro$configuration[1] ){
    tabla$iteration[1] = filtro$iteration[1]
  }

  # memo is assigned the value of the filter table configuration
  memo = filtro$configuration[1]

  #The NAs of the table are replaced in the iteration column
  for(i in 2:dim(filtro)[1]){

    #if the same configuration has more than one iteration, a new row is created
    if(memo == filtro$configuration[i]){
      add <- tabla[tabla$.ID. == memo,]
      add$iteration = filtro$iteration[i]
      tabla <- rbind(tabla,add)
    }
    #The iteration is assigned to the configuration
    else{
      tabla$iteration[tabla$.ID. == filtro$configuration[i]] = filtro$iteration[i]
    }
    memo = filtro$configuration[i]
  }

  # Column .ID. and .PARENT. are removed
  tabla <- tabla[, !(names(tabla) %in% c(".ID.",".PARENT."))]

  # NA are converted to character in the table
  for(k in 1:(length(tabla))){
    tabla[[k]][is.na(tabla[[k]])] <- "NA"
  }

  # The graph is created
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

