#'Box Plot
#'
#'Create a graphic of box plot of irace using the best quality id
#'By default the graph of the last iteration is shown
#'
#'@param iraceResults
#'The data generated when loading the Rdata file created by irace
#'
#'@param numberIteration
#'Numeric, It is the number referring to the iteration that you want to graph,
#'the range of these varies according to the Rdata used (example: numberIteration = 5)
#'
#'@param idConfigurations
#'Numeric vector, you need to put the configurations you want to analyze
#' (example: idConfigurations = c(20,50,100,300,500,600,700))
#'
#'@param rpd
#'Logical (default TRUE) to fit through an equation of minimum percentage distance
#'between the values of each row of all configurations
#'
#'@param fileName
#' String, A pdf will be created in the location and with the
#' assigned name (example: "~/patch/example/filename")
#'
#'@return box plot
#'
#'@importFrom stats reshape
#'@importFrom ggplot2 ggplot geom_boxplot geom_jitter position_jitter aes theme labs
#'@importFrom grDevices dev.off pdf
#'
#'@export
#'
#'@examples
#'boxplot_training(iraceResults)
#'boxplot_training(iraceResults,numberIteration = 5)
#'boxplot_training(iraceResults,idConfigurations = c(20,50,100,300,500,600,700))
#'

boxplot_training <- function(iraceResults, numberIteration = NULL, idConfigurations = NULL, rpd = TRUE ,fileName = NULL){

  #Variable assignment
  Performance <- Elite_configuration <- NULL
  long <- length(iraceResults$allElites)

  if(!is.null(numberIteration) & !is.null(idConfigurations)){
    return("You cannot use idConfigurations and numberIteration at the same time")
  }

  #It is checked if the fileName argument was added
  if(!is.null(numberIteration)){
    #We verify that numberIteration is within the range of values it can take
    if(numberIteration > 0 && numberIteration <= long){
      long <- numberIteration
    #If numberIteration is out of range it delivers a message per screen
    }else{
      return(print("iteration number out of range"))
    }
  }

  #A vector is created with the id of all elite configurations from the iteration entered
  id <- iraceResults$allElites[[long]]

  if(!is.null(idConfigurations)){
    n_conf = c(1:dim(iraceResults$experiments)[2])
    if(FALSE %in% (idConfigurations %in% n_conf)){
      return(paste("The following settings are out of range:",idConfigurations[!(idConfigurations %in% n_conf)]))
    }else{
      id <- idConfigurations
    }
  }

  #A table is created with the values of all elite configurations of the id of the requested iteration
  distance <- iraceResults$experiments

  if(rpd == TRUE){
    distance <- 100*(distance - apply(distance,1,min, na.rm = TRUE))/apply(distance,1,min, na.rm=TRUE)
  }

  matriz <- as.data.frame(distance[,id])

  #If the length of id is one, a different value must be added to the column
  if(length(id) == 1){
    colnames(matriz)[colnames(matriz) == "iraceResults$experiments[, id]"] <- id
  }

  #value of elements that the matrix contains
  n_row_col = as.numeric(dim(matriz)[1]*dim(matriz)[2])

  #A restructured table is created
  tabla <- reshape(matriz,varying = c(as.character(id)),
                   v.names = "Performance",
                   timevar = "Elite_configuration",
                   times = c(as.character(id)),
                   new.row.names = 1:n_row_col,
                   direction = "long")

  #The plot scatter is created and assigned to p
  p <- ggplot(tabla, aes(x=Elite_configuration,y=Performance,color=Elite_configuration)) +
    geom_boxplot(na.rm = TRUE) +
    geom_jitter(shape=16, position=position_jitter(0.2), na.rm = TRUE) +
    theme(legend.position="none") +
    labs(x="Elite Configurations")

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
