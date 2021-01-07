#'Graphic box plot
#'
#'Create a graphic of box plot of irace using the best quality id
#'By default the graph of the last iteration is shown
#'
#'@param iraceResults
#'
#'The data generated when loading the Rdata file created by irace
#'
#'@param numberIteration
#'
#'It's of type numeric
#'Allows to choose which iteration of the elite configuration of irace to graph
#'
#'@param fileName
#'
#'It's of type string
#'You must place the path where you want to save the file and its name without the extension pdf
#'
#'@return plot
#'
#'@importFrom stats reshape
#'@importFrom ggplot2 ggplot geom_boxplot geom_jitter position_jitter aes theme labs
#'@importFrom grDevices dev.off pdf
#'
#'@export
#'
#'@examples
#'NULL

ibp <- function(iraceResults, numberIteration = NULL,fileName = NULL){

  #Variable assignment
  Performance <- Elite_configuration <- NULL
  long <- length(iraceResults$allElites)

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

  #A table is created with the values of all elite configurations of the id of the requested iteration
  matriz <- as.data.frame(iraceResults$experiments[,id])

  #If the length of id is one, a different value must be added to the column
  if(length(id) == 1){
    colnames(matriz)[colnames(matriz) == "iraceResults$experiments[, id]"] <- id
  }

  #value of elements that the matrix contains
  n_row_col = as.numeric(dim(matriz)[1]*dim(matriz)[2])

  #A restructured table is created
  tabla <- reshape(matriz,varying = c(as.character(id)), v.names = "Performance", timevar = "Elite_configuration",times = c(as.character(id)),new.row.names = 1:n_row_col,direction = "long")

  #The plot scatter is created and assigned to p
  p <- ggplot(tabla, aes(x=Elite_configuration,y=Performance,color=Elite_configuration)) + geom_boxplot() + geom_jitter(shape=16, position=position_jitter(0.2)) + theme(legend.position="none") + labs(x="Elite Configurations")

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
