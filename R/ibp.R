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

  Performance <- Elite_configuration <- NULL
  long <- length(iraceResults$allElites)

  if(!is.null(numberIteration)){
    if(numberIteration <= long){
      long <- numberIteration
    }else{
      return(print("iteration number out of range"))
    }
  }

  id <- iraceResults$allElites[[long]]
  matriz <- as.data.frame(iraceResults$experiments[,id])

  if(length(id) == 1){
    colnames(matriz)[colnames(matriz) == "iraceResults$experiments[, id]"] <- id
  }

  n_row_col = as.numeric(dim(matriz)[1]*dim(matriz)[2])
  tabla <- reshape(matriz,varying = c(as.character(id)), v.names = "Performance", timevar = "Elite_configuration",times = c(as.character(id)),new.row.names = 1:n_row_col,direction = "long")
  p <- ggplot(tabla, aes(x=Elite_configuration,y=Performance,color=Elite_configuration)) + geom_boxplot() + geom_jitter(shape=16, position=position_jitter(0.2)) + theme(legend.position="none") + labs(x="Elite Configurations")

  if(!is.null(fileName)){
    pdf(paste0(fileName,".pdf"),width = 6.79,height = 2.32)
    plot(p)
    dev.off()
  }else{
    p
  }
}
