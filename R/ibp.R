#' Graphic box plot
#'
#'Create a graphic of box plot from the latest iteration
#'of irace using the best quality id
#'
#' @param iraceResults
#'
#' The data generated when loading the Rdata file created by irace
#'
#' @param fileName
#'
#'It is of type string
#'You must place the path where you want to save the file and its name, it is not necessary to place the extension of the pdf
#'If you do not put the route it will be saved in the current location where you are working
#'If you do not want to save the file and the graph is displayed on the screen, just put NULL in the argument
#'
#' @return plot
#'
#' @importFrom stats reshape
#' @importFrom ggplot2 ggplot geom_boxplot geom_jitter position_jitter aes theme
#' @importFrom grDevices dev.off pdf
#'
#' @export
#'
#' @examples
#' NULL

ibp <- function(iraceResults, fileName){
  Performance <- Elite_configuration <- NULL
  long <- length(iraceResults$allElites)
  id <- iraceResults$allElites[[long]]
  matriz <- as.data.frame(iraceResults$experiments[,id])
  n_row_col = as.numeric(dim(matriz)[1]*dim(matriz)[2])
  tabla <- reshape(matriz,varying = c(as.character(id)), v.names = "Performance", timevar = "Elite_configuration",times = c(as.character(id)),new.row.names = 1:n_row_col,direction = "long")
  p <- ggplot(tabla, aes(x=Elite_configuration,y=Performance,color=Elite_configuration)) + geom_boxplot() + geom_jitter(shape=16, position=position_jitter(0.2)) + theme(legend.position="none")
  if(!is.null(fileName)){
    pdf(paste0(fileName,".pdf"),width = 6.79,height = 2.32)
    plot(p)
    dev.off()
  }else{
    p
  }
}
