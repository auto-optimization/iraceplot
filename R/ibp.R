#' Graphic box plot
#'
#'Create a graphic of box plot from the latest iteration
#'of irace using the best quality id
#'
#' @param iraceResults
#'
#' The data generated when loading the Rdata file created by irace
#'
#' @return plot
#'
#' @importFrom stats reshape
#' @importFrom ggplot2 ggplot geom_boxplot geom_jitter position_jitter aes theme
#'
#' @export
#'
#' @examples
#' NULL

ibp <- function(iraceResults){
  Performance <- Elite_configuration <- NULL
  long <- length(iraceResults$allElites)
  id <- iraceResults$allElites[[long]]
  matriz <- as.data.frame(iraceResults$experiments[,id])
  n_row_col = as.numeric(dim(matriz)[1]*dim(matriz)[2])
  tabla <- reshape(matriz,varying = c(as.character(id)), v.names = "Performance", timevar = "Elite_configuration",times = c(as.character(id)),new.row.names = 1:n_row_col,direction = "long")
  p <- ggplot(tabla, aes(x=Elite_configuration,y=Performance,color=Elite_configuration)) + geom_boxplot() + geom_jitter(shape=16, position=position_jitter(0.2)) + theme(legend.position="none")
  p
}
