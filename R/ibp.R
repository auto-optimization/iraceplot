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
#' @importFrom ggplot2 ggplot geom_boxplot geom_jitter position_jitter aes
#'
#' @export
#'
#' @examples
#' NULL

ibp <- function(iraceResults){
  ID <- Datos <- NULL
  long <- length(iraceResults$allElites)
  id <- iraceResults$allElites[[long]]
  matriz <- as.data.frame(iraceResults$experiments[,id])
  n_row_col = as.numeric(dim(matriz)[1]*dim(matriz)[2])
  tabla <- reshape(matriz,varying = c(as.character(id)), v.names = "Datos", timevar = "ID",times = c(as.character(id)),new.row.names = 1:n_row_col,direction = "long")
  p <- ggplot(tabla, aes(x=ID,y=Datos,color=ID)) + geom_boxplot() + geom_jitter(shape=16, position=position_jitter(0.2))
  p
}
