#' Parameters Names
#'
#' @description
#' The function returns a string vector with the names of
#' the document parameters iraceResults
#'
#' @param iraceResults
#' The data generated when loading the Rdata file created by irace
#'
#' @return String vector
#' @export
#'
#' @examples
#' NULL

get_parameters_names <- function(iraceResults){
  return(iraceResults$parameters$names)
}
