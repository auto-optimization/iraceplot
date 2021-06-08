#' Parameters Names
#'
#' @description
#' The function returns a string vector with the names of
#' the document parameters irace_results
#'
#' @template arg_irace_results
#'
#' @return String vector
#' @export
#'
#' @examples
#' get_parameters_names(iraceResults)
get_parameters_names <- function(irace_results) {
  return(irace_results$parameters$names)
}
