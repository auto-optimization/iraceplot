#' Print parameter dependencies as a tree
#' 
#' @template arg_parameters
#'
#' @examples
#' # Read the parameters directly from text.
#' parameters_tab <- '
#' a "" i (2, 10)
#' b "" c (yes, no) | a < 5
#' c "" o (low, medium, high) | (a == 2) | (b == "yes")
#' d "" r (a, 50)
#' '
#' parameters <- irace::readParameters(text=parameters_tab)
#' parameters_tree(parameters)
#' 
#' @author Manuel López-Ibáñez
#' @export
parameters_tree <- function(parameters)
{
  # FIXME: Print the conditions also.
  depends <- parameters$depends
  params <- names(depends)
  
  revtree <- lapply(params, function(y)
    params[sapply(depends, function(x, elem) elem %in% x, elem = y)])
  revtree <- c(list(params[sapply(depends, length) == 0L]), revtree)
  revdep <- data.frame(stringsAsFactors = FALSE,
                       parameter = c(" ", params),
                       dependencies = I(revtree))
  cli::cat_print(cli::tree(revdep, root = " "))
}
