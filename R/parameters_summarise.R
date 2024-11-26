#' Summarise parameters space
#' 
#' @template arg_parameters
#'
#' @return [tibble::tibble()]
#' 
#' @examples
#' # Read the parameters directly from text.
#' parameters_tab <- '
#' a "" i (2, 10)
#' b "" c (yes, no) | a < 5
#' c "" o (low, medium, high) | (a == 2) | (b == "yes")
#' d "" r (a, 50)'
#' parameters <- irace::readParameters(text=parameters_tab)
#' parameters_summarise(parameters)
#' 
#' @author Manuel López-Ibáñez
#' @export
parameters_summarise <- function(parameters)
  tibble(n_total = parameters$nbParameters,
         n_fixed = parameters$nbFixed,
         n_int = sum(parameters$types == "i"),
         n_real = sum(parameters$types == "r"),
         n_cat = sum(parameters$types == "c"),
         n_ord = sum(parameters$types == "o"),
         n_conditional = sum(!sapply(parameters$conditions, isTRUE)),
         n_dependent = sum(sapply(parameters$get(), getElement, name="is_dependent")),
         max_level = max(parameters$hierarchy))

