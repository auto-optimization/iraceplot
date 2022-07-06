#' Print parameter space in the textual format accepted by irace.
#' 
#' FIXME: Dependent parameter bounds are not supported yet. This function will move to the irace package in the next release.
#'
#' @param params (`list()`) Parameter object stored in `irace.Rdata` or read with `irace::readParameters()`.
#'
#' @param digits (`integer()`) The desired number of digits after the decimal point for real-valued parameters. Default is 15, but it should be the value in `scenario$digits`.
#' 
#' @examples
#'  library(irace)
#'  parameters.table <- '
#'  # name       switch           type  values               [conditions (using R syntax)]
#'  algorithm    "--"             c     (as,mmas,eas,ras,acs)
#'  localsearch  "--localsearch " c     (0, 1, 2, 3)
#'  ants         "--ants "        i,log (5, 100)
#'  q0           "--q0 "          r     (0.0, 1.0)           | algorithm == "acs"
#'  nnls         "--nnls "        i     (5, 50)              | localsearch %in% c(1,2,3)
#'  '
#' parameters <- readParameters(text=parameters.table)
#' printParameters(parameters)
#' @export
printParameters <- function(params, digits = 15L)
{
  names_len <- max(nchar(params$names))
  switches_len <- max(nchar(params$switches)) + 2
  for (name in params$names) {
    switch <- paste0('"', params$switches[[name]], '"')
    type <- params$types[[name]]
    transf <- params$transform[[name]]
    domain <- params$domain[[name]]
    if (type == "r") domain <- formatC(domain, digits=digits, format="f", drop0trailing=TRUE)
    domain <- paste0('(', paste0(domain, collapse=","), ')')
    condition <- params$conditions[[name]]
    condition <- if (isTRUE(condition)) "" else paste0(" | ", condition)
    if (!is.null(transf) && transf != "") type <- paste0(type, ",", transf)
    cat(sprintf('%*s %*s %s %-15s%s\n', -names_len, name, -switches_len, switch, type, domain, condition))
  }
}
