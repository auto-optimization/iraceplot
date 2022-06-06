#' Print parameter space in the textual format accepted by irace.
#'
#' @param params (`list()`) Parameter object stored in `irace.Rdata` or read with `irace::readParameters()`.
#'
#' @param digits (`integer()`) The desired number of digits after the decimal point for real-valued parameters. Default is 15, but it should be the value in `scenario$digits`.
#' 
#' @examples
#' load(system.file(package="iraceplot", "exdata", "guide-example.Rdata", mustWork = TRUE))
#' printParameters(iraceResults$parameters)
#' @export
printParameters <- function(params, digits = 15L)
{
  names_len <- max(nchar(params$names))
  switches_len <- max(nchar(params$switches)) + 2
  for (name in params$names) {
    switch <- paste0('"', params$switches[[name]], '"')
    type <- params$types[[name]]
    domain <- params$domain[[name]]
    # FIXME: We should use digits
    if (type == "r") domain <- formatC(domain, digits=digits, format="f", drop0trailing=TRUE)
    domain <- paste0('(', paste0(domain, collapse=","), ')')
    condition <- params$conditions[[name]]
    condition <- if (isTRUE(condition)) "" else paste0(" | ", condition)
    cat(sprintf('%*s %*s %s %-15s%s\n', -names_len, name, -switches_len, switch, type, domain, condition))
  }
}
