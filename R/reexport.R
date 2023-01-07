#' @importFrom irace read_logfile
#' @export
irace::read_logfile

#' Read the log file (`log-ablation.Rdata`) produced by [irace::ablation()].
#'
#' @param filename Filename that contains the log file saved by [irace::ablation()]. Example: `log-ablation.Rdata`.
#' 
#' @return (`list()`)
#' @export
read_ablogfile <- function(filename) irace::read_logfile(filename, name = "ablog")

