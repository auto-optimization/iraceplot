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

# Available in irace >= 3.6
get_instanceID_seed_pairs <- function(iraceResults, index, instances = FALSE)
{
  if (missing(iraceResults)) stop("argument 'iraceResults' is missing")
  iraceResults <- read_logfile(iraceResults)
  instancesList <- iraceResults$state$.irace$instancesList
  if (!missing(index))
    instancesList <- instancesList[index, ]
  if (!instances)
    return(instancesList)

  if (!is.atomic(iraceResults$scenario$instances)) {
    warning("instances=TRUE requested, but instances are not of atomic type")
    return(instancesList)
  }
  col <- if ("instance" %in% colnames(instancesList)) "instance" else "instanceID"
  instanceID <- instancesList[, col]
  cbind(instancesList, instance = iraceResults$scenario$instances[instanceID])
}
