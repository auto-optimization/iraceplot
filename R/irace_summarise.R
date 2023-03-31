#' Summarise the results of a run of irace
#' 
#' @template arg_irace_results
#'
#' @return `list()`
#' 
#' @examples
#' irace_results <- read_logfile(system.file(package="iraceplot", "exdata",
#'                                          "guide-example.Rdata", mustWork = TRUE))
#' irace_summarise(irace_results)
#' 
#' @author Manuel López-Ibáñez
#' @export
irace_summarise <- function(irace_results)
{
  if (missing(irace_results)) stop("argument 'irace_results' is missing")
  irace_results <- read_logfile(irace_results)

  niterations <- length(irace_results$allElites)

  time_cpu_user <- time_cpu_sys <- time_cpu_total <- time_wallclock <- NA
  if (!is.null(irace_results$state$elapsed)) {
    time_cpu_user <- irace_results$state$elapsed[["user"]]
    time_cpu_sys <- irace_results$state$elapsed[["system"]]
    time_cpu_total <- time_cpu_user + time_cpu_sys
    time_wallclock <- irace_results$state$elapsed[["wallclock"]]
  }
  
  list(
    version = irace_results$irace.version,
    n_iterations = niterations,
    n_configurations = nrow(irace_results$allConfigurations),
    n_instances = nrow(irace_results$experiments),
    n_experiments = nrow(irace_results$experimentLog),
    n_elites = length(irace_results$allElites[[niterations]]),
    n_soft_restarts = sum(irace_results$softRestart),
    n_rejected = length(irace_results$state$rejectedIDs),
    time_cpu_user = time_cpu_user,
    time_cpu_sys = time_cpu_sys,
    time_cpu_total = time_cpu_total,
    time_wallclock = time_wallclock,
    termination_reason = if (is.null(irace_results$state$completed)) "Missing" else irace_results$state$completed
  )
}
