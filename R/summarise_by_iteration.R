#' Summarise by iteration
#'
#' @template arg_irace_results
#'
#' @return tibble
#'
#' @examples
#' irace_result <- read_logfile(system.file(package="irace", "exdata",
#'                                          "irace-acotsp.Rdata", mustWork = TRUE))
#' summarise_by_iteration(irace_result)
#' @export
summarise_by_iteration <- function(irace_results)
{
  iteration <- configuration <- instance <- NULL # Silence warnings
  as_tibble(irace_results$experimentLog) %>%
    group_by(iteration) %>%
    summarise(configurations = n_distinct(configuration),
              instances = n_distinct(instance), experiments=dplyr::n()) %>%
    mutate(elites = sapply(irace_results$allElites, length),
           best_id = irace_results$iterationElites)
}
