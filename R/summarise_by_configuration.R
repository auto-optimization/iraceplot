#' Summarise by configuration
#'
#' @template arg_irace_results
#' @param elites_only (`logical(1)`) If TRUE, only report the final elite configurations.
#' @param instances (`character(1)`) Select data from the training instances (`"train"`) or from the test instances if available (`"test"`). The default is from both (`"both"`).
#'
#' @return tibble
#'
#' @examples
#' irace_results <- read_logfile(system.file(package="irace", "exdata",
#'                                          "irace-acotsp.Rdata", mustWork = TRUE))
#' summarise_by_configuration(irace_results, instances = "train", elites_only = TRUE)
#' @export
summarise_by_configuration <- function(irace_results, elites_only = FALSE, instances = c("both", "train", "test"))
{
  instances <- match.arg(instances)
  test_ids <- ids <- exp_testing <- exp <- NULL
  best_elites <- as.character(irace_results$allElites[[length(irace_results$allElites)]])

  if (instances != "test") {
    ids <- if (elites_only) best_elites else colnames(irace_results$experiments)
    exp <- as_tibble(irace_results$experiments[, ids, drop=FALSE]) %>%
      tidyr::pivot_longer(dplyr::all_of(ids), names_to="ID") %>% tidyr::drop_na()
  }

  if (instances != "train") {
    if (has_testing_data(irace_results)) {
      test_ids <- if (elites_only) best_elites else colnames(irace_results$testing$experiments)
      exp_testing <- as_tibble(irace_results$testing$experiments[, test_ids, drop=FALSE]) %>%
        tidyr::pivot_longer(dplyr::all_of(test_ids), names_to="ID") %>% tidyr::drop_na()
    } else if (instances == "test") {
      cli_abort('{.code instances="test"} but {.arg irace_results} does not have testing data.')
    }
  }
  ids <- unique(c(ids, test_ids))
  ID <- value <- NULL # Silence warning
  exp <- dplyr::bind_rows(exp, exp_testing) %>% group_by(ID) %>%
    summarise(n_instances = dplyr::n(), mean = mean(value), sd = sd(value), median = median(value), min = min(value), max = max(value))
  exp[match(ids, exp$ID), , drop=FALSE]
}
