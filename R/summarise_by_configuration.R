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
  test_ids <- ids <- exp_test <- exp <- ranks <- ranks_test <- NULL
  best_elites <- as.character(irace_results$allElites[[length(irace_results$allElites)]])

  if (instances != "test") {
    experiments <- irace_results$experiments
    ids <- if (elites_only) best_elites else colnames(experiments)
    exp <- as_tibble(experiments[, ids, drop=FALSE]) %>%
      tidyr::pivot_longer(dplyr::all_of(ids), names_to="ID") %>% tidyr::drop_na()
    ranks <- as_tibble(matrixStats::rowRanks(experiments, ties.method = "average")[, ids, drop=FALSE]) %>%
      tidyr::pivot_longer(dplyr::all_of(ids), names_to="ID") %>% tidyr::drop_na()
  }

  if (instances != "train") {
    if (has_testing_data(irace_results)) {
      experiments <- irace_results$testing$experiments
      test_ids <- if (elites_only) best_elites else colnames(experiments)
      # Testing data should have no NAs.
      exp_test <- as_tibble(experiments[, test_ids, drop=FALSE]) %>%
        tidyr::pivot_longer(dplyr::all_of(test_ids), names_to="ID")
      ranks_test <- as_tibble(matrixStats::rowRanks(experiments, ties.method = "average")[, test_ids, drop=FALSE]) %>%
        tidyr::pivot_longer(dplyr::all_of(test_ids), names_to="ID")

    } else if (instances == "test") {
      cli_abort('{.code instances="test"} but {.arg irace_results} does not have testing data.')
    }
  }
  ids <- unique(c(ids, test_ids))
  ID <- value <- NULL # Silence warning
  exp <- dplyr::bind_rows(exp, exp_test) %>% group_by(ID) %>%
    summarise(n_instances = dplyr::n(), mean = mean(value), sd = sd(value), median = median(value), min = min(value), max = max(value))
  ranks <- dplyr::bind_rows(ranks, ranks_test) %>% group_by(ID) %>%
    summarise(rank_mean = mean(value), rank_sd = sd(value))
  exp <- dplyr::inner_join(exp, ranks, by = "ID")
  exp[match(ids, exp$ID), , drop=FALSE]
}
