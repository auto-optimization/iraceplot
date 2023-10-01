#' Summarise by instance
#'
#' @template arg_irace_results
#'
#' @return tibble
#'
#' @examples
#' irace_result <- read_logfile(system.file(package="irace", "exdata",
#'                                          "irace-acotsp.Rdata", mustWork = TRUE))
#' summarise_by_instance(irace_result)
#' @export
summarise_by_instance <- function(irace_results)
{
  # FIXME: Handle non-atomic instances (just use instanceID)
  instances <- get_instanceID_seed_pairs(irace_results, index = 1:nrow(irace_results$experiments), instances=TRUE)[, "instance"]

  # FIXME: There must be a faster/easier way to do this.
  freq_count <- function(x) {
    x <- table(x)
    setNames(as.vector(x), names(x))
  }
  exp_by_instance <- freq_count(instances[irace_results$experimentLog[,"instance"]])
  seeds_by_instance <- freq_count(instances)

  ID <- value <- instance <- NULL # Silence warnings
  
  byinstance <- as_tibble(irace_results$experiments) %>%
    mutate(instance = instances, .before=1) %>%
    tidyr::pivot_longer(!c("instance"), names_to="ID") %>%
    group_by(instance) %>% tidyr::drop_na() %>%
    summarise(mean = mean(value), sd = sd(value), median = median(value),
              min = min(value), max = max(value),
              best_id = ID[which.min(value)]) %>%
    # Sort by the original order in instancesList
    arrange(factor(instance, levels = unique(instances))) %>%
    mutate(seeds = seeds_by_instance[as.character(instance)], .after="instance") %>%
    mutate(experiments = exp_by_instance[as.character(instance)], .after="instance")

  if (is.character(byinstance$instance)) {
    # FIXME: This should be smarter and try harder to detect if it is a path.
    basename_inst <- basename(byinstance$instance)
    if (length(basename_inst) == length(byinstance$instance)) {
      byinstance <- byinstance %>% mutate(instance = basename_inst)
    }
  }
  byinstance
}
