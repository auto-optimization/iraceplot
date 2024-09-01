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
  instances <- get_instanceID_seed_pairs(irace_results, index = seq_nrow(irace_results$experiments), instances=TRUE)
  # For non-atomic instances just use instanceID.
  instances <- if (is.character(instances[["instance"]])) instances[["instance"]] else instances[["instanceID"]]
  
  # FIXME: There must be a faster/easier way to do this.
  freq_count <- function(x) {
    x <- table(x)
    setNames(as.vector(x), names(x))
  }
  exp_by_instance <- freq_count(instances[irace_results$state$experiment_log[["instance"]]])
  seeds_by_instance <- freq_count(instances)

  value <- instance <- NULL # Silence warnings
  
  byinstance <- as_tibble(irace_results$experiments) %>%
    mutate(instance = instances, .before=1) %>%
    tidyr::pivot_longer(!c("instance"), names_to="ID") %>%
    group_by(instance) %>% tidyr::drop_na() %>%
    summarise(mean = mean(value), sd = sd(value), median = median(value),
              min = min(value), max = max(value),
              best_id = .data$ID[which.min(value)]) %>%
    # Sort by the original order in instancesList
    arrange(factor(instance, levels = unique(instances))) %>%
    mutate(seeds = seeds_by_instance[as.character(instance)], .after="instance") %>%
    mutate(experiments = exp_by_instance[as.character(instance)], .after="instance")

  if (is.character(byinstance$instance)) {
    rel_inst <- as.character(path_rel(byinstance$instance, start = path_common(byinstance$instance)))
    # We want the instances to be unique still, otherwise don't touch them.
    if (length(unique(rel_inst)) == length(byinstance$instance)) {
      byinstance <- byinstance %>% mutate(instance = rel_inst)
    }
  }
  byinstance
}
