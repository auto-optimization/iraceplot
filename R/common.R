calculate_rpd <- function(x)
{
  x <- as.matrix(x)
  x[is.infinite(x)] <- NA # Remove infinities
  min_cols <- matrixStats::rowMins(x, na.rm = TRUE)
  100 * abs((x - min_cols) / min_cols)
}

orca_pdf <- function(filename, plot)
{
  # The filename value is worked to separate it and assign it to new values.
  nameFile <- basename(filename)
  nameFile <- maybe_add_file_extension(nameFile, "pdf")
  directory <- paste0(dirname(filename), sep = "/")
  withr::with_dir(directory, plotly::orca(plot, nameFile))
}

orca_save_plot <- function(plot_list, filename)
{
  if (is.null(filename))
    return(invisible())
  
  directory <- paste0(dirname(filename), sep = "/")
  if (length(plot_list) == 1L) {
    plotly::orca(plot_list[[1L]], irace::path_rel2abs(filename))
  } else {
    base_name <- strsplit(basename(filename),split = '[.]')[[1L]][1L]
    ext <- strsplit(basename(filename),split = '[.]')[[1L]][2L]
    for (i in seq_along(plot_list)) {
      part <- paste0("-", i)
      cfile <- irace::path_rel2abs(paste0(directory, "/", base_name, part,"." , ext))
      plotly::orca(plot_list[[i]], cfile)
    }
  }
}

iraceplot_warn <- function(...)
  cli_alert_warning(text = paste0("{.strong Warning:} ", ...))


maybe_add_file_extension <- function(filename, ext)
{
  if (startsWith(ext, ".")) ext <- substring(ext, 2L)
  if (!has_file_extension(filename, ext)) filename <- paste0(filename, ".", ext)
  filename
}

has_file_extension <- function(filename, ext)
{
  if (startsWith(ext, ".")) ext <- substring(ext, 2L)
  grepl(paste0('[.]', ext, '$'), filename, ignore.case = TRUE)
}

check_unknown_param_names <- function(x, parameters_names)
{
  x <- unlist(x)
  if (any(x %not_in% parameters_names))
    stop("Unknown parameter names: ", paste0(setdiff(x, parameters_names), collapse=", "))
  x
}

subset_param_names <- function(x, parameters_names, is_fixed)
{
  if (is.null(x)) return(parameters_names[!is_fixed])
  check_unknown_param_names(x, parameters_names)
}


seq_ncol <- function(x) seq_len(ncol(x))
seq_nrow <- function(x) seq_len(nrow(x))
# Same as !(x %in% table). Package data.table has %notin%.
"%not_in%" <- function(x, table) is.na(match(x, table))
