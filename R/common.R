calculate_rpd <- function(x)
{
  min_cols <- matrixStats::rowMins(as.matrix(x), na.rm = TRUE)
  100 * (x - min_cols) / min_cols
}

orca_pdf <- function(filename, plot)
{
  # The filename value is worked to separate it and assign it to new values.
  nameFile <- basename(filename)
  if (!has_file_extension(nameFile, "pdf")) nameFile <- paste0(nameFile, ".pdf")
  directory <- paste0(dirname(filename), sep = "/")
  withr::with_dir(directory, orca(plot, nameFile))
}


orca_save_plot <- function(plot_list, filename)
{
  if (!is.null(filename)) {
    directory <- paste0(dirname(filename), sep = "/")
    if (length(plot_list) == 1) {
      orca(plot_list[[1]], path_rel2abs(filename))
    } else {
      base_name <- strsplit(basename(filename),split = '[.]')[[1]][1]
      ext <- strsplit(basename(filename),split = '[.]')[[1]][2]
      for (i in seq_along(plot_list)) {
        part <- paste0("-", i)
        cfile <- path_rel2abs(paste0(directory, "/", base_name, part,"." , ext))
        orca(plot_list[[i]], cfile)
      }
    }
  }
}

has_file_extension <- function(filename, extension)
{
  if (startsWith(extension, ".")) extension <- substring(extension, 2L)
  grepl(paste0('[.]', extension, '$'), filename, ignore.case = TRUE)
}

