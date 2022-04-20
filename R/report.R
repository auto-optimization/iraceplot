#' Create HTML Report from irace data
#' 
#' This function creates an HTML report of the most relevant irace data.  This
#' report provides general statistics and plots that show the best
#' configurations and their performance. Example: <https://auto-optimization.github.io/iraceplot/articles/example/report_example.html>
#'
#' @template arg_irace_results
#'
#' @param filename (`character(1)`)
#' Filename indicating where to save the report (example: `"~/path-to/filename"`)
#' @template arg_interactive
#'
#' @examples
#' NULL
#' @export
#' @md
report <- function(irace_results, filename, interactive = base::interactive())
{
  if (missing(irace_results)) stop("You must provide irace_results")
  if (missing(filename)) stop("You must provide a filename")
 
  if (!has_file_extension(filename, "html"))
    filename <- paste0(filename, ".html")
  final_file <- path_rel2abs(filename)
  cat("Creating file", final_file, "\n")
  rmarkdown::render(input=system.file("template", "report_html.Rmd", package = "iraceplot"), output_file=final_file)
  utils::browseURL(final_file)
}

has_file_extension <- function(filename, extension)
{
  if (startsWith(extension, ".")) extension <- substring(extension, 2L)
  grepl(paste0('[.]', extension, '$'), filename, ignore.case = TRUE)
}
