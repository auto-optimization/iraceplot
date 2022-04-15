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
#'
#' @examples
#' NULL
#' @export
#' @md
report <- function(irace_results, filename)
{
  # It is verified that only the location of the data file or its loaded form can be entered
  if (missing(irace_results)) {
    stop("Error: You must provide irace_results \n")
  } 
  
  if (missing(filename)) {
    stop("Error: You must provide a filename \n")
  } 
  if (!has_file_extension(filename, "html"))
    filename <- paste0(filename, ".html")
  final_file <- path_rel2abs(filename)
  cat("Creating file", final_file, "\n")
  rmarkdown::render(input=system.file("template", "report_html.Rmd", package = "iraceplot"), output_file=final_file)
  utils::browseURL(final_file)
}

has_file_extension <- function(filename, extension)
{
  grepl(paste0('[.]', extension, '$'), filename, ignore.case = TRUE)
}
