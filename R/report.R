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
#' # load(system.file(package="irace", "exdata", "irace-acotsp.Rdata", mustWork = TRUE))
#' # report(iraceResults, filename="report")
#' NULL
#' @export
report <- function(irace_results, filename, interactive = base::interactive())
{
  # TODO: use irace::read_logfile() so that irace_results can be also the path
  # to .Rdata.
  if (missing(irace_results)) stop("You must provide irace_results")
  if (missing(filename)) stop("You must provide a filename")
 
  filename <- path_rel2abs(maybe_add_file_extension(filename, "html"))
  cat("Creating file", filename, "\n")
  rmarkdown::render(input=system.file("template", "report_html.Rmd", package = "iraceplot"), output_file=filename)
  if (interactive) utils::browseURL(filename)
  filename
}

