#' Report Iraceplot
#' 
#' The `report` function creates an HTML report of the most relevant irace log
#' data.
#' 
#' This report provides general statistics and plots that show the best configurations
#' and their performance.
#'
#' @template arg_irace_results
#'
#' @param filename (`character(1)`)
#' String, file name indicating where to save the report (example: "~/path-to/filename")
#'
#' @return document
#'
#' @examples
#' NULL
#' @export
#' @md
report <- function(irace_results,  filename) {

  # It is verified that only the location of the data file or its loaded form can be entered
  if (is.null(irace_results)) {
    stop("Error: You must provide irace_results \n")
  } 
  
  if (is.null(filename)) {
    stop("Error: You must provide a filename \n")
  } 
  
  final_file <- path_rel2abs(paste0(filename, ".html"))
  # Output file name is generated
  reportes <- tibble(
    filename = stringr::str_c(paste0(filename, ".html"))
  )
  cat("Creating file", final_file, "\n")

  rmarkdown::render(input=system.file("template", "report_html.Rmd", package = "iraceplot"), output_file=final_file)
  
  utils::browseURL(final_file)
  
}
