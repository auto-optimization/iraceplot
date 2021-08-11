#' Report Iraceplot
#' 
#' The `report` function creates an HTML report of the most relevant irace log
#' data.
#'
#' @template arg_irace_results
#'
#' @param file_name
#' String, file name indicating where to save the report (example: "~/path-to/file_name")
#'
#' @return document
#' @export
#'
#' @examples
#' NULL
report <- function(irace_results,  file_name) {

  # It is verified that only the location of the data file or its loaded form can be entered
  if (is.null(irace_results)) {
    cat("Error: You must provide irace_results \n")
    stop()
  } 
  

  final_file <- path_rel2abs(paste0(file_name, ".html"))
  # Output file name is generated
  reportes <- tibble(
    file_name = stringr::str_c(paste0(file_name, ".html"))
  )
  cat("Creating file", final_file, "\n")
  # The output location of the file is placed as well as the location report_html.Rmd
  reportes %>%
    select(output_file = file_name) %>%
    purrr::pwalk(rmarkdown::render, input = system.file("template", "report_html.Rmd", package = "iraceplot"))
  
  browseURL(final_file)
  
}
