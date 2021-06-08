#' Report Iraceplot
#'
#' @param format
#' String, either "pdf" or "html". The parameter according to its
#' value is how the output file format will be. By default it is "html"
#'
#' @template arg_irace_results
#'
#' @param irace_file
#' String, indicate the location where the Rdata file is located
#' (example: "~/path-to/file.Rdata")
#'
#' @param file_name
#' String, file name indicating where to save a pdf file with the plot.
#' A pdf extension will be added to the file name provided
#' (example: "~/path-to/file_name")
#'
#' @return document
#' @export
#'
#' @examples
#' NULL
report <- function(format = "html", irace_results = NULL, irace_file = NULL, file_name) {


  # It is verified that only the location of the data file or its loaded form can be entered
  if (is.null(irace_results) & is.null(irace_file)) {
    return("You must enter irace_results or irace_file")
  } else if (!is.null(irace_file) & is.null(irace_results)) {
    load(irace_file)
  } else if (!is.null(irace_results) & !is.null(irace_file)) {
    return("You can only enter irace_results or irace_file, but not both")
  }

  if (format == "pdf") {
    final_file <- path_rel2abs(paste0(file_name, ".pdf"))
    # Output file name is generated
    reportes <- tibble(
      file_name = stringr::str_c(paste0(file_name, ".pdf"))
    )
    # The output location of the file is placed as well as the location report_pdf.Rmd
    reportes %>%
      select(output_file = file_name) %>%
      purrr::pwalk(rmarkdown::render, input = system.file("template", "report_pdf.Rmd", package = "iraceplot"))
  } else if (format == "html") {
    final_file <- path_rel2abs(paste0(file_name, ".html"))
    # Output file name is generated
    reportes <- tibble(
      file_name = stringr::str_c(paste0(file_name, ".html"))
    )
    # The output location of the file is placed as well as the location report_html.Rmd
    reportes %>%
      select(output_file = file_name) %>%
      purrr::pwalk(rmarkdown::render, input = system.file("template", "report_html.Rmd", package = "iraceplot"))
    browseURL(final_file)
  }
}
