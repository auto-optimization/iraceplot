#' Report Iraceplot
#'
#' @param iraceResults
#' irace log variable obtained from the Rdata file generate by irace
#'
#' @param fileName
#' String, file name indicating where to save a pdf file with the plot.
#' A pdf extension will be added to the file name provided
#' (example: "~/path-to/filename")
#'
#' @return document
#' @export
#'
#' @importFrom dplyr tibble
#'
#' @examples
#' NULL

report <- function(iraceResults, fileName = NULL){

  filename <- NULL

  reportes <- tibble(
    filename = stringr::str_c(paste0(fileName,".pdf"))
  )

  reportes %>%
    select(output_file = filename) %>%
    purrr::pwalk(rmarkdown::render, input = "man/html/report.Rmd")

  #rmarkdown::render("doc/iraceplot_package.Rmd", output_format = "word_document")
}
