#' Report Iraceplot
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
    filename = stringr::str_c(paste0(fileName,".docx"))
  )

  reportes %>%
    select(output_file = filename) %>%
    purrr::pwalk(rmarkdown::render, input = "man/html/report.Rmd")

  #rmarkdown::render("doc/iraceplot_package.Rmd", output_format = "word_document")
}
