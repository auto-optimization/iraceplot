#' Report Iraceplot
#'
#' @return document
#' @export
#'
#' @importFrom dplyr tibble
#'
#' @examples
#' NULL

report <- function(){

  filename <- NULL

  reportes <- tibble(
    filename = stringr::str_c("report_iraceplot.docx")
  )

  reportes %>%
    select(output_file = filename) %>%
    purrr::pwalk(rmarkdown::render, input = "doc/iraceplot_package.Rmd")

  #rmarkdown::render("doc/iraceplot_package.Rmd", output_format = "word_document")
}
