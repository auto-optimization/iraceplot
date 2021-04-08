#' Report Iraceplot
#'
#' @param iraceResults
#' irace log variable obtained from the Rdata file generate by irace
#'
#' @param locationRdata
#' String, indicate the location where the Rdata file is located
#' (example: "~/path-to/file.Rdata")
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

report <- function(iraceResults = NULL, locationRdata = NULL,fileName){

  filename <- NULL

  if(is.null(iraceResults) & is.null(locationRdata)){
    return("You must enter iraceResults or locationRData")
  }else if(!is.null(locationRdata) & is.null(iraceResults)){
    load(locationRdata)
  }else if(!is.null(iraceResults) & !is.null(locationRdata)){
    return("You can only enter iraceResults or locationRData, but not both")
  }

  reportes <- tibble(
    filename = stringr::str_c(paste0(fileName,".pdf"))
  )

  reportes %>%
    select(output_file = filename) %>%
    purrr::pwalk(rmarkdown::render, input = "man/html/report.Rmd")

}
