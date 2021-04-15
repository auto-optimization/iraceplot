#' Report Iraceplot
#'
#' @param format
#' String, either "pdf" or "html". The parameter according to its
#' value is how the output file format will be.,By default it is "html"
#'
#' @param iraceResults
#' irace log variable obtained from the Rdata file generate by irace
#'
#' @param iraceFile
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

report <- function(format = "html",iraceResults = NULL, iraceFile = NULL,fileName){
  #start of parameters
  filename <- NULL

  #It is verified that only the location of the data file or its loaded form can be entered
  if(is.null(iraceResults) & is.null(iraceFile)){
    return("You must enter iraceResults or iraceFile")
  }else if(!is.null(iraceFile) & is.null(iraceResults)){
    load(iraceFile)
  }else if(!is.null(iraceResults) & !is.null(iraceFile)){
    return("You can only enter iraceResults or iraceFile, but not both")
  }

  if(format == "pdf"){
    final_file <- irace:::path.rel2abs(paste0(fileName,".pdf"))
    #Output file name is generated
    reportes <- tibble(
      filename = stringr::str_c(paste0(fileName,".pdf"))
    )
    #The output location of the file is placed as well as the location report_pdf.Rmd
    reportes %>%
      select(output_file = filename) %>%
      purrr::pwalk(rmarkdown::render, input = system.file("template","report_pdf.Rmd", package = 'iraceplot'))
  }else if(format == "html"){
    final_file <- irace:::path.rel2abs(paste0(fileName,".html"))
    print(final_file)
    #Output file name is generated
    reportes <- tibble(
      filename = stringr::str_c(paste0(fileName,".html"))
    )
    #The output location of the file is placed as well as the location report_html.Rmd
    reportes %>%
      select(output_file = filename) %>%
      purrr::pwalk(rmarkdown::render, input = system.file("template","report_html.Rmd", package = 'iraceplot'))
    browseURL(final_file)
  }


}
