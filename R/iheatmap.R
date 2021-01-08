#' Heat Map Plot
#'
#' Create a heat map plot interactive
#'
#' @param iraceResults
#'
#'The data generated when loading the Rdata file created by irace
#'
#' @param fileName
#'
#'It's of type string
#'You must place the path where you want to save the file and its name without the extension pdf
#'
#' @return plot
#' @export
#'
#' @importFrom plotly ggplotly orca
#' @importFrom tidyr pivot_longer
#' @importFrom forcats fct_relevel
#' @importFrom tibble rownames_to_column
#' @importFrom ggplot2 geom_tile scale_fill_viridis_c element_blank
#' @importFrom graphics text
#' @importFrom stats C
#' @importFrom dplyr mutate %>%
#' @importFrom stringr str_split
#'
#'
#'
#' @examples
#' NULL

iheatmap <- function(iraceResults, fileName = NULL){

  C <- RANK <- text <- i_id  <- union <- NULL

  #The values of the experiments are assigned to the variable experiments
  experiments <- iraceResults$experiments
  experiments[] <- rank(experiments, na.last="keep")

  #he table is created and organized for ease of use
  tabla <- experiments %>%
  as.data.frame() %>%
  rownames_to_column("i_id") %>%
  pivot_longer(-c(i_id), names_to = "C", values_to = "RANK") %>%
  mutate(C= fct_relevel(C,as.character(1:ncol(experiments)))) %>%
  mutate(i_id= fct_relevel(i_id,as.character(1:nrow(experiments))))

  #The text field was added to the table to show it in the interactive plot
  tabla <- tabla %>%
  mutate(text = paste0("x: ", C, "\n", "y: ", i_id, "\n", "Value: ",round(RANK,2), "\n"))

  #Heat map plot is created
  q <- ggplot(tabla,aes(x=C, y=i_id, fill=RANK,text=text)) + geom_tile()  +scale_fill_viridis_c(na.value = "#ECE1EB") + labs(x = "Configurations", y = "ID") + theme(axis.text.x = element_blank(),axis.ticks = element_blank())

  #The plot becomes interactive
  p <- plotly::ggplotly(q, tooltip="text")

  #If the value in fileName is added the pdf file is created
  if(!is.null(fileName)){
    #print("Aun no se implementa el pasarlo a pdf, ya que trabaja de otra forma que es orca")
    directory = str_split(fileName,"/")
    nameFile = directory[[1]][length(directory[[1]])]
    directory = directory[[1]][-length(directory[[1]])]
    for(i in 1:length(directory)){
      union <- paste0(union,directory[[i]],sep="/")
    }

    withr::with_dir(union, orca(p, paste0(nameFile,".pdf")))
  #If you do not add the value of fileName, the plot is displayed
  }else{
    p
  }
}
