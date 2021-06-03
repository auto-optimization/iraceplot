#' Heat Map Plot
#'
#' @description
#' The function will return a heat map plot of all experimental data configurations
#'
#' @template arg_irace_results
#'
#' @param file_name
#' string, A pdf will be created in the location and with the
#' assigned name (example: "~/patch/example/file_name")
#'
#' @param interactive
#' Logical (Default interactive() ), Allows you to decide when generating the graph, i
#' t is generated interactively (It is created with the plotly package) which
#' is the default option or it is generated statically (It is created with the ggplot2
#' package). You must set interactive = FALSE.
#'
#' @return heatmap plot
#' @export
#'
#' @examples
#' plot_experiments_matrix(iraceResults, interactive = interactive() )

plot_experiments_matrix <- function(irace_results, file_name = NULL, interactive = interactive() ){
  #Variable assignment
  C <- RANK <- text <- i_id  <- union <- NULL

  #The values of the experiments are assigned to the variable experiments
  experiments <- irace_results$experiments
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
  p <- ggplot(tabla,aes(x=C, y=i_id, fill=RANK,text=text)) +
    geom_tile() +
    scale_fill_viridis_c(na.value = "#ECE1EB") +
    labs(x = "Configurations", y = "Instances") +
    theme(axis.text.x = element_blank(),axis.ticks = element_blank())

  #The plot becomes interactive
  if(interactive == TRUE){
    p <- plotly::ggplotly(p, tooltip="text")
  }


  #If the value in file_name is added the pdf file is created
  if(!is.null(file_name)){
    #The file_name value is worked to separate it and assign it to new values.
    nameFile = basename(file_name)
    directory = paste0(dirname(file_name),sep="/")
    withr::with_dir(directory, orca(p, paste0(nameFile,".pdf")))

  #If you do not add the value of file_name, the plot is displayed
  }else{
    p
    return(p)
  }
}
