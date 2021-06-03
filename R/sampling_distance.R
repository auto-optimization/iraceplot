#' Distance Iteration Plot
#'
#' @description
#' Shows the mean of the difference between the configurations that were run for each iteration
#'
#' @template arg_irace_results
#' @param type
#' String, either "line", "boxplot" or "both". by default it is "both" will show both graphics, "line" which will show a plot of
#' points and lines, "boxplot" will show a box plot
#' @param t
#' Numeric, It is a percentage factor that will determine the range of difference
#' between settings (example: t = 0.05 is equivalent to 5 percent)
#' @param file_name
#' String, A pdf will be created in the location and with the assigned
#' name (example: "~/patch/example/file_name")
#'
#' @return line or box plot
#'
#' @export
#'
#' @examples
#' #sampling_distance(iraceResults)
#' #sampling_distance(iraceResults, type = "boxplot", t=0.2)

sampling_distance <- function(irace_results, type = "both", t = 0.05, file_name = NULL){

  if(!(type == "line" | type == "boxplot" | type == "both")){
    print("The type parameter entered is incorrect")
  }

  #variable assignment
  media <- allconf <- valor <- iterations <- tabla_box <- iteration <- vectorP <-NULL
  allconf <- irace_results$allConfigurations
  n_param <- length(allconf) - 2

  #The value of the distance between each configuration is created
  for (i in 1:length(irace_results$allElites)) {
    distance <- NULL
    ids <- unique(subset(as.data.frame(irace_results$experimentLog),
                         iteration %in% i, select = c("configuration"),
                         drop = TRUE))
    iterations <- c(iterations,i)

    for (j in 1:(length(ids))){
      for (k in j:(length(ids))) {
        valor <- distance_config(irace_results, id_configurations = c(ids[j],ids[k]), t)
        distance <- c(distance,valor)
      }
    }

    it <- i
    datos <- data.frame(it,distance)
    tabla_box <- rbind(tabla_box,datos)
    media <- c(media, mean(distance))
  }

  #A graph of points and lines is created
  if(type == "line" | type == "both"){

    tabla <- data.frame(iterations,media)

    p <- ggplot(tabla,aes(x = iterations,y=media, color=iterations)) +
         geom_point() +
         geom_line() +
         scale_y_continuous(limits = c(0,n_param),
                            breaks = seq(0,n_param,2)) +
         scale_x_continuous(limits = c(0.8,length(irace_results$allElites)+0.2),
                             breaks = seq(1,length(irace_results$allElites),1)) +
         scale_color_viridis_c() +
         labs(y = "RPD", x = "iteration", color = "IT.")
    vectorP[1] <- list(p)

  #A box plot is created
  }
  if(type == "boxplot" | type == "both"){

    p <- ggplot(tabla_box, aes(x=it,y=distance, group = it, color = it)) +
      geom_boxplot(na.rm = TRUE) +
      scale_color_viridis_c()+
      scale_y_continuous(limits = c(0,n_param),
                         breaks = seq(0,n_param,2)) +
      scale_x_continuous(limits = c(0.6,length(irace_results$allElites)+0.4),
                         breaks = seq(1,length(irace_results$allElites),1)) +
      labs(x = "iteration", y = "RPD", color = "IT.")
    vectorP[2] <- list(p)

    }

  #If the value in file_name is added the pdf file is created
  if(!is.null(file_name)){
    if(type == "both"){
      pdf(paste0(file_name,".pdf"), width = 12)
      do.call("grid.arrange",c(vectorP[1],ncol=1))
      do.call("grid.arrange",c(vectorP[2],ncol=1))
      dev.off()
    }else{
      ggsave(file_name,plot = p)
    }

    #If you do not add the value of file_name, the plot is displayed
  }else{
    if(type == "both"){
      do.call("grid.arrange",c(vectorP,nrow = 2))
    }else{
      p
      return(p)
    }
  }
}
