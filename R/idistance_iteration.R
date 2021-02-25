#' Distance Iteration Plot
#'
#' @description
#' Shows the mean of the difference between the configurations that were run for each iteration
#'
#' @param iraceResults
#' The data generated when loading the Rdata file created by irace
#' @param type
#' String, either "line" or "boxplot". by default it is "line" which will show a plot of
#' points and lines, "boxplot" will show a box plot
#' @param t
#' Numeric, It is a percentage factor that will determine the range of difference
#' between settings (example: t = 0.05 is equivalent to 5 percent)
#' @param fileName
#' String, A pdf will be created in the location and with the assigned
#' name (example: "~/patch/example/filename")
#'
#' @importFrom ggplot2 geom_line scale_x_continuous
#'
#' @return line or box plot
#'
#' @export
#'
#' @examples
#' NULL

idistance_iteration <- function(iraceResults, type = "line", t = 0.05, fileName = NULL){

  if(!(type == "line" | type == "boxplot")){
    print("The type parameter entered is incorrect")
  }

  #variable assignment
  media <- allconf <- valor <- iterations <- tabla_box <- iteration <-NULL
  allconf <- iraceResults$allConfigurations
  n_param <- length(allconf) - 2

  #The value of the distance between each configuration is created
  for (i in 1:length(iraceResults$allElites)) {
    distance <- NULL
    ids <- unique(subset(as.data.frame(iraceResults$experimentLog),
                         iteration %in% i, select = c("configuration"),
                         drop = TRUE))
    iterations <- c(iterations,i)

    for (j in 1:(length(ids))){
      for (k in j:(length(ids))) {
        valor <- distance_config(iraceResults, idConfigurations = c(ids[j],ids[k]), t)
        distance <- c(distance,valor)
      }
    }

    it <- i
    datos <- data.frame(it,distance)
    tabla_box <- rbind(tabla_box,datos)
    media <- c(media, mean(distance))
  }

  #A graph of points and lines is created
  if(type == "line"){

    tabla <- data.frame(iterations,media)

    p <- ggplot(tabla,aes(x = iterations,y=media, color=iterations)) +
         geom_point() +
         geom_line() +
         scale_y_continuous(limits = c(0,n_param),
                            breaks = seq(0,n_param,2)) +
         scale_x_continuous(limits = c(0,length(iraceResults$allElites)+1),
                             breaks = seq(1,length(iraceResults$allElites),1)) +
         scale_color_viridis_c() +
         labs(y = "RPD", x = "iteration")

  #A box plot is created
  }else if(type == "boxplot"){

    p <- ggplot(tabla_box, aes(x=it,y=distance, group = it, color = it)) +
      geom_boxplot(na.rm = TRUE) +
      scale_color_viridis_c()+
      scale_y_continuous(limits = c(0,n_param),
                         breaks = seq(0,n_param,2)) +
      scale_x_continuous(limits = c(0,length(iraceResults$allElites)+1),
                         breaks = seq(1,length(iraceResults$allElites),1)) +
      labs(x = "iteration", y = "RPD")

    }

  #If the value in fileName is added the pdf file is created
  if(!is.null(fileName)){
    pdf(paste0(fileName,".pdf"), width = 12)
    plot(p)
    dev.off()
    #If you do not add the value of fileName, the plot is displayed
  }else{
    p
  }
}
