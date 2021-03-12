#' Frequency and Density plot based on its iteration
#'
#' @description
#' The function will return a frequency plot used
#' for categorical data (its values are string, show a bar plot) or
#' numeric data (show a histogram and density plot) by each iteration
#'
#' @param iraceResults
#' The data generated when loading the Rdata file created by irace
#'
#' @param parameter
#' String, value of the parameter to be analyzed (example: parameter = "algorithm")
#'
#' @param fileName
#' String, A pdf will be created in the location and with the assigned
#' name (example: "~/patch/example/filename")
#'
#' @return bar plot
#' @export
#'
#' @importFrom ggplot2 scale_fill_manual vars guide_legend facet_grid
#' @importFrom viridis viridis
#'
#' @examples
#' NULL

parameter_frequency_iteration <- function(iraceResults,parameter,fileName = NULL){
  #Variable assignment
  memo <- vectorPlot <- configuration <- x <- Freq <- iteration_f <- ..density.. <- NULL

  #verify that param_names is other than null
  if(!is.null(parameter)){
    #verify that param_names contain the data entered
    if( "FALSE" %in% names(table(parameter %in% iraceResults$parameters$names))){
      return("Some wrong parameter entered")
    }
    #verify that param_names contain more than one parameter
    else if(length(parameter) != 1){
      return("You can only enter one parameter")
    }

  }

  # table is created with all settings
  tabla <-iraceResults$allConfigurations[c(".ID.",parameter)]
  filtro <- unique(iraceResults$experimentLog[,c("iteration","configuration")])


  # The filter table is created and ordered according to the configurations
  filtro <- as.data.frame(filtro)
  filtro <- arrange(filtro,configuration)

  # An iteration table is created and added to the table
  iteration <- sample(NA,size=dim(tabla)[1],replace = TRUE)
  tabla <- cbind(tabla,iteration)

  # The NA of the first row of the table is replaced in the iteration column
  if(tabla$.ID.[1] == filtro$configuration[1] ){
    tabla$iteration[1] = filtro$iteration[1]
  }

  # memo is assigned the value of the filter table configuration
  memo = filtro$configuration[1]

  #The NAs of the table are replaced in the iteration column
  for(i in 2:dim(filtro)[1]){

    #if the same configuration has more than one iteration, a new row is created
    if(memo == filtro$configuration[i]){
      add <- tabla[tabla$.ID. == memo,]
      add$iteration = filtro$iteration[i]
      tabla <- rbind(tabla,add)
    }
    #The iteration is assigned to the configuration
    else{
      tabla$iteration[tabla$.ID. == filtro$configuration[i]] = filtro$iteration[i]
    }
    memo = filtro$configuration[i]
  }

  # Column .ID. and .PARENT. are removed
  tabla <- tabla[, !(names(tabla) %in% c(".ID."))]

  # the iteration column is scaled
  tabla[["iteration"]][1]<- as.character(tabla[["iteration"]][1])

  # The first column is renamed
  colnames(tabla)[1] <- "x"

  # If the parameter is of type character a frequency graph is displayed
  if(class(tabla[[1]]) == "character"){

    tabla <- as.data.frame(table(tabla))
    tabla$iteration_f = factor(tabla$iteration,levels = rev(unique(tabla$iteration)))

    p <- ggplot(tabla, aes(x=x,y=Freq, fill=x)) +
      geom_bar(stat = "identity") +
      facet_grid(vars(iteration_f),scales = "free") +
      scale_fill_manual(values = viridis(length(unique(tabla$x))),
                        guide = guide_legend(title = parameter)) +
      labs(y = "Frequency", x = parameter) +
      scale_y_continuous(n.breaks = 3) +
      theme(strip.text.y = element_text(angle = 0))

    # The plot is saved in a list
    vectorPlot[1] <- list(p)

  }else if(class(tabla[[1]]) == "numeric"){

    tabla <- na.omit(tabla)
    tabla$iteration_f = factor(tabla$iteration,levels = rev(unique(tabla$iteration)))

    nbreaks <- pretty(range(tabla$x), n = nclass.Sturges(tabla$x),
                      min.n = 1)
    # density and histogram plot
    p <- ggplot(as.data.frame(tabla), aes(x = x, fill=iteration)) +
      geom_histogram(aes(y = ..density..), breaks = nbreaks,
                     color = "black", fill = "gray") +
      geom_density(alpha = 0.7) +
      scale_fill_manual(values = viridis(length(unique(tabla$iteration)))) +
      facet_grid(vars(iteration_f),scales = "free") +
      labs(x=parameter, y="Frequency")+
      theme(axis.title.y = element_text(),
            axis.title.x = element_text(size = 10),
            plot.title = element_text(hjust = 0.5, size = rel(0.8), face = "bold"),
            axis.ticks.x = element_blank()
      ) +
      scale_y_continuous(n.breaks = 3) +
      theme(strip.text.y = element_text(angle = 0))

    # The plot is saved in a list
    vectorPlot[1] <- list(p)
  }

  #If the value in fileName is added the pdf file is created
  if(!is.null(fileName)){
    pdf(paste0(fileName,".pdf"))
    do.call("grid.arrange",c(vectorPlot,ncol=1))
    dev.off()
    #If you do not add the value of fileName, the plot is displayed
  }else{
    p
    return(p)
  }

}
