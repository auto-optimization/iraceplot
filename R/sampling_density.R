#' Density plot based on its iteration
#'
#' @description
#' The function will return a density plot for parameters numeric data
#' in case of placing a categorical parameter, it will show a bar plot
#'
#' @param irace_results
#' The data generated when loading the Rdata file created by irace
#'
#' @param parameter
#' String, value of the parameter to be analyzed (example: parameter = "alpha")
#'
#' @param file_name
#' String, A pdf will be created in the location and with the
#' assigned name (example: "~/patch/example/file_name")
#'
#' @return density plot
#' @export
#'
#' @importFrom ggridges geom_density_ridges
#' @importFrom gridExtra grid.arrange
#'
#' @examples
#' sampling_density(iraceResults,parameter = "alpha")

sampling_density <- function(irace_results,parameter,file_name = NULL){
  #Variable assignment
  memo <- vectorPlot <- configuration <- x <- Freq <- iteration_f <- ..density.. <- NULL

  #verify that param_names is other than null
  if(!is.null(parameter)){
    #verify that param_names contain the data entered
    if( "FALSE" %in% names(table(parameter %in% irace_results$parameters$names))){
      return("Some wrong parameter entered")
    }
    #verify that param_names contain more than one parameter
    else if(length(parameter) != 1){
      return("You can only enter one parameter")
    }

  }

  # table is created with all settings
  tabla <-irace_results$allConfigurations[c(".ID.",parameter)]
  filtro <- unique(irace_results$experimentLog[,c("iteration","configuration")])


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

  }else if(class(tabla[[1]]) == "numeric"){

    tabla <- na.omit(tabla)
    tabla$iteration_f = factor(tabla$iteration,levels = rev(unique(tabla$iteration)))

    # density plot
    p <- ggplot(tabla, aes(x, y = iteration)) +
      geom_density_ridges(aes(fill = iteration), na.rm = TRUE) +
      scale_fill_manual(values = viridis(length(unique(tabla$iteration))))+
      labs(x = parameter, y = "Iteration")

    # # density plot
    # q <- ggplot(tabla, aes(x=x, fill=iteration)) +
    #   geom_density(alpha = 1, na.rm = TRUE) +
    #   facet_grid(vars(iteration_f),scales = "free", space = "free_y") +
    #   scale_fill_manual(values = viridis(length(unique(tabla$iteration)))) +
    #   labs(x = parameter) +
    #   scale_y_continuous(n.breaks = 3) +
    #   theme(strip.text.y = element_text(angle = 0))
    #
    # # The plot is saved in a list
    # vectorPlot[2] <- list(q)
  }

  #If the value in file_name is added the pdf file is created
  if(!is.null(file_name)){
    ggsave(file_name,plot = do.call("grid.arrange",c(vectorPlot,ncol=1)))
    #If you do not add the value of file_name, the plot is displayed
  }else{
    p
    return(p)
  }
}
