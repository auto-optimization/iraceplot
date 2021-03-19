#' Frequency and Density plot based on its iteration
#'
#' @description
#' The function will return two plots, one of frequency and
#' one of density of the numerical parameter, in case of being a categorical parameter
#' it will only show one of frequency
#'
#' @param iraceResults
#' The data generated when loading the Rdata file created by irace
#'
#' @param parameter
#' String, value of the parameter to be analyzed (example: parameter = "alpha")
#'
#' @param fileName
#' String, A pdf will be created in the location and with the
#' assigned name (example: "~/patch/example/filename")
#'
#' @return frequency and density plot
#' @export
#'
#' @examples
#' NULL

sampling_model_compare <- function(iraceResults,parameter,fileName = NULL){

  vectorPlot <- NULL

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

  if(iraceResults$parameters$types[[parameter]] == "c"){
    p <- sampling_frequency_iteration(iraceResults,parameter)
    vectorPlot[1] <- list(p)
  }else{
    p <- sampling_frequency_iteration(iraceResults,parameter)
    p <- p + theme(legend.position = "none")
    vectorPlot[1] <- list(p)
    q <- sampling_density(iraceResults,parameter)
    vectorPlot[2] <- list(q)
  }

  #If the value in fileName is added the pdf file is created
  if(!is.null(fileName)){
    pdf(paste0(fileName,".pdf"))
    if(length(vectorPlot) == 1){
      do.call("grid.arrange",c(vectorPlot,ncol=1))
    }else{
      do.call("grid.arrange",c(vectorPlot,ncol=2))
    }
    dev.off()
    #If you do not add the value of fileName, the plot is displayed
  }else{
    if(length(vectorPlot) == 1){
      do.call("grid.arrange",c(vectorPlot,ncol=1))
      #grid.arrange(vectorPlot,ncol=1)
    }else{
      do.call("grid.arrange",c(vectorPlot,ncol=2))
      #grid.arrange(vectorPlot,ncol=2)
    }
  }


}
