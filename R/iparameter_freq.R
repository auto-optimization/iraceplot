#' Parameter Frequency Plot
#'
#' @param iraceResults
#' The data generated when loading the Rdata file created by irace
#'
#' @param param_names
#' It's of type vector
#' Is a vector with the parameters to be displayed in the plot
#'
#'
#' @param fileName
#' It's of type string
#' You must place the path where you want to save the file and its name without the extension pdf
#'
#' @return plot
#' @export
#'
#' @importFrom ggplot2 geom_bar geom_density geom_histogram ggtitle scale_y_continuous rel
#' @importFrom stats na.omit
#' @importFrom grDevices nclass.Sturges
#'
#' @examples
#' NULL

iparameter_freq <- function(iraceResults, param_names = NULL, fileName = NULL){

  #Variable assignment
  vectorG <- tabla <- Var1 <- Freq <- ..density.. <-NULL
  param_names <- unlist(param_names)

  if(!is.null(param_names)){
    if("FALSE" %in% (param_names %in% iraceResults$parameters$names)){
      return("Some wrong parameter entered")
    }
    config <- iraceResults$allConfigurations[param_names]

  }else{
    config <- iraceResults$allConfigurations[iraceResults$parameters$names]
  }


  for(i in 1:length(config)){

    #plot bars
    if(class(config[[i]]) == "character"){
      tabla <- as.data.frame(table(config[[i]]))
      p <- ggplot(data = tabla, aes(x = Var1,y = Freq)) +
        geom_bar(stat = "identity",fill = "grey", color = "black") +
        labs(x = "Values") +
        ggtitle(colnames(config)[i]) +
        theme(axis.title.y = element_blank(),
              axis.title.x = element_text(size = 8),
              plot.title = element_text(hjust = 0.5, size = rel(0.8), face = "bold"),
              axis.ticks.x = element_blank()
              ) +
        scale_y_continuous(n.breaks = 3)
      #the plot is saved in a list
      vectorG[i] <- list(p)

    }
    #histogram and density plot
    else if(class(config[[i]]) == "numeric"){
      tabla <- na.omit(config[[i]])
      nbreaks <- pretty(range(tabla), n = nclass.Sturges(tabla),
                        min.n = 1)
      q <- ggplot(as.data.frame(tabla), aes(x = tabla)) +
        geom_histogram(aes(y = ..density..), breaks = nbreaks,
                       color = "black", fill = "gray") +
        geom_density(color = "blue",fill = "blue", alpha = 0.2) +
        labs(x="Values")+
        ggtitle(colnames(config)[i])+
        theme(axis.title.y = element_blank(),
              axis.title.x = element_text(size = 8),
              plot.title = element_text(hjust = 0.5, size = rel(0.8), face = "bold"),
              axis.ticks.x = element_blank()
              ) +
        scale_y_continuous(n.breaks = 3)
      #the plot is saved in a list
      vectorG[i] <- list(q)
    }
  }

  #If the value in fileName is added the pdf file is created
  if(!is.null(fileName)){
    pdf(paste0(fileName,".pdf"))
    if(length(config) == 1 ){
      do.call("grid.arrange",c(vectorG,ncol=1))
    }else if(length(config) == 2){
      do.call("grid.arrange",c(vectorG,ncol=2))
    }else if(length(config) > 2 && length(config) <= 9){
      do.call("grid.arrange",c(vectorG,ncol=3))
    }else{
      a = 1
      b = 9
      for(k in 1:(ceiling(length(config)/9))){
        do.call("grid.arrange",c(vectorG[a:b],ncol=3))
        a = b+1
        if(length(config) > (k+1)*9){
          b = (k+1)*9
        }else{
          b = length(config)
        }
      }
    }

    dev.off()
  #If you do not add the value of fileName, the plot is displayed
  }else{
    do.call("grid.arrange",c(vectorG,ncol=3))
  }

}
