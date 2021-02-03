#' Box Plot Testing
#'
#' @param iraceResults
#' The data generated when loading the Rdata file created by irace
#' @param type
#' type = NULL, all testing experiments settings
#' type = 1, the last iteration of the elite settings
#' type = 2, the best settings of each iteration
#' @param distance_min
#' The data is treated referring to the minimum distance of each percentage row
#' @param fileName
#' It's of type string
#' You must place the path where you want to save the file and its name without the extension pdf
#'
#' @return plot
#' @export
#'
#' @examples
#' NULL

iboxplot_test <- function(iraceResults, type = NULL, distance_min = FALSE ,fileName = NULL){

  # verify that test this in iraceResults
  if(!("testing" %in% names(iraceResults))){
    return("iraceResults does not contain the testing element")
  }

  ids <- performance <- NULL
  # the table is created with all the data from testing experiments
  tabla <- as.data.frame(iraceResults$testing$experiments)

  # the table values are modified
  if(distance_min == TRUE){
    tabla <- 100*(tabla - apply(tabla,1,min))/apply(tabla,1,min)
  }
  # all testing experiments settings
  if(is.null(type)){
    datos <- tabla
  }
  # the last iteration of the elite settings
  else if(type == 1){
    num_it <- length(iraceResults$allElites)
    datos <- tabla[as.character(iraceResults$allElites[[num_it]])]

  }
  # the best settings of each iteration
  else if(type == 2){
    datos <- tabla[as.character(iraceResults$iterationElites)]
  }else{
    return("non existent type")
  }

  # the data is processed
  datos <- reshape(datos,
                  varying = as.vector(colnames(datos)),
                  v.names = "performance",
                  timevar = "ids",
                  times = as.vector(colnames(datos)),
                  new.row.names = 1:(dim(datos)[1]*dim(datos)[2]),
                  direction = "long")
  # column iteration is added
  if(!is.null(type)){
    if(type == 2){
      iteration <- sample(NA,size=dim(datos)[1],replace = TRUE)
      datos <- cbind(datos,iteration)

      for(i in 1:length(unique(datos$ids))){
        datos$iteration[datos$ids == unique(datos$ids)[i]] <- i
      }
      datos$iteration_f = factor(datos$iteration,levels = (unique(datos$iteration)))
    }
  }

  # the box plot is created
  p <- ggplot(datos, aes(x=ids,y=performance,color=ids)) +
    geom_boxplot() +
    theme(legend.position="none") +
    labs(x="IDs")

  # each box plot is divided by iteration
  if(!is.null(type)){
    if(type == 2){
      p <- p + facet_grid(cols = vars(datos$iteration_f), scales = "free")
    }
  }

  #If the value in fileName is added the pdf file is created
  if(!is.null(fileName)){
    pdf(paste0(fileName,".pdf"))
    plot(p)
    dev.off()
    #If you do not add the value of fileName, the plot is displayed
  }else{
    p
  }

}
