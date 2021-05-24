#'Box Plot
#'
#'Create a graphic of box plot of irace using the best quality id
#'By default the graph of the last iteration is shown
#'
#'@template arg_irace_results
#'
#'@param number_iteration
#'Numeric, It is the number referring to the iteration that you want to graph,
#'the range of these varies according to the Rdata used (example: number_iteration = 5)
#'
#'@param id_configurations
#'Numeric vector, you need to put the configurations you want to analyze
#' (example: id_configurations = c(20,50,100,300,500,600,700))
#'
#'@param rpd
#'Logical (default TRUE) to fit through an equation of minimum percentage distance
#'between the values of each row of all configurations
#'
#'@param file_name
#' String, A pdf will be created in the location and with the
#' assigned name (example: "~/patch/example/file_name")
#'
#'@return box plot
#'
#'@importFrom stats reshape
#'@importFrom ggplot2 ggplot geom_boxplot geom_jitter position_jitter aes theme labs
#'@importFrom grDevices dev.off pdf
#'
#'@export
#'
#'@examples
#'boxplot_training(iraceResults)
#'boxplot_training(iraceResults,number_iteration = 5)
#'boxplot_training(iraceResults,id_configurations = c(20,50,100,300,500,600,700))
#'

boxplot_training <- function(irace_results, number_iteration = NULL, id_configurations = NULL, rpd = TRUE ,file_name = NULL){

  #Variable assignment
  Performance <- Elite_configuration <- NULL
  long <- length(irace_results$allElites)

  if(!is.null(number_iteration) & !is.null(id_configurations)){
    return("You cannot use id_configurations and number_iteration at the same time")
  }

  #It is checked if the file_name argument was added
  if(!is.null(number_iteration)){
    #We verify that number_iteration is within the range of values it can take
    if(number_iteration > 0 && number_iteration <= long){
      long <- number_iteration
    #If number_iteration is out of range it delivers a message per screen
    }else{
      return(print("iteration number out of range"))
    }
  }

  #A vector is created with the id of all elite configurations from the iteration entered
  id <- irace_results$allElites[[long]]

  if(!is.null(id_configurations)){
    n_conf = c(1:dim(irace_results$experiments)[2])
    if(FALSE %in% (id_configurations %in% n_conf)){
      return(paste("The following settings are out of range:",id_configurations[!(id_configurations %in% n_conf)]))
    }else{
      id <- id_configurations
    }
  }

  #A table is created with the values of all elite configurations of the id of the requested iteration
  distance <- irace_results$experiments

  if(rpd == TRUE){
    distance <- 100*(distance - apply(distance,1,min, na.rm = TRUE))/apply(distance,1,min, na.rm=TRUE)
  }

  matriz <- as.data.frame(distance[,id])

  #If the length of id is one, a different value must be added to the column
  if(length(id) == 1){
    colnames(matriz)[colnames(matriz) == "irace_results$experiments[, id]"] <- id
  }

  #value of elements that the matrix contains
  n_row_col = as.numeric(dim(matriz)[1]*dim(matriz)[2])

  #A restructured table is created
  tabla <- reshape(matriz,varying = c(as.character(id)),
                   v.names = "Performance",
                   timevar = "Elite_configuration",
                   times = c(as.character(id)),
                   new.row.names = 1:n_row_col,
                   direction = "long")

  #The plot scatter is created and assigned to p
  p <- ggplot(tabla, aes(x=Elite_configuration,y=Performance,color=Elite_configuration)) +
    geom_boxplot(na.rm = TRUE) +
    geom_jitter(shape=16, position=position_jitter(0.2), na.rm = TRUE) +
    theme(legend.position="none") +
    labs(x="Elite Configurations")

  #If the value in file_name is added the pdf file is created
  if(!is.null(file_name)){
    ggsave(file_name,plot = p)
  #If you do not add the value of file_name, the plot is displayed
  }else{
    p
    return(p)
  }
}
