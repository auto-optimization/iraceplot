#' Scatter Plot Training
#'
#' @description
#' The function will return a scatter plot
#' comparing two elite configurations in training
#'
#' @template arg_irace_results
#'
#' @param id_configurations
#' Numeric vector, you need to put the elite settings you
#' want to compare, only 2 values are allowed (example: idVector = c(806,809))
#'
#' @param rpd
#' Logical (default TRUE) to fit through an equation of minimum percentage
#' distance between the values of each row of all configurations
#'
#' @param file_name
#' String, A pdf will be created in the location and with the
#' assigned name (example: "~/patch/example/file_name")
#'
#' @return plot
#' @export
#'
#'@importFrom ggplot2 geom_point
#'
#' @examples
#' scatter_training(iraceResults, id_configurations = c(806,809))

scatter_training <- function(irace_results, id_configurations, rpd = TRUE, file_name = NULL){

  #Variable assignment
  iteracionFiltrada <- NULL

  #Verify that the entered id are within the possible range
  if(id_configurations[1] <= 0 || id_configurations[1] > dim(irace_results$experiments)[2]){
    return(paste("id out of range",id_configurations[1]))
  }else if(id_configurations[2] <= 0 || id_configurations[2] > dim(irace_results$experiments)[2]){
    return(paste("id out of range",id_configurations[2]))
  }
  #Verify that a vector of length 2 is entered
  if(length(id_configurations) == 1 || length(id_configurations) > 2){
    return("You must enter a vector with 2 values")
  }

  distance <- irace_results$experiments

  if(rpd == TRUE){

    distance <- 100*(distance - apply(distance,1,min, na.rm = TRUE))/apply(distance,1,min, na.rm=TRUE)
  }

  #An array of true and/or false to display if the field has data
  filtro <- !is.na(distance[,id_configurations])

  #A vector is created only with paired iterations (both true)
  for(i in 1:dim(filtro)[1]){
    if(filtro[i,][1] && filtro[i,][2]){
      iteracionFiltrada <- c(iteracionFiltrada,i)
    }
  }

  #A table is created with only the paired values
  tabla <- as.data.frame((distance[,id_configurations])[iteracionFiltrada,])

  #If you can't find paired data
  if(length(iteracionFiltrada) == 0){
    return("The entered configurations do not have paired data")
  #If it only finds a paired data, the table must be restructured
  }else if(length(iteracionFiltrada) == 1){
    conf1 <- tabla[1,]
    conf2 <- tabla[2,]
    tabla <- as.data.frame(conf1)
    tabla$conf2 <- conf2
  }

  #The names of the columns are changed otherwise they would be shown in the graph as they are numbers
  colnames(tabla)[colnames(tabla) == id_configurations[1]] <- "conf1"
  colnames(tabla)[colnames(tabla) == id_configurations[2]] <- "conf2"

  #The plot scatter is created and assigned to p
  p <- ggplot(tabla, aes(x=conf1, y=conf2, color=conf1)) +
    geom_point() +

  if(rpd == TRUE){
    labs(color=" ",x = paste("Configuration",id_configurations[1],"RPD"), y = paste("Configuration",id_configurations[2],"RPD"))
  }else{
    labs(color=" ",x = paste("Configuration",id_configurations[1],"Performance"), y = paste("Configuration",id_configurations[2],"Performance"))
  }

  #If the value in file_name is added the pdf file is created
  if(!is.null(file_name)){
    ggsave(file_name,plot = p)
    return(p)
  #If you do not add the value of file_name, the plot is displayed
  }else{
    p
    return(p)
  }
}
