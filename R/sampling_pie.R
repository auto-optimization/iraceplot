#' Sunburst Plot
#'
#' @description
#' The isunburst function will return a sunburst plot of the categorical parameters
#'
#' @param iraceResults
#' The data generated when loading the Rdata file created by irace
#' @param parameters
#' String vector, a set of categorical type parameters
#' (example: parameters = c("algorithm","dlb"))
#' @param fileName
#' String, A pdf will be created in the location and with the assigned
#' name (example: "~/patch/example/filename")
#' @return sunburst plot
#' @export
#'
#' @examples
#' NULL

sampling_pie <- function(iraceResults, parameters = NULL,fileName = NULL){

  #variable assignment
  param_c <- parents <- labels <- values <- ids <- depend <-NULL

  dependency = FALSE
  # Logical (default FALSE) that allows to verify if the parameters
  # are dependent on others, modifying the visualization of the plot

  #assigns categorical type parameters to param_c
  for(i in 1:length(iraceResults$parameters$types)){
    if(iraceResults$parameters$types[[i]] == "c"){
      param_c = c(param_c,names(iraceResults$parameters$types)[i])
    }
  }

  if(!is.null(parameters)){
    if(FALSE %in% (parameters %in% param_c)){
      print("Only categorical data can be used")
      return(paste("The following parameters are not found:",parameters[!(parameters %in% param_c)]))
    }else{
      param_c = unique(parameters)
    }
  }

  #the table is generated only with categorical parameters
  data <- as.data.frame(iraceResults$allConfigurations[param_c])

  #checks if there is dependency between the parameters
  if(dependency == TRUE){
    for (i in 1:length(data)) {
      if(!identical(iraceResults$parameters$depends[[colnames(data)[i]]], character(0))){
        depend[colnames(data)[i]] <- list(iraceResults$parameters$depends[[colnames(data)[i]]])
      }
    }
  }

  #the table data is generated
  for (j in 1:length(data)) {

    tabla <- table(data[j],useNA = "ifany")

    for (k in 1:length(tabla)) {
      if(k == 1){

        ids <- c(ids,colnames(data)[j])

        if(!is.null(depend[[colnames(data)[j]]]) && dependency == TRUE){
          parents <- c(parents, depend[[colnames(data)[j]]])
        }else{
          parents <- c(parents,"")

        }
        labels <- c(labels,colnames(data)[j])
        values <- c(values,sum(tabla))

      }
      ids <- c(ids,paste(colnames(data)[j],names(tabla)[k],sep = " - "))
      parents <- c(parents,colnames(data)[j])
      labels <- c(labels,names(tabla)[k])
      values <- c(values,tabla[[k]])
    }
  }

  #The data table that will be used for the graph is created
  data_f <- data.frame(ids,parents,labels,values, stringsAsFactors = FALSE)
  data_f[is.na(data_f)] <- "NA"

  #if there is a dependency, the values ​​of the dependent data are added to its parent
  if(!is.null(depend) && dependency == TRUE){
    for (i in 1:length(depend)) {
      data_f$values[data_f$ids == depend[[i]]] <- data_f$values[data_f$ids == depend[[i]]] + data_f$values[data_f$ids == names(depend[i])]
    }
  }

  #the graph is created
  p <- plot_ly(
    type='sunburst',
    ids = data_f$ids,
    labels=data_f$labels,
    parents=data_f$parents,
    values = data_f$values,
    branchvalues = "total"
  )

  #If the value in fileName is added the pdf file is created
  if(!is.null(fileName)){
    #The fileName value is worked to separate it and assign it to new values.
    nameFile = basename(fileName)
    directory = paste0(dirname(fileName),sep="/")
    withr::with_dir(directory, orca(p, paste0(nameFile,".pdf")))

    #If you do not add the value of fileName, the plot is displayed
  }else{
    p
  }

}
