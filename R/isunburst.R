isunburst <- function(iraceResults,fileName = NULL){

  param_c <- parents <- labels <- values <- ids <- NULL

  for(i in 1:length(iraceResults$parameters$types)){
    if(iraceResults$parameters$types[[i]] == "c"){
      param_c = c(param_c,names(iraceResults$parameters$types)[i])
    }
  }

  data <- as.data.frame(iraceResults$allConfigurations[param_c])

  for (j in 1:length(data)) {

    tabla <- table(data[j],useNA = "ifany")

    for (k in 1:length(tabla)) {
      if(k == 1){
        ids <- c(ids,colnames(data)[j])
        parents <- c(parents,"")
        labels <- c(labels,colnames(data)[j])
        values <- c(values,sum(tabla))
      }
      ids <- c(ids,paste(colnames(data)[j],names(tabla)[k],sep = " - "))
      parents <- c(parents,colnames(data)[j])
      labels <- c(labels,names(tabla)[k])
      values <- c(values,tabla[[k]])
    }
  }

  data_f <- data.frame(ids,parents,labels,values, stringsAsFactors = FALSE)
  data_f[is.na(data_f)] <- "NA"

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
