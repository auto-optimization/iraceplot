#' Parallel Coordinates Plot static
#'
#' @description
#' The iparallelcoord function will return a parallel cordinates plot
#' allowing the analysis of the set of parameters
#'
#' @param iraceResults
#' The data generated when loading the Rdata file created by irace
#'
#' @param idConfiguration
#' Numeric vector, you need to put the configurations you want to analyze
#' (example: idConfiguration = c(20,50,100,300,500,600,700))
#'
#' @param param_names
#' String vector, you need to put the parameters you want to analyze
#' (example: param_names = c("algorithm","alpha","rho","q0","rasrank"))
#' @param iterations
#'
#'
#' @param fileName
#' A pdf will be created in the location and with
#' the assigned name (example: "~/patch/example/filename")
#'
#' @return parallel cordinates plot
#' @export
#' @importFrom GGally ggparcoord
#' @importFrom ggplot2 element_text scale_color_viridis_d
#' @importFrom plotly plot_ly add_trace
#'
#'
#' @examples
#' NULL

iparallelcoord <- function(iraceResults, idConfiguration = NULL, param_names = NULL, iterations = NULL,fileName = NULL){

  #Variable assignment
  memo  <- configuration <- dim <- NULL
  idConfiguration <- unlist(idConfiguration)
  param_names <- unlist(param_names)

  #verify that param_names is other than null
  if(!is.null(param_names)){
    #verify that param_names contain the data entered
    if( "FALSE" %in% names(table(param_names %in% iraceResults$parameters$names))){
      return("Some wrong parameter entered")
    }
    #verify that param_names contain more than one parameter
    else if(length(param_names) < 2){
      return("You must enter at least two parameters")
    }

  }

  if(!is.null(idConfiguration)){

    # Verify that the entered id are within the possible range
    if(length(idConfiguration[idConfiguration < 1]) >= 1 || length(idConfiguration[idConfiguration > dim(iraceResults$allConfigurations)[1]]) >= 1){
      return("IDs entered are outside the range of settings")
    }

    # Verify that the id entered are more than 1 or less than the possible total
    if(length(idConfiguration) <= 1 || length(idConfiguration) > dim(iraceResults$allConfigurations)[1] ){
      return("You must enter more than one id")
    }

    # the table to be used and the filter with the iterations and configuration is created
    selection <- iraceResults$allConfigurations[, ".ID."] %in% idConfiguration
    tabla <- iraceResults$allConfigurations[selection,]
    filtro <- unique(iraceResults$experimentLog[,c("iteration","configuration")])
    selection2 <- filtro[, "configuration"] %in% idConfiguration
    filtro <- filtro[selection2,]
  }
  # table is created with all settings
  else{
    tabla <-iraceResults$allConfigurations
    filtro <- unique(iraceResults$experimentLog[,c("iteration","configuration")])
  }

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
  tabla <- tabla[, !(names(tabla) %in% c(".ID.",".PARENT."))]
  if(!is.null(param_names)){
    param_names <- c(param_names,"iteration")
    tabla <- tabla[, (names(tabla) %in% param_names )]
  }

  #NA data processing
  for(k in 1:(length(tabla))){
    if(class(tabla[[k]]) == "numeric" && NA %in% tabla[[k]]){
      tabla[[k]][is.na(tabla[[k]])] <- (iraceResults$parameters$domain[[colnames(tabla)[k]]][2] + 1)
    }else if(class(tabla[[k]]) == "character" && NA %in% tabla[[k]]){
      tabla[[k]][is.na(tabla[[k]])] <- "NA"
    }
  }

  if(!is.null(iterations)){
     tabla <- tabla[tabla$iteration %in% iterations,]
  }

  #create plot dimensions
  for(i in 1:length(tabla)){

    if(colnames(tabla)[i] == "iteration"){
      dim[[i]] = list(
        range = c(min(tabla[[i]], na.rm = TRUE),max(tabla[[i]], na.rm = TRUE)),
        values = tabla[[i]],
        label = colnames(tabla)[i],
        visible = FALSE
      )
      #if the column is of type character
    }else if(class(tabla[[i]]) == "character"){
      factor_tab <- NULL
      factor_tab <- factor(tabla[[i]])
      levels(factor_tab) <- c(1:length(unique(tabla[[i]])))
      if("NA" %in% tabla[[i]]){
        tickT = c(iraceResults$parameters$domain[[colnames(tabla)[i]]],"NA")
      }else{
        tickT = iraceResults$parameters$domain[[colnames(tabla)[i]]]
      }
      dim[[i]] = list(
            label = colnames(tabla)[i],
            tickvals = c(1:length(unique(tabla[[i]]))),
            #ticktext = unique(tabla[[i]]),
            ticktext = tickT,
            values = factor_tab
            )
      #if the column is of type numeric
    }else if(class(tabla[[i]]) == "numeric"){
      if((as.numeric(iraceResults$parameters$domain[[colnames(tabla)[i]]][2])+1) %in% tabla[[i]]){
        minimo = iraceResults$parameters$domain[[colnames(tabla)[i]]][1]
        medio = round((max(tabla[[i]], na.rm = TRUE)/4),1)
        medio2 = round((max(tabla[[i]], na.rm = TRUE)/2),1)
        medio3 = round((max(tabla[[i]], na.rm = TRUE)*(3/4)),1)
        maximo = iraceResults$parameters$domain[[colnames(tabla)[i]]][2] +1
        dim[[i]] = list(
          range = c(iraceResults$parameters$domain[[colnames(tabla)[i]]][1],iraceResults$parameters$domain[[colnames(tabla)[i]]][2]+1),
          tickvals = c(minimo,medio,medio2,medio3,maximo),
          ticktext = c(minimo,medio,medio2,medio3,"NA"),
          values = tabla[[i]],
          label = colnames(tabla)[i]
        )
      }else{
        minimo = iraceResults$parameters$domain[[colnames(tabla)[i]]][1]
        medio = round((max(tabla[[i]], na.rm = TRUE)/4),1)
        medio2 = round((max(tabla[[i]], na.rm = TRUE)/2),1)
        medio3 = round((max(tabla[[i]], na.rm = TRUE)*(3/4)),1)
        maximo = iraceResults$parameters$domain[[colnames(tabla)[i]]][2]
        dim[[i]] = list(
          range = c(iraceResults$parameters$domain[[colnames(tabla)[i]]][1],iraceResults$parameters$domain[[colnames(tabla)[i]]][2]),
          tickvals = c(minimo,medio,medio2,medio3,maximo),
          ticktext = c(minimo,medio,medio2,medio3,maximo),
          values = tabla[[i]],
          label = colnames(tabla)[i])
      }

    }
    #other types
    else{
        dim[[i]] = list(
          range = c(min(tabla[[i]], na.rm = TRUE),max(tabla[[i]], na.rm = TRUE)),
          values = tabla[[i]],
          label = colnames(tabla)[i]
        )
      }

  }

  iteration_f <- factor(as.character(tabla$iteration), ordered = TRUE)
  levels(iteration_f) <- c(1:length(unique(tabla$iteration)))
  tabla <- cbind(tabla,iteration_f)

  #plot creation
  p <- tabla %>%
    plot_ly(width = 700, height = 600)
  p <- p %>% add_trace(type = 'parcoords',
                           line = list(color = ~iteration_f,
                                       colorscale = 'Viridis',
                                       showscale = TRUE,
                                       reversescale = TRUE,
                                       cmin = min(tabla$iteration_f),
                                       cmax = max(tabla$iteration_f)),
                           dimensions = dim
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
