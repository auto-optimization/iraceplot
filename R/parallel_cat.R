#' Parallel Cordinate Category
#'
#' @description
#' The parallel_coord_category function will return a graph of categorical
#' parallel coordinates allowing the analysis of the set of parameters
#' allowing the visualization of the data and the filtering by iteration
#'
#' @template arg_irace_results
#'
#' @param id_configuration
#' Numeric vector, you need to put the configurations you want to analyze
#' (example: id_configuration = c(20,50,100,300,500,600,700))
#'
#' @param param_names
#' String vector, you need to put the parameters you want to analyze
#' (example: param_names = c("algorithm","alpha","rho","q0","rasrank"))
#'
#' @param iterations
#' NUmeric vector, you need to put the iterations you want to analyze
#' (example: iterations = c(1,4,5))
#'
#' @param pdf_all_parameters
#' logical (default FALSE), If I want to create a pdf with all the parameters,
#' I must put TRUE, otherwise it will be created only with the default
#' parameters (15 or less) or those entered.
#'
#' @param file_name
#' A pdf will be created in the location and with
#' the assigned name (example: "~/patch/example/file_name")
#'
#' @return parallel coordinate category plot
#' @export
#'
#' @examples
#' parallel_cat(iraceResults)
#' parallel_cat(iraceResults,id_configuration = c(20,50,100,300,500,600,700))
#' parallel_cat(iraceResults, param_names = c("algorithm","alpha","rho","q0","rasrank"))
#' parallel_cat(iraceResults, iterations =  c(1,4,6))

parallel_cat <- function(irace_results, id_configuration = NULL, param_names = NULL, iterations = NULL,pdf_all_parameters = FALSE,file_name = NULL){

  #Variable assignment
  memo  <- configuration <- dim <- tickV <- vectorP <- x <- y <- id <- freq <- NULL
  id_configuration <- unlist(id_configuration)
  param_names <- unlist(param_names)


  if(!is.null(iterations)){
    it <- c(1:length(irace_results$allElites))
    if(FALSE %in% (iterations %in% it)){
      return("The interactions entered are outside the possible range")
    }
  }

  #verify that param_names is other than null
  if(!is.null(param_names)){
    #verify that param_names contain the data entered
    if( "FALSE" %in% names(table(param_names %in% irace_results$parameters$names))){
      return("Some wrong parameter entered")
      #verify that param_names contain more than one parameter
    }else if(length(param_names) < 2){
      return("You must enter at least two parameters")
    }

  }

  if(!is.null(id_configuration)){

    # Verify that the entered id are within the possible range
    if(length(id_configuration[id_configuration < 1]) >= 1 || length(id_configuration[id_configuration > dim(irace_results$allConfigurations)[1]]) >= 1){
      return("IDs entered are outside the range of settings")
    }

    # Verify that the id entered are more than 1 or less than the possible total
    if(length(id_configuration) <= 1 || length(id_configuration) > dim(irace_results$allConfigurations)[1] ){
      return("You must enter more than one id")
    }

    # the table to be used and the filter with the iterations and configuration is created
    selection <- irace_results$allConfigurations[, ".ID."] %in% id_configuration
    tabla <- irace_results$allConfigurations[selection,]
    filtro <- unique(irace_results$experimentLog[,c("iteration","configuration")])
    selection2 <- filtro[, "configuration"] %in% id_configuration
    filtro <- filtro[selection2,]
    # table is created with all settings
  }else{
    tabla <-irace_results$allConfigurations
    filtro <- unique(irace_results$experimentLog[,c("iteration","configuration")])
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
      #The iteration is assigned to the configuration
    }else{
      tabla$iteration[tabla$.ID. == filtro$configuration[i]] = filtro$iteration[i]
    }
    memo = filtro$configuration[i]
  }

  # Column .ID. and .PARENT. are removed
  tabla <- tabla[, !(names(tabla) %in% c(".ID.",".PARENT."))]
  if(is.null(param_names) & length(get_parameters_names(irace_results)) > 15 & pdf_all_parameters == FALSE){
    param_names = get_parameters_names(irace_results)[1:15]
  }
  if(!is.null(param_names)){
    param_names <- c(param_names,"iteration")
    tabla <- tabla[, (names(tabla) %in% param_names )]
  }

  # for(k in 1:(length(tabla))){
  #    tabla[[k]][1] = as.character(tabla[[k]][1])
  # }

  if(!is.null(iterations)){
    tabla <- tabla[tabla$iteration %in% iterations,]
  }

  for (i in 1:(dim(tabla)[2]-1)) {
    if(class(irace_results$parameters$domain[colnames(tabla[i])][[1]]) == "numeric"){
      valor = paste(irace_results$parameters$domain[colnames(tabla[i])][[1]][1],"-",irace_results$parameters$domain[colnames(tabla[i])][[1]][2])
      tabla[i][!is.na(tabla[i])] <- valor
      tabla[i][is.na(tabla[i])] <- "NA"
    }
    if(TRUE %in% is.na(tabla[i])){
      tabla[i][is.na(tabla[i])] <- "NA"
    }
  }

  iteration_f <- factor(as.character(tabla$iteration), ordered = TRUE)
  levels(iteration_f) <- c(1:length(unique(tabla$iteration)))
  tabla$iteration <- iteration_f

  tabla$iteration[1] <- as.character(tabla$iteration[1])

  tabla <- tabla %>% group_by(tabla[1:dim(tabla)[2]]) %>% summarise(freq = n()) #%>% filter(freq > 1)

  tabla <- gather_set_data(tabla,1:(dim(tabla)[2]-1))

  tabla <- tabla[tabla$x != "iteration",]

  if(!is.null(file_name) & pdf_all_parameters == TRUE & length(get_parameters_names(irace_results))>15){
    inicio = 1
    final = 15
    for (i in 1:ceiling(length(get_parameters_names(irace_results))/15)) {
      n_parameters = length(get_parameters_names(irace_results))
      params = get_parameters_names(irace_results)[inicio:final]
      params = params[!is.na(params)]
      q <- parallel_cat(irace_results, id_configuration, param_names = params , iterations)
      vectorP[i] <- list(q)
      inicio = final + 1
      final = (i+1)*15
    }

  }
  p <- ggplot(tabla, aes(x, id = id, split = y, value = freq)) +
    geom_parallel_sets(aes(fill = iteration), alpha = 0.8, axis.width = 0.2) +
    geom_parallel_sets_axes(axis.width = 0.5, alpha = 0.4) +
    geom_parallel_sets_labels(colour = "black",angle = 90,size = 3) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, size = 6),
          axis.title.x = element_blank())

  #If the value in file_name is added the pdf file is created
  if(!is.null(file_name)){

    pdf(paste0(file_name,".pdf"))
    for (i in 1:length(vectorP)) {
      plot(vectorP[[i]])
    }
    dev.off()
    #If you do not add the value of file_name, the plot is displayed
  }else{
    p
  }
  return(p)
}

