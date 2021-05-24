#' The configurations by iteration and instance
#'
#' @description
#' A graph is created with all the settings and instance of the training data
#'
#' @template arg_irace_results
#' 
#' @param rpd
#' Logical (default TRUE) to fit through an equation of minimum percentage distance
#' between the values of each row of all configurations
#' @param file_name
#' String, A pdf will be created in the location and with the
#' assigned name (example: "~/patch/example/file_name")
#'
#' @importFrom ggplot2 scale_shape_manual theme_bw scale_x_discrete scale_color_manual scale_size_manual scale_alpha_manual
#' @importFrom grDevices rainbow
#'
#' @return plot
#' @export
#'
#' @examples
#' #configurations_display(iraceResults)

configurations_display <- function(irace_results, rpd = TRUE, file_name = NULL){

  #variable assignment
  time <- bound <- instance <- configuration <- iteration <- nconfig <- cont_exe <- NULL
  nconfig = 0
  experiments <- as.data.frame(irace_results$experiments)

  # the table values are modified
  if(rpd == TRUE){
    experiments <- (experiments - apply(experiments,1,min,na.rm = TRUE))/apply(experiments,1,min,na.rm = TRUE)
  }

  #variable assignment
  exp_log <- select(as.data.frame(irace_results$experimentLog),-time,-bound)
  value <- sample(NA,size=dim(exp_log)[1],replace = TRUE)
  execution <- sample(NA,size=dim(exp_log)[1],replace = TRUE)
  tabla <- cbind(exp_log,value, execution)

  #the values of each configuration are added to the table
  cont_exe = 0
  for (i in 1:dim(exp_log)[1]) {
    for (j in 1:dim(irace_results$experiments)[1]) {
      if(!is.na(experiments[[tabla$configuration[i]]][j])){

        if(is.na(tabla$value[i])){
          cont_exe = cont_exe+1
          tabla$value[i] = experiments[[tabla$configuration[i]]][j]
          tabla$execution[i] = cont_exe
        }else{
          cont_exe = cont_exe+1
          add <- tabla[i,]
          add$value = experiments[[tabla$configuration[i]]][j]
          add$execution = cont_exe
          tabla <- rbind(tabla,add)
        }
      }
    }
  }

  #new columns are created and added to the table
  type <- sample(NA,size=dim(tabla)[1],replace = TRUE)
  conf_it <- sample(NA,size=dim(tabla)[1],replace = TRUE)
  instance_it <- sample(NA,size=dim(tabla)[1],replace = TRUE)
  media_regular <- sample(NA,size=dim(tabla)[1],replace = TRUE)
  media_elite <- sample(NA,size=dim(tabla)[1],replace = TRUE)
  regular_color <- sample("median iteration",size=dim(tabla)[1],replace = TRUE)
  elite_color <- sample("median elites",size=dim(tabla)[1],replace = TRUE)
  tabla <- cbind(tabla, type, conf_it, instance_it, media_regular, media_elite, regular_color,elite_color)
  tabla <- tabla[order(tabla$execution),]

  #the data is added to the conf_it, instance_it and type columns
  for (j in 1:length(irace_results$allElites)) {
    nconfig = max(tabla$execution[tabla$iteration == j])
    tabla$conf_it[tabla$iteration == j] = nconfig
    tabla$instance_it[tabla$iteration == j] = max(unique(tabla$instance[tabla$iteration == j]))

    if(j == length(irace_results$allElites)){
      tabla$type[tabla$iteration == j & !(tabla$configuration %in% irace_results$allElites[[j]])] = "regular config."
      tabla$type[tabla$iteration == j & (tabla$configuration %in% irace_results$allElites[[j]])] = "final elite config."
      tabla$type[tabla$iteration == j & (tabla$configuration %in% irace_results$allElites[[j]][1])] = "best found config."
    }else{
      tabla$type[tabla$iteration == j & !(tabla$configuration %in% irace_results$allElites[[j]])] = "regular config."
      tabla$type[tabla$iteration == j & tabla$configuration %in% irace_results$allElites[[j]]] = "elite config."
    }
  }

  #The mean values ​​are calculated in the configurations by iteration
  for (k in 1:length(irace_results$allElites)) {
    tabla$media_regular[tabla$iteration == k] = mean(tabla$value[tabla$iteration == k])
    tabla$media_elite[tabla$iteration == k] = mean(tabla$value[tabla$iteration == k & (tabla$type == "elite config." | tabla$type == "final elite config." | tabla$type == "best found config.")])
  }

  #Instance and configuration columns are converted to character
  tabla$instance[1] <- as.character(tabla$instance[1])
  tabla$configuration[1] <- as.character(tabla$configuration[1])

  #the text column is generated
  tabla <- tabla %>%
    mutate(text = paste0("execution: ", execution, "\n", "instance: ", instance, "\n", "configuration: ",configuration, "\n"))

  #the execution column is passed to factor and added to the table
  exe_factor <- factor(tabla$execution)
  levels(exe_factor) <- tabla$execution
  tabla <- cbind(tabla,exe_factor)

  #point plot creation
  q <- ggplot(tabla, aes(x = exe_factor,y = value,color = instance,text=text)) +
    geom_point(aes(shape = type, size = type, alpha = type)) +
    facet_grid(cols = vars(tabla$instance_it),scales = "free_x", space = "free_x") +
    scale_shape_manual(values = c(22,21,24,4)) +
    scale_color_manual(values = c(rainbow(length(unique(tabla$instance))),"red","orange"), breaks = c("median elites", "median iteration")) +
    scale_size_manual(values = c(2,2,2,0.5)) +
    scale_alpha_manual(values = c(0.8,0.6,1,0.2)) +
    scale_x_discrete(breaks = c(1,unique(tabla$conf_it))) +
    labs(x = "Candidate evaluations",
         y =  "RPD",
         subtitle = "Instances evaluated") +
    theme(axis.text.x = element_text(angle = 90),
          axis.ticks.x = element_blank(),
          plot.subtitle = element_text(hjust = 0.5),
          strip.text.x = element_text(size = 8),
          legend.position = "right",
          legend.title = element_blank()) +
    geom_point(mapping = aes(y = media_elite, color = elite_color),size = 0.1,) +
    geom_point(mapping = aes(y = media_regular, color = regular_color),size = 0.1)


  #The graph is transformed to plotly
  p <- plotly::ggplotly(q, tooltip = "text")

  #If the value in file_name is added the pdf file is created
  if(!is.null(file_name)){
    ggsave(file_name,plot = q)
    #If you do not add the value of file_name, the plot is displayed
  }else{
    q
    return(q)
  }
}
