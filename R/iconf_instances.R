#' The configurations by iteration and instance
#'
#' @description
#' A graph is created with all the settings and instance of the training data
#' @param iraceResults
#' The data generated when loading the Rdata file created by irace
#' @param distance_min
#' Logical (default FALSE) to fit through an equation of minimum percentage distance
#' between the values of each row of all configurations
#' @param fileName
#' String, A pdf will be created in the location and with the
#' assigned name (example: "~/patch/example/filename")
#'
#' @importFrom ggplot2 scale_shape_manual theme_bw scale_x_discrete
#'
#' @return plot
#' @export
#'
#' @examples
#' NULL

iconf_instances <- function(iraceResults, distance_min = FALSE, fileName = NULL){

  time <- bound <- instance <- configuration <- iteration <- nconfig <- NULL
  nconfig = 0
  experiments <- as.data.frame(iraceResults$experiments)

  if(distance_min == TRUE){
    experiments <- 100*(experiments - apply(experiments,1,min,na.rm = TRUE))/apply(experiments,1,min,na.rm = TRUE)
  }

  exp_log <- select(as.data.frame(iraceResults$experimentLog),-time,-bound)

  value <- sample(NA,size=dim(exp_log)[1],replace = TRUE)
  type <- sample(NA,size=dim(exp_log)[1],replace = TRUE)
  conf_it <- sample(NA,size=dim(exp_log)[1],replace = TRUE)
  instance_it <- sample(NA,size=dim(exp_log)[1],replace = TRUE)
  media_regular <- sample(NA,size=dim(exp_log)[1],replace = TRUE)
  media_elite <- sample(NA,size=dim(exp_log)[1],replace = TRUE)
  execution <- (1:dim(exp_log)[1])

  tabla <- cbind(exp_log,value, type, execution, conf_it, instance_it, media_regular,media_elite)

  for (i in 1:dim(experiments)[2]) {
    tabla$value[(tabla["configuration"] == i)] = mean(experiments[[i]], na.rm = TRUE)
  }

  for (j in 1:length(iraceResults$allElites)) {
    nconfig = nconfig + length(tabla$iteration[tabla$iteration == j])
    tabla$conf_it[tabla$iteration == j] = nconfig
    tabla$instance_it[tabla$iteration == j] = max(unique(tabla$instance[tabla$iteration == j]))

    if(j == length(iraceResults$allElites)){
      tabla$type[tabla$iteration == j & !(tabla$configuration %in% iraceResults$allElites[[j]])] = "regular"
      tabla$type[tabla$iteration == j & (tabla$configuration %in% iraceResults$allElites[[j]])] = "final"
      tabla$type[tabla$iteration == j & (tabla$configuration %in% iraceResults$allElites[[j]][1])] = "best"
    }else{
      tabla$type[tabla$iteration == j & !(tabla$configuration %in% iraceResults$allElites[[j]])] = "regular"
      tabla$type[(tabla$iteration == j & tabla$configuration %in% iraceResults$allElites[[j]])] = "elite"
    }
  }

  for (k in 1:length(iraceResults$allElites)) {
    tabla$media_regular[tabla$iteration == k] = mean(tabla$value[tabla$iteration == k])
    tabla$media_elite[tabla$iteration == k] = mean(tabla$value[tabla$iteration == k & (tabla$type == "elite" | tabla$type == "final" | tabla$type == "best")])
  }

  tabla$instance[1] <- as.character(tabla$instance[1])
  tabla$configuration[1] <- as.character(tabla$configuration[1])

  tabla <- tabla %>%
    mutate(text = paste0("execution: ", execution, "\n", "instance: ", instance, "\n", "configuration: ",configuration, "\n"))
  exe_factor <- factor(tabla$execution)
  levels(exe_factor) <- c(1:length(tabla$execution))
  tabla <- cbind(tabla,exe_factor)

  q <- ggplot(tabla, aes(x = exe_factor, y = value, color = instance,text=text)) +
       geom_point(aes(shape = type)) +
       facet_grid(cols = vars(tabla$instance_it),scales = "free_x", space = "free_x") +
       scale_shape_manual(values = c(11,16,18,4)) +
       theme_bw() +
       scale_color_viridis_d() +
       scale_x_discrete(breaks = c(1,unique(tabla$conf_it))) +
       labs(x = "Candidate evaluations",
            y =  "Relative deviaton",
            subtitle = "Instances evaluated") +
       theme(legend.position = "none",
             axis.text.x = element_text(angle = 90),
             axis.ticks.x = element_blank(),
             plot.subtitle = element_text(hjust = 0.5))



  p <- plotly::ggplotly(q, tooltip = "text")

  #If the value in fileName is added the pdf file is created
  if(!is.null(fileName)){
    pdf(paste0(fileName,".pdf"), width = 12)
    plot(q)
    dev.off()
    #If you do not add the value of fileName, the plot is displayed
  }else{
    q
  }
}
