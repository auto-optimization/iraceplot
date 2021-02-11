#' Box Plot Testing
#'
#' @description
#' The function will return a box plot, using the data generated in the test
#' settings coloring the best configuration in each iteration
#'
#' @param iraceResults
#' The data generated when loading the Rdata file created by irace
#' @param type
#' String, either "all", "best" or "last". By default it is "all" which shows all the configurations,
#' "best" shows the best configurations of each iteration and
#' "last" shows the configurations of the last iteration
#' @param distance_min
#' Logical (default FALSE) to fit through an equation of minimum percentage distance between
#' the values of each row of all configurations
#' @param fileName
#' String, A pdf will be created in the location and with the assigned
#' name (example: "~/patch/example/filename")
#' @return box plot
#' @export
#'
#' @examples
#' NULL

iboxplot_test <- function(iraceResults, type = "all", distance_min = FALSE ,fileName = NULL){

  # verify that test this in iraceResults
  if(!("testing" %in% names(iraceResults))){
    return("iraceResults does not contain the testing element")
  }

  ids <- performance <- v_allElites <- names_col <- best_conf <- ids_f <- iteration_f <- NULL
  # the table is created with all the data from testing experiments
  tabla <- as.data.frame(iraceResults$testing$experiments)

  # the table values are modified
  if(distance_min == TRUE){
    tabla <- 100*(tabla - apply(tabla,1,min))/apply(tabla,1,min)
  }

  # all testing experiments settings
  if(type == "all"){
    for (j in 1:length(iraceResults$allElites)) {
      v_allElites <- c(v_allElites,iraceResults$allElites[[j]])
    }
    datos <- tabla[as.character(v_allElites)]

  # the last iteration of the elite settings
  }else if(type == "last"){
    num_it <- length(iraceResults$allElites)
    v_allElites <- as.character(iraceResults$allElites[[num_it]])
    datos <- tabla[v_allElites]

  # the best settings of each iteration
  }else if(type == "best"){
    v_allElites <- as.character(iraceResults$iterationElites)
    datos <- tabla[v_allElites]
  }else{
    return("non existent type")
  }

  names_col = colnames(datos)
  # the data is processed
  datos <- reshape(datos,
                  varying = as.vector(colnames(datos)),
                  v.names = "performance",
                  timevar = "ids",
                  times = as.vector(colnames(datos)),
                  new.row.names = 1:(dim(datos)[1]*dim(datos)[2]),
                  direction = "long")

  # column iteration is added
  if(type == "all" || type == "best"){

      iteration <- sample(NA,size=dim(datos)[1],replace = TRUE)
      datos <- cbind(datos,iteration)

      if(type == "all"){

        a = 1
        for (i in 1:length(iraceResults$allElites)) {
          for (k in 1:length(iraceResults$allElites[[i]])) {
            datos$iteration[datos$ids == names_col[a]] <- i
            a = a+1
          }
        }

      }else if(type == "best"){
        for(i in 1:length(unique(datos$ids))){
          datos$iteration[datos$ids == unique(datos$ids)[i]] <- i
        }
      }

      datos$iteration_f = factor(datos$iteration,levels = (unique(datos$iteration)))
  }

  for(k in 1:length(names_col)){
    if(!(names_col[k] == as.character(v_allElites)[k])){
      datos$ids[datos$ids == names_col[k]] <- as.character(v_allElites)[k]
    }
  }

  if(type == "all" || type == "last"){
    best_conf <- sample(NA,size=dim(datos)[1],replace = TRUE)
    datos <- cbind(datos,best_conf)
    if(type == "all"){
      for (i in 1:length(iraceResults$allElites)) {
        datos$best_conf[datos$iteration == i & datos$ids == as.character(iraceResults$iterationElites[i])] <- as.character(i)
      }
    }else{
      datos$best_conf[datos$ids == v_allElites[1]] <- as.character(1)
    }
  }

  datos$ids_f = factor(datos$ids, levels = unique(datos$ids))

  # the box plot is created
  if(type == "last"){
    p <- ggplot(datos, aes(x=ids_f,y=performance, color = best_conf))

  }else if(type == "best"){
    p <- ggplot(datos, aes(x=ids_f,y=performance, color = iteration_f)) +
      labs(subtitle = "iterations") +
      theme(plot.subtitle = element_text(hjust = 0.5))

  }else{
    p <- ggplot(datos, aes(x=ids_f,y=performance, colour = best_conf)) +
      labs(subtitle = "iterations")+
      theme(plot.subtitle = element_text(hjust = 0.5),
            axis.text.x = element_text(size = 6.4))

  }

   p <- p +
    geom_boxplot() +
    theme(legend.position="none") +
    labs(x="IDs")

  #each box plot is divided by iteration
  if(type == "all"){
      p <- p + facet_grid(cols = vars(datos$iteration_f), scales = "free")
  }else if(type == "best"){
    p <- p + facet_grid(cols = vars(datos$iteration_f), scales = "free")
  }

  #If the value in fileName is added the pdf file is created
  if(!is.null(fileName)){
    pdf(paste0(fileName,".pdf"), width = 12)
    plot(p)
    dev.off()
    #If you do not add the value of fileName, the plot is displayed
  }else{
    p
  }

}
