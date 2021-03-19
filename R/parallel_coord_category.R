parallel_coord_category <- function(iraceResults, fileName = NULL){

  for(k in 1:(length(tabla))){
    tabla[[k]][1] = as.character(tabla[[k]][1])
  }

  tabla <- tabla %>% group_by(algorithm,localsearch,alpha,beta,rho,ants,nnls,q0,dlb,rasrank,elitistants) %>% summarise(freq = n()) %>% filter(freq > 1)

  tabla <- gather_set_data(tabla,1:11)

  ggplot(tabla, aes(x, id = id, split = y, value = freq)) +
    geom_parallel_sets(aes(fill = algorithm), alpha = 0.3, axis.width = 0.2) +
    geom_parallel_sets_axes(axis.width = 0.2) +
    geom_parallel_sets_labels(colour = "black",angle = 360,size = 3) +
    theme_bw()
}
