#' Distance between configurations
#'
#' Calculate the difference between a configuration and the others in the irace data.
#'
#' @template arg_irace_results
#'
#' @param id_configuration
#' Numeric, configuration id which should be compared to others 
#' (example: id_configuration = c(806,809))
#' 
#' @param t
#' Numeric, (default 0.05) threshold that defines the distance (percentage of the domain size) 
#' to consider a parameter value equal to other.
#'
#' @return numeric
#'
#' @examples
#' NULL
distance_config <- function(irace_results, id_configuration, t = 0.05) {
  
  if (length(id_configuration) != 1) {
    stop("Error: You must enter one configuration id\n")
  } else if (id_configuration %not_in% irace_results$allConfigurations[[".ID."]]) {
    stop(paste("Error: Configuration", id_configuration[1], "does not exist\n", sep = " "))
  } 
  
  if (t < 0 || t > 1){
    stop("Error: threshold t should be in [0,1]\n")
  }
  
  distance <- .ID. <- .PARENT. <- NULL
  
  #Get configurations
  config <- select(irace_results$allConfigurations[id_configuration, ], -.ID., -.PARENT.)
  others <- select(irace_results$allConfigurations[irace_results$allConfigurations$.ID. %not_in% id_configuration, ], -.ID., -.PARENT.)
  tipos <- irace_results$parameters$types
  
  distance <- rep(0, nrow(others))
  
  # Categorical parameters
  cat_par <- names(tipos[tipos %in% c("c", "o")])
  for (pname in cat_par) {
    if(is.na(config[,pname])) {
      distance <- distance + as.numeric(!is.na(others[,pname]))
    } else {
      are_na <- is.na(others[,pname])
      distance <- distance + as.numeric(are_na)
      distance[!are_na] <- distance[!are_na] + as.numeric(others[!are_na, pname] != config[,pname])
    }
  }
  
  # Numerical parameters
  num_par <- names(tipos[tipos %in% c("i", "r", "i,log", "r,log")])
  # calculate distance
  threshold <- lapply(irace_results$parameters$domain[num_par], function(d) return(abs((d[2]-d[1])*t)))
  for (pname in num_par) {
    if(is.na(config[,pname])) {
      distance <- distance + as.numeric(!is.na(others[,pname]))
    } else {
      are_na <- is.na(others[,pname])
      distance <- distance + as.numeric(are_na)
      distance[!are_na] <- distance[!are_na] + as.numeric(abs(others[!are_na,pname] - config[,pname]) > threshold[[pname]])
    }
  }
  
  return(distance)
}
