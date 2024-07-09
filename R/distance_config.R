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
  
  if (length(id_configuration) != 1L) {
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
  parameters <- irace_results$scenario$parameters
  types <- parameters$types
  
  distance <- rep(0, nrow(others))
  
  # Categorical parameters
  cat_par <- names(types[types %in% c("c", "o")])
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
  num_par <- names(types[types %in% c("i", "r")])
  # calculate distance
  threshold <- t * abs(sapply(parameters$domains[num_par], function(d) d[2L]-d[1L]))

  for (pname in num_par) {
    if(is.na(config[,pname])) {
      distance <- distance + as.numeric(!is.na(others[,pname]))
    } else {
      are_na <- is.na(others[,pname])
      distance <- distance + as.numeric(are_na)
      distance[!are_na] <- distance[!are_na] + as.numeric(abs(others[!are_na,pname] - config[,pname]) > threshold[[pname]])
    }
  }
  distance
}
