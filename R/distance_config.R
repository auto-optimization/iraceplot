#' Distance between configurations
#'
#' @description
#' Calculate the difference between two settings. Calculate the difference between two configurations. The greater
#' the number, the greater the difference
#'
#' @template arg_irace_results
#'
#' @param id_configurations
#' Numeric vector, you need to put the settings you
#' want to compare, only 2 values are allowed (example: idVector = c(806,809))
#' @param t
#' Numeric, It is a percentage factor that will determine the range of difference
#' between settings (example: t = 0.05 is equivalent to 5 percent)
#'
#' @return numeric
#'
#' @examples
#' NULL
distance_config <- function(irace_results, id_configurations, t = 0.05) {

  # restrictions
  if (length(id_configurations) != 2) {
    return("You must enter two settings")
  } else if (FALSE %in% (id_configurations[1] %in% irace_results$allConfigurations[[".ID."]])) {
    return(paste("Configuration", id_configurations[1], "does not exist", sep = " "))
  } else if (FALSE %in% (id_configurations[2] %in% irace_results$allConfigurations[[".ID."]])) {
    return(paste("Configuration", id_configurations[2], "does not exist", sep = " "))
  }

  # variable assignment
  distance <- .ID. <- .PARENT. <- NULL
  datos <- select(irace_results$allConfigurations[id_configurations, ], -.ID., -.PARENT.)
  tipos <- irace_results$parameters$types

  # the sum of the distance between parameters according to their type
  for (i in 1:length(datos)) {
    # c = category
    if (tipos[[colnames(datos)[i]]] == "c") {
      if (is.na(datos[[i]][1]) && is.na(datos[[i]][2])) {
        distance <- c(distance, 0)
      } else if (is.na(datos[[i]][1]) || is.na(datos[[i]][2])) {
        distance <- c(distance, 1)
      } else if (datos[[i]][1] == datos[[i]][2]) {
        distance <- c(distance, 0)
      } else {
        distance <- c(distance, 1)
      }
      # r and i = numeric
    } else if (tipos[[colnames(datos)[i]]] == "r" || tipos[[colnames(datos)[i]]] == "i") {
      if (is.na(datos[[i]][1]) && is.na(datos[[i]][2])) {
        distance <- c(distance, 0)
      } else if (is.na(datos[[i]][1]) || is.na(datos[[i]][2])) {
        distance <- c(distance, 1)
      } else if (datos[[i]][1] == datos[[i]][2]) {
        distance <- c(distance, 0)
      } else {
        min <- irace_results$parameters$domain[[colnames(datos)[i]]][1]
        max <- irace_results$parameters$domain[[colnames(datos)[i]]][2]
        dt <- (max - min) * t
        if (abs(datos[[i]][1] - datos[[i]][2]) <= dt) {
          distance <- c(distance, 0)
        } else {
          distance <- c(distance, 1)
        }
      }
    }
  }
  # the total difference between two configurations
  return(sum(distance))
}
