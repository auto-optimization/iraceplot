#' Parameter Frequency and density Plot
#'
#' @description
#' The function will return a frequency and density plot,
#' for categorical parameters (string) one of frequency is created,
#' in case of numerical parameters it will show a histogram and its density
#'
#' @template arg_irace_results
#'
#' @param param_names
#' String vector, A set of parameters to be plotted
#' (example: param_names = c("algorithm","alpha","rho","q0","rasrank"))
#'
#' @param n
#' Numeric, It will allow going through the various parameters of 9 in 9 graphs
#' (example: if we place an n = 1, it will show us the parameters from 1 to 9,
#' in case an n = 2 it will show the parameters from 10 to 18 and so on)
#'
#' @param file_name
#' String, A pdf will be created in the location and with the assigned
#' name. By default, all the parameters will be displayed appropriately, with nine
#' graphics for each sheet. (example: "~/patch/example/file_name")
#'
#' @return Frequency and/or density plot
#' @export
#'
#' @examples
#' sampling_frequency(iraceResults)
#' sampling_frequency(iraceResults, n = 2)
#' sampling_frequency(iraceResults, param_names = c("algorithm", "alpha", "rho", "q0", "rasrank"))
sampling_frequency <- function(irace_results, param_names = NULL, n = NULL, file_name = NULL) {

  # Variable assignment
  vectorG <- tabla <- Var1 <- Freq <- ..density.. <- inicio <- fin <- max_p <- NULL
  param_names <- unlist(param_names)
  max_p <- 9

  if (length(get_parameters_names(irace_results)) > max_p & is.null(n) & is.null(param_names) & is.null(file_name)) {
    print("There are too many parameters to display. It will select relevant parameters")
    print(paste("The first", max_p, "parameters will be displayed"))
    param_names <- get_parameters_names(irace_results)[1:max_p]
  }


  if (!is.null(param_names)) {
    if ("FALSE" %in% (param_names %in% irace_results$parameters$names)) {
      return("Some wrong parameter entered")
    }
    n <- 1
    if (!is.null(n)) {
      if (n < 1 | n > ceiling(length(param_names) / max_p)) {
        return(paste("n cannot be less than 1 and greater than", ceiling(length(param_names) / max_p)))
      }
      inicio <- (max_p * n - 8)
      fin <- max_p * n
      param_names <- param_names[inicio:fin]
      param_names <- param_names[!is.na(param_names)]
      config <- irace_results$allConfigurations[param_names]
    } else {
      config <- irace_results$allConfigurations[param_names]
    }
  } else {
    if (!is.null(n)) {
      if (n < 1 | n > ceiling(length(get_parameters_names(irace_results)) / max_p)) {
        return(paste("n cannot be less than 1 and greater than", ceiling(length(get_parameters_names(irace_results)) / max_p)))
      }
      inicio <- (max_p * n - 8)
      fin <- max_p * n
      params <- irace_results$parameters$names[inicio:fin]
      params <- params[!is.na(params)]
      config <- irace_results$allConfigurations[params]
    } else {
      config <- irace_results$allConfigurations[irace_results$parameters$names]
    }
  }


  for (i in 1:length(config)) {

    # plot bars
    if (class(config[[i]]) == "character") {
      tabla <- as.data.frame(table(config[[i]]))
      p <- ggplot(data = tabla, aes(x = Var1, y = Freq)) +
        geom_bar(stat = "identity", fill = "grey", color = "black") +
        labs(x = "Values") +
        ggtitle(colnames(config)[i]) +
        theme(
          axis.title.y = element_blank(),
          axis.title.x = element_text(size = 8),
          plot.title = element_text(hjust = 0.5, size = rel(0.8), face = "bold"),
          axis.ticks.x = element_blank()
        ) +
        scale_y_continuous(n.breaks = 3)
      # the plot is saved in a list
      vectorG[i] <- list(p)
    }
    # histogram and density plot
    else if (class(config[[i]]) == "numeric") {
      tabla <- na.omit(config[[i]])
      nbreaks <- pretty(range(tabla),
        n = nclass.Sturges(tabla),
        min.n = 1
      )
      q <- ggplot(as.data.frame(tabla), aes(x = tabla)) +
        geom_histogram(aes(y = ..density..),
          breaks = nbreaks,
          color = "black", fill = "gray"
        ) +
        geom_density(color = "blue", fill = "blue", alpha = 0.2) +
        labs(x = "Values") +
        ggtitle(colnames(config)[i]) +
        theme(
          axis.title.y = element_blank(),
          axis.title.x = element_text(size = 8),
          plot.title = element_text(hjust = 0.5, size = rel(0.8), face = "bold"),
          axis.ticks.x = element_blank()
        ) +
        scale_y_continuous(n.breaks = 3)
      # the plot is saved in a list
      vectorG[i] <- list(q)
    }
  }

  # If the value in file_name is added the pdf file is created
  if (!is.null(file_name)) {
    if (length(config) == 1) {
      ggsave(file_name, plot = do.call("grid.arrange", c(vectorG, ncol = 1)))
    } else if (length(config) == 2) {
      ggsave(file_name, plot = do.call("grid.arrange", c(vectorG, ncol = 2)))
    } else if (length(config) > 2 && length(config) <= 9) {
      ggsave(file_name, plot = do.call("grid.arrange", c(vectorG, ncol = 3)))
    } else {
      pdf(paste0(file_name, ".pdf"), width = 12)
      a <- 1
      b <- 9
      for (k in 1:(ceiling(length(config) / 9))) {
        do.call("grid.arrange", c(vectorG[a:b], ncol = 3))
        a <- b + 1
        if (length(config) > (k + 1) * 9) {
          b <- (k + 1) * 9
        } else {
          b <- length(config)
        }
      }
      dev.off()
    }


    # If you do not add the value of file_name, the plot is displayed
  } else {
    do.call("grid.arrange", c(vectorG, ncol = 3))
    return(vectorG)
  }
}
