#' Sampling distance Plot
#'
#' @description
#' The `sampling_distance` function creates a plot that displays the mean of the 
#' distance between the configurations that were executed in each iteration.
#' 
#' For categorical parameters the distance is calculated as the hamming distance, 
#' for numerical parameters a equality interval is defined by a threshold
#' specified by argument t and hamming distance is calculated using this interval. 
#'
#' @template arg_irace_results
#' @param type
#' String, (default "boxplot") Type of plot to be produces, either "line", "boxplot" 
#' or "both". The "boxplot" setting shows a boxplot of the mean distance of all
#' configurations, "line" shows the mean distance of the solution population in each 
#' iteration, "both" shows both plots.
#' 
#' @param t
#' Numeric, (default 0.05) percentage factor that will determine a distance to 
#' define equal numerical parameter values. If the numerical parameter values to be 
#' compared are v1 and v2 they are considered equal if `|v1-v2| <= |ub-lb|*t`. 
#' 
#' @template arg_filename
#'
#' @return line or box plot
#'
#' @examples
#' iraceResults <- read_logfile(system.file(package="irace", "exdata",
#'                                          "irace-acotsp.Rdata", mustWork = TRUE))
#' sampling_distance(iraceResults)
#' \donttest{
#' sampling_distance(iraceResults, type = "boxplot", t=0.07)
#' }
#' @export
sampling_distance <- function(irace_results, type = c("boxplot", "line", "both"), t = 0.05, filename = NULL)
{
  type <- match.arg(type)

  # variable assignment
  ids <- media <- allconf <- valor <- iterations <- tabla_box <- iteration <- vectorP <- NULL
  allconf <- irace_results$allConfigurations
  n_param <- length(allconf) - 2
  niterations <- length(irace_results$allElites)
  
  all.distance <- matrix(0, ncol=nrow(allconf), nrow=nrow(allconf))
  a <- 1:nrow(allconf)
  for (confid in allconf$.ID.) {
    all.distance[confid,a[-confid]] <- distance_config(irace_results, id_configuration=confid, t=t)
  }

  # The value of the distance between each configuration is created
  for (i in 1:niterations) {
    ids <- unique(irace_results$experimentLog[irace_results$experimentLog[,"iteration"] %in% i, "configuration"])
    iterations <- c(iterations, i)

    dd <- all.distance[ids,ids]
    distance <- dd[upper.tri(dd)]
    it <- i
    datos <- data.frame(it, distance)
    tabla_box <- rbind(tabla_box, datos)
    tabla_box[,"it"] <- as.character(tabla_box[,"it"])
    media <-  c(media, mean(distance))
  }
  
  tabla_box[,"it"] <- factor(tabla_box[,"it"] , levels=as.character(1:niterations))

  # A graph of points and lines is created
  if (type == "line" | type == "both") {
    tabla <- as.data.frame(cbind(iterations, media))
    colnames(tabla) <- c("iterations", "media")
    p <- ggplot(tabla, aes(x = iterations, y = media)) +
      geom_point() +
      geom_line() +
      scale_y_continuous(
        limits = c(0, n_param),
        breaks = seq(0, n_param, 2)
      ) +
      scale_x_continuous(
        limits = c(1, niterations),
        breaks = seq(1, niterations, by=1)
      ) +
      scale_color_viridis_c() +
      labs(y = "Distance", x = "iteration", color = "IT.") + 
      theme (legend.position = "none")
    vectorP[1] <- list(p)

    # A box plot is created
  }
  if (type == "boxplot" | type == "both") {
    p <- ggplot(tabla_box, aes(x = it, y = distance, group = it, color = it)) +
      geom_boxplot(na.rm = TRUE) +
      scale_color_viridis_d() +
      scale_y_continuous(
        limits = c(0, n_param),
        breaks = seq(0, n_param, 2)
      ) +
      labs(x = "iteration", y = "Distance", color = "IT.") +
      theme (legend.position = "none")
    vectorP[2] <- list(p)
  }

  # If the value in filename is added the pdf file is created
  if (!is.null(filename)) {
    if (type == "both") {
      if (!has_file_extension(filename, "pdf"))
        stop("Unknown filetype: ", filename)
      pdf(filename, width = 12)
      do.call("grid.arrange", c(vectorP[1], ncol = 1))
      do.call("grid.arrange", c(vectorP[2], ncol = 1))
      dev.off()
    } else {
      ggsave(filename, plot = p)
    }

    # If you do not add the value of filename, the plot is displayed
  } else {
    if (type == "both") {
      do.call("grid.arrange", c(vectorP, nrow = 2))
    } else {
      p
      return(p)
    }
  }
}
