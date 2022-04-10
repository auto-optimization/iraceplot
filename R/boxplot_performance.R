#' Box Plot Performance of a set of configurations
#'
#' @description
#' The `boxplot_performance` function creates a box plot that displays the 
#' performance of a set of configurations which can be displayed by iteration. 
#' 
#' The performance data is obtained from the experiment matrix provided in the 
#' experiments argument. The configurations can be selected using the allElites
#' argument and this argument can be also used to define the iteration of each
#' elite configuration was evaluated.
#'
#' @param experiments 
#' Experiment matrix obtained from irace training or testing data. Configurations 
#' in columns and instances in rows. As in irace, column names (configurations ids) 
#' should be characters.
#' 
#' @param allElites
#' List or vector of configuration ids, (default NULL). These configurations
#' should be included in the plot. If the argument is not provided all configurations
#' in experiments are included. If allElites is a vector all configurations are
#' assumed without iteration unless argument type="ibest" is provided, in which case
#' each configuration is assumed to be from a different iteration. If allElites 
#' is a list, each element of the  list is assumed as an iteration.
#'
#' @param type
#' String, (default "all") possible values are "all", "ibest" or "best". "all" 
#' shows all the selected configurations showing iterations if the information 
#' is provided. "best" shows the elite configurations of the last iteration, note
#' that the best configuration is always assumed to be first in the vector.
#' "ibest" shows the elite configurations of each iteration, note
#' that the best configuration is always assumed to be first in the vector of 
#' each iteration.
#' 
#' @param first_is_best
#' Boolean (default TRUE) Enables the display in a different color the best configuration
#' identified as the first one in a vector. If FALSE, all configurations are shown
#' in the same color.
#' 
#' @template arg_rpd
#' 
#' @param show_points
#' Logical, (default TRUE) TRUE to plot performance points together with the box plot.
#' 
#' @param best_color 
#' String, (default ""#08bfaa"") color to display best configurations.
#' 
#' @param x_lab 
#' String, (default "Configurations") label for the x axis.
#' 
#' @template arg_filename
#' 
#' @return box plot
#' @export
#'
#' @examples
#' boxplot_performance(iraceResults$experiments, iraceResults$allElites)
#' boxplot_performance(iraceResults$experiments, iraceResults$allElites, type = "ibest")
boxplot_performance <- function(experiments, allElites= NULL, type = "all",
                                first_is_best = FALSE, rpd = TRUE, show_points=TRUE, 
                                best_color = "#08bfaa", x_lab ="Configurations", 
                                filename = NULL) {
  ids <- performance <- v_allElites <- names_col <- best_conf <- ids_f <- iteration_f <- NULL
  
  if (!(is.matrix(experiments) || is.data.frame(experiments))) {
    stop("Error: experiments must be a matrix or a data frame")
  }
  
  if (!(type %in% c("all", "ibest"))) {
    stop("Error: The type argument provided is incorrect\n")
  }
  
  if (type=="ibest" && !first_is_best) {
    cat("Note: The setting type=ibest only supports first_is_best=TRUE, ignoring this setting.\n")
    first_is_best <- TRUE
  }
  
  # Get the order of configurations
  if (is.null(allElites)) {
    cat("Note: all Elites not provided, assumming all configurations in experiments as elites.\n")
    allElites <- list()
    allElites[[1]] <- get_ranked_ids(experiments)
    if (type == "ibest") {
      stop(paste0("Error: The type argument provided (",type,") is not supported when no allElites value provided\n"))
    }
  } else if (type=="ibest" && !is.list(allElites)) {
    cat ("Note: Since type=ibest, assumming vector best configuration by iteration in allElites.\n")
    allElites <- as.list(allElites)
  }
  
  # The table is created with all the data from testing experiments
  experiments <- as.data.frame(experiments)
  
  # The experiments values are modified
  if (rpd) {
    experiments <- calculate_rpd(experiments)
  }
  
  # Generate iteration and final elite vector
  iterationElites <- finalElites <- allElites
  if (is.list(allElites)) 
    iterationElites <- unlist(lapply(allElites, function(x) x[1]))
  
  # Select experiments that will be used
  if (type == "all") {
    v_allElites <- as.character(unique(unlist(allElites)))
    if (!all(v_allElites %in% colnames(experiments))) 
      stop("Error: Missing elite performance in experiments matrix")
  } else if (type == "ibest") {
    if (!all(iterationElites %in% colnames(experiments)))
      stop("Error: Missing iteration elites performance in experiments matrix")
    v_allElites <- as.character(iterationElites)
  } else {
    stop("Error: non existent type argument")
  }
  data <- as.data.frame(experiments[,v_allElites, drop=FALSE])
  names_col <- colnames(data)
  
  # Get the experiment data together with the iterations
  if (is.list(allElites)) {
    fdata <- NULL
    if (type == "all") {
      for (i in 1:length(allElites)) {
        d <- data[, as.character(allElites[[i]]), drop=FALSE]
        d <- reshape(d,
                     varying = as.vector(colnames(d)),
                     v.names = "performance",
                     timevar = "ids",
                     times = as.vector(colnames(d)),
                     new.row.names = 1:(nrow(d) * ncol(d)),
                     direction = "long"
        )
        d <- d[!is.na(d[,"performance"]),]
        d[,"iteration"] <- i
        fdata <- rbind(fdata, d)
      }
    } else if (type == "ibest") {
      for (i in 1:length(iterationElites)) {
        d <- data[, as.character(iterationElites[i]), drop=FALSE]
        d <- reshape(d,
                     varying = as.vector(colnames(d)),
                     v.names = "performance",
                     timevar = "ids",
                     times = as.vector(colnames(d)),
                     new.row.names = 1:(nrow(d) * ncol(d)),
                     direction = "long"
        )
        d <- d[!is.na(d[,"performance"]),]
        d[,"iteration"] <- i
        fdata <- rbind(fdata, d)
      }
    }
    data <- fdata
    data$iteration_f <- factor(data$iteration, levels = (unique(data$iteration)))
  } else {
    data <- reshape(data,
                    varying = as.character(colnames(data)),
                    v.names = "performance",
                    timevar = "ids",
                    times = as.character(colnames(data)),
                    new.row.names = 1:(nrow(data) * ncol(data)),
                    direction = "long"
    )
    data <- data[!is.na(data[,"performance"]),]
    data$iteration <- 0
  }
  
  # Marcar las mejores configuraciones
  if (type == "all") {
    best_conf <- rep("none", nrow(data))
    data <- cbind(data, best_conf)
    if (first_is_best) {
      if (is.list(allElites)) {
        for (i in 1:length(allElites)) {
          data$best_conf[data$iteration == i & data$ids == as.character(iterationElites[i])] <- "best" 
        }
      } else {
        data$best_conf[data$ids == as.character(iterationElites[1])] <- "best" 
      }
    } 
  } 
  
  data$ids_f <- factor(data$ids, levels = unique(data$ids))
  
  # the box plot is created
  if (type == "ibest") {
    # type="ibest"
    p <- ggplot(data, aes(x = ids_f, y = performance, color = iteration_f)) 
    p <- p + labs(subtitle = "Iterations") +
             theme(plot.subtitle = element_text(hjust = 0.5))
  } else {
    # type="all"
    if (first_is_best) {
      p <- ggplot(data, aes(x = ids_f, y = performance, colour = best_conf)) 
    } else {
      if (is.list(allElites)){
        p <- ggplot(data, aes(x = ids_f, y = performance, colour = iteration_f)) 
      } else {
        p <- ggplot(data, aes(x = ids_f, y = performance, colour = ids_f)) 
      }
    }
    if (is.list(allElites)) {
      p <- p + labs(subtitle = "Iterations")
      p <- p + theme(plot.subtitle = element_text(hjust = 0.5),
                     axis.text.x = element_text(size = 6.4, angle = 90)) 
    } else {
      p <- p + theme(plot.subtitle = element_text(hjust = 0.5)) 
    }
    
    if (first_is_best) 
      p <- p +  scale_color_manual(values=c("#08bfaa", "#999999"))
          #scale_color_hue(h = c(220, 270))
    
    
  }
  
  y_lab <- "Performance"
  if (rpd) y_lab <- "RPD performance"
  
  p <- p +
    geom_boxplot() +
    theme(legend.position = "none") +
    labs(x = x_lab , y = y_lab)
  
  if (show_points) 
    p <- p + geom_jitter(shape = 16, position = position_jitter(0.2), alpha=0.2, na.rm = TRUE) 
  
  # each box plot is divided by iteration
  if (is.list(allElites)) {
    p <- p + facet_grid(cols = vars(data$iteration_f), scales = "free")
  }
  
  # If the value in filename is added the pdf file is created
  if (!is.null(filename)) {
    ggsave(filename, plot = p)
    # If you do not add the value of filename, the plot is displayed
  } else {
    p
    return(p)
  }
}

get_ranked_ids <- function(experiments){
  allElites <- c()
  naExp <- sort(colSums(!is.na(experiments)), decreasing = TRUE)
  for (r in unique(naExp)) {
    confId <- names(naExp[naExp == r])
    confMean <- colMeans(experiments[, confId, drop=FALSE], na.rm = TRUE)
    allElites <- c(allElites, names(sort(confMean)))
  }
  return(allElites)
}

calculate_rpd <- function(x)
{
  100 * (x - apply(x, 1, min, na.rm = TRUE)) / apply(x, 1, min, na.rm = TRUE)
}
