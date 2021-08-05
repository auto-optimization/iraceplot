#' Box Plot Testing
#'
#' @description
#' The function will return a box plot, using the data generated in the test
#' settings coloring the best configuration in each iteration
#'
#' @template arg_irace_results
#'
#' @param type
#' String, either "all", "ibest" or "best". By default it is "all" which shows all the configurations,
#' "best" shows the configurations of the last iteration and
#' "ibest" shows the best configurations of each iteration
#' 
#' @param rpd
#' Logical (default TRUE) TRUE to plot performance as the relative percentage deviation to 
#' best results, FALSE to plot raw performance
#' 
#' @param file_name
#' String, File name to save plot (example: "~/patch/example/file_name.png")
#' 
#' @return box plot
#' @export
#'
#' @examples
#' boxplot_test(iraceResults)
#' boxplot_test(iraceResults, type = "ibest")
#' boxplot_test(iraceResults, type = "best")
boxplot_test <- function(irace_results, type = "all", rpd = TRUE, file_name = NULL) {
  # verify that test this in irace_results
  if (!("testing" %in% names(irace_results))) {
    cat("Error: irace_results does not contain the testing data")
    stop()
  }

  if (!(type %in% c("all", "best", "ibest"))) {
    cat("The type argument provided is incorrect\n")
  }
  
  if (type=="ibest" && iraceResults$scenario$testIterationElites) {
      cat("Warning: irace data does not contain iteration elites testing, changing plot type to \"best\"\n")
      type <- "best"
  }

  ids <- performance <- v_allElites <- names_col <- best_conf <- ids_f <- iteration_f <- NULL
  # the table is created with all the data from testing experiments
  experiments <- as.data.frame(irace_results$testing$experiments)

  # the experiments values are modified
  if (rpd) {
    experiments <- 100 * (experiments - apply(experiments, 1, min)) / apply(experiments, 1, min)
  }

  # all testing experiments settings
  if (type == "all") {
    v_allElites <- unlist(irace_results$allElites)
    v_allElites <- as.character(v_allElites[v_allElites %in% colnames(experiments)])
    data <- experiments[,v_allElites, drop=FALSE]
    
    # the last iteration of the elite settings
  } else if (type == "best") {
    test_elites <- irace_results$allElites[[length(irace_results$allElites)]]
    v_allElites <- as.character(test_elites[test_elites %in% colnames(experiments)])
    data <- experiments[,v_allElites, drop=FALSE]
    
    # the best settings of each iteration
  } else if (type == "ibest") {
    v_allElites <- as.character(irace_results$iterationElites)
    data <- experiments[, v_allElites, drop=FALSE]
  } else {
    cat("Error: non existent type argument\n")
  }

  names_col <- colnames(data)
  # the data is processed
  data <- reshape(data,
    varying = as.vector(colnames(data)),
    v.names = "performance",
    timevar = "ids",
    times = as.vector(colnames(data)),
    new.row.names = 1:(dim(data)[1] * dim(data)[2]),
    direction = "long"
  )

  # column iteration is added
  if (type == "all" || type == "ibest") {
    iteration <- sample(NA, size = dim(data)[1], replace = TRUE)
    data <- cbind(data, iteration)

    if (type == "all") {
      a <- 1
      for (i in 1:length(irace_results$allElites)) {
        test_elites <- irace_results$allElites[[i]][irace_results$allElites[[i]] %in% names_col]
        for (k in 1:length(test_elites)) {
          data$iteration[data$ids == names_col[a]] <- i
          a <- a + 1
        }
      }
    } else if (type == "ibest") {
      for (i in 1:length(unique(data$ids))) {
        data$iteration[data$ids == unique(data$ids)[i]] <- i
      }
    }

    data$iteration_f <- factor(data$iteration, levels = (unique(data$iteration)))
  }

  for (k in 1:length(names_col)) {
    if (!(names_col[k] == as.character(v_allElites)[k])) {
      data$ids[data$ids == names_col[k]] <- as.character(v_allElites)[k]
    }
  }

  if (type == "all" || type == "best") {
    best_conf <- sample(NA, size = dim(data)[1], replace = TRUE)
    data <- cbind(data, best_conf)
    if (type == "all") {
      for (i in 1:length(irace_results$allElites)) {
        data$best_conf[data$iteration == i & data$ids == as.character(irace_results$iterationElites[i])] <- "best" # as.character(i)
      }
    } else {
      data$best_conf[data$ids == v_allElites[1]] <- "best" # as.character(1)
    }
  }

  data$ids_f <- factor(data$ids, levels = unique(data$ids))

  # the box plot is created
  if (type == "best") {
    p <- ggplot(data, aes(x = ids_f, y = performance, color = best_conf)) +
      scale_color_hue(h = c(220, 270))
  } else if (type == "ibest") {
    p <- ggplot(data, aes(x = ids_f, y = performance, color = iteration_f)) +
      labs(subtitle = "iterations") +
      theme(plot.subtitle = element_text(hjust = 0.5))
  } else {
    p <- ggplot(data, aes(x = ids_f, y = performance, colour = best_conf)) +
      labs(subtitle = "iterations") +
      theme(
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(size = 6.4, angle = 90)
      ) +
      scale_color_hue(h = c(220, 270))
  }
  
  y_lab <- "Performance"
  if (rpd) y_lab <- "RPD performance"

  p <- p +
    geom_boxplot() +
    theme(legend.position = "none") +
    labs(x = "ID", y = y_lab)

  # each box plot is divided by iteration
  if (type == "all" || type == "ibest") {
    p <- p + facet_grid(cols = vars(data$iteration_f), scales = "free")
  }

  # If the value in file_name is added the pdf file is created
  if (!is.null(file_name)) {
    ggsave(file_name, plot = p)
    # If you do not add the value of file_name, the plot is displayed
  } else {
    p
    return(p)
  }
}
