# Categorical model generation
#
# @description
# 
# Rebuild the probabilities of the sampling models used in irace to generate
# configurations in each iteration.
# 
# @template arg_irace_results
#
# @param param_name
# String, parameter to be included in the plot (example: param_name = "algorithm"))
# 
# @return data frame with columns "iteration", "elite", "parameter", "value", "prob"
# 
# @examples
# NULL
# FIXME: Add examples
getCategoricalModel <- function(irace_results, param_name) 
{
  if (!(irace_results$parameters$types[[param_name]] %in% c("c"))) {
    stop("Error: Parameter is not categorical\n")
  }
  iterations <- length(irace_results$allElites)
  domain <- irace_results$parameters$domain[[param_name]] 
  n_val <- length(irace_results$parameters$domain[[param_name]])
  prob <- rep((1 / n_val), n_val)
    
  # Get elite data by iteration
  all_elites <- list()
  for (i in 1:iterations){
    all_elites[[i]] <- irace_results$allConfigurations[irace_results$allElites[[i]], c(".ID.", ".PARENT.",param_name) ]
  }
  
  total_iterations <- floor(2 + log2(irace_results$parameters$nbVariable))

  X <- NULL
  models <- list()
  for (i in 1:(iterations-1)) {
    models[[i]] <- list() 
    total_iterations <- max(total_iterations, i+1)
    for (elite in 1:length(irace_results$allElites[[i]])) {
      cid <- all_elites[[i]][elite, ".ID."]
      parent <- all_elites[[i]][elite, ".PARENT."]
      if (i==1) {
        cprob <- prob 
      } else {
        if (as.character(cid) %in% names(models[[i-1]]))
          cprob <- models[[i-1]][[as.character(cid)]] 
        else
          cprob <- models[[i-1]][[as.character(parent)]] 
      }
      
      cprob <- cprob * (1 - (i / total_iterations))
      index <- which (domain == all_elites[[i]][elite, param_name])
      cprob[index] <- (cprob[index] + (i / total_iterations))
      if (irace_results$scenario$elitist) {
        cprob <- cprob / sum(cprob)
        probmax <- 0.2^(1 / irace_results$parameters$nbVariable)
        cprob <- pmin(cprob, probmax)
      }
      # Normalize probabilities.
      cprob <- cprob / sum(cprob)
      models[[i]][[as.character(cid)]] <- cprob
      for (v in 1:length(domain)) 
        X <- rbind(X, cbind(i, elite, param_name, domain[v], as.character(cprob[v])))
    }
  }
  X <- as.data.frame(X, stringsAsFactors=FALSE)
  colnames(X) <-c("iteration", "elite", "parameter", "value", "prob")
  X[, "prob"] <- as.numeric(X[, "prob"])
  return(X)
}

# Numerical model generation
#
# @description
# 
# Rebuild the sampling distribution parameters of the models used by irace to sampling configurations during the configuration process.
# 
# @template arg_irace_results
#
# @param param_name
# String, parameter to be included in the plot (example: param_name = "algorithm"))
# 
# @return data frame with columns "iteration", "elite", "parameter", "mean", "sd"
# 
# @examples
# NULL
# FIXME: Add examples.
getNumericalModel <- function(irace_results, param_name) 
{
  if (!(irace_results$parameters$types[[param_name]] %in% c("i", "r", "i,log", "r,log"))) {
    stop("Parameter is not numerical")
  }
  
  iterations <- length(irace_results$allElites)
  domain <- irace_results$parameters$domain
  n_par <- irace_results$parameters$nbVariable
  

  # Get elite data by iteration
  all_elites <- list()
  for (i in 1:iterations){
    all_elites[[i]] <- irace_results$allConfigurations[irace_results$allElites[[i]], param_name]
  }
  
  # Get initial model standard deviation 
  s <- (domain[[param_name]][2] - domain[[param_name]][1])/2
  
  X <- NULL
  for (i in 1:(iterations-1)) {
    # Get not elite configurations executed in an iteration
    it_conf <- unique(irace_results$experimentLog[irace_results$experimentLog[,"iteration"] == (i+1), "configuration"])
    new_it_conf <- it_conf[!(it_conf %in% irace_results$allElites[[i]])]
    n_conf <- length(new_it_conf)
    
    # Generate updated standard deviation (numerical params)
    s <- s * (1/n_conf)^(1/n_par)
    for (elite in 1:length(irace_results$allElites[[i]])){
      par_mean <- all_elites[[i]][elite]
      X <- rbind(X, cbind(i, elite, param_name, par_mean, s))
    }
  }
  
  X <- as.data.frame(X)
  colnames(X) <-c("iteration", "elite", "parameter", "mean", "sd")
  X[,"sd"] <- as.numeric(as.character(X[,"sd"]))
  X[,"mean"] <- as.numeric(as.character(X[,"mean"]))
  rownames(X) <- NULL
  return(X)
}

# Plot a categorical model
#
# @description
# 
# The `plotCategoricalModel` function creates a stacked bar plot showing
# the sampling probabilities of the parameter values for the elite
# configurations in the iterations of the configuration process. 
# 
# @param model_data
# String, data frame obtained from the `getCategoricalModel` function
#
# @param domain
# String Vector, domain of the parameter whose model will be plotted
# 
# @return bar plot
# FIXME: examples!
plotCategoricalModel <- function(model_data, domain) 
{
  value <- prob <- elite <- NULL 
  model_data$elite <- factor(model_data$elite)
  p <- ggplot(model_data, aes(fill=value, y=prob, x=elite, group=value)) +
    geom_bar(position="stack", stat="identity") + 
    ggplot2::scale_fill_viridis_d() +
    facet_grid(~ iteration, scales = "free", space = "free")
    
  
  p <- p  + labs(x = "Elite configurations", y = "Probability") +
      theme(axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.title.x = element_text(vjust = 4),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank()) 

  return(p)
}


# Plot a categorical model
#
# @description
# 
# Creates a sampling distributions plot of the numerical parameters for the
# elite configurations of an iteration.
# 
# This plot shows de density function of the truncated normal distributions
# associated to each parameter for each elite configuration.
# 
# @param iteration
# Numeric, iteration that should considered in the plot
# 
# @param model_data
# String, data frame obtained from the `getNumericalModel` function
#
# @param domain
# Numeric vector, domain of the parameter whose model will be plotted
# 
# @param xlabel_iteration
# Numeric, iteration in which the x axis labels should be included
# 
# @return sampling distribution plot
# FIXME: examples!
plotNumericalModel <- function(iteration, model_data, domain, xlabel_iteration)
{
  model_data <- model_data[model_data[,"iteration"] == iteration, ]
  model_data[,"elite"] <- as.factor(model_data[,"elite"])
  
  x  <- seq(from=domain[1], to=domain[2], length.out = 100)
  
  # create  plot
  p   <- ggplot(as.data.frame(x, ncol=1), aes(x=x))
  el <- unique(as.character(model_data[,"elite"]))
  col <- viridisLite::viridis(length(el))
  
  for (i in 1:length(el)) {
    mm <- model_data[model_data[,"elite"] == el[i], "mean"]
    ss <- model_data[model_data[,"elite"] == el[i], "sd"]
    if(is.na(mm)) next
    p <- p + ggplot2::stat_function(fun = truncnorm::dtruncnorm, 
                           geom = "area",
                           args=list(mean = mm, 
                                     sd = ss, 
                                     a = domain[1], 
                                     b = domain[2]), 
                           color = col[i], 
                           fill = col[i],
                           xlim = domain,
                           size = 0.5, 
                           alpha = 0.3)
  }
  if (xlabel_iteration==iteration){
    p <- p + ggplot2::ylab(as.character(iteration+1)) +
         theme(axis.title.x = element_blank(), 
               axis.title.y = element_text(vjust = 0),
               axis.text.y = element_blank(),
               axis.ticks.y = element_blank())
  } else {
    p <- p + theme(axis.text.x = element_blank(),
                   axis.title.x = element_blank(),
                   axis.ticks.x = element_blank(),
                   axis.title.y = element_text(vjust = 0),
                   axis.text.y = element_blank(), 
                   axis.ticks.y = element_blank()) 
    p <- p + labs(y = as.character(iteration+1))
  }
  p <- p + ggplot2::xlim(domain[1], domain[2])
  return(p)
}

#' Plot the sampling models used by irace
#'
#' @description
#' 
#' Display the sampling models from which irace generated parameter values for
#' new configurations during the configurations process.
#' 
#' For categorical parameters a stacked bar plot is created. This plot shows
#' the sampling probabilities of the parameter values for the elite
#' configurations in the iterations of the configuration process. 
#' 
#' For numerical parameters a sampling distributions plot of the
#' numerical parameters for the elite configurations of an iteration.
#' This plot shows de density function of the truncated normal distributions
#' associated to each parameter for each elite configuration on each iteration.
#' 
#' @template arg_irace_results
#' 
#' @param param_name
#' String, parameter to be included in the plot, e.g., `param_name = "algorithm"`
#' 
#' @template arg_filename
#' 
#' @return sampling model plot
#'
#' @examples
#' iraceResults <- read_logfile(system.file(package="irace", "exdata",
#'                                          "irace-acotsp.Rdata", mustWork = TRUE))
#' plot_model(iraceResults, param_name="algorithm")
#' \donttest{ 
#' plot_model(iraceResults, param_name="alpha")
#' }
#' @export
plot_model <- function(irace_results, param_name, filename=NULL)
{
  check_unknown_param_names(param_name, irace_results$parameters$names)
  iterations <- length(irace_results$allElites)
  
  if (irace_results$parameters$types[param_name] %in% c("c", "o")) {
    X <- getCategoricalModel(irace_results, param_name)
    q <- plotCategoricalModel(model_data=X, domain=irace_results$parameters$domain[[param_name]])
  } else {
    X <- getNumericalModel(irace_results, param_name)
    p <- lapply((iterations-1):1, plotNumericalModel, model_data=X, 
                domain=irace_results$parameters$domain[[param_name]], 
                xlabel_iteration=1)
    q <- do.call("grid.arrange", c(p, ncol = 1, left="Iterations"))
  }
  
  if (!is.null(filename))
    ggsave(filename, plot = q)
  q
}
