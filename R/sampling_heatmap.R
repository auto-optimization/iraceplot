# Get the domain intervals to build a heat map plot over
# the parameters values
#
# @description
# 
# The `get_domain` function generates a set of size intervals  
# based on the domain of a parameter
# 
# @param param_name
# String, name of a parameter in the parameters object
#
# @param parameters
# List, Parameter definition object obtained from an irace log file
#
# @param size
# Integer, number of intervals to create in the domain
# 
# @return list with domain elements:
#  - param_name: parameter name
#  - type: parameter type either "n" (numerical) or "c" categorical
#  - size: size of the resulting domains
#  - param: ticks for the plot
#  - names: domain labels for the plot
#  - domain: starting points of the intervals
get_domain <- function(param_name, parameters, size)
{
  is_real <- function(param_name, parameters) parameters$types[param_name] %in% c("r", "r,log")
  is_integer <- function(param_name, parameters) parameters$types[param_name] %in% c("i", "i,log")
  is_cat <- function(param_name, parameters) parameters$types[param_name] %in% c("c", "o")
    
  old_domain <- parameters$domain[[param_name]]

  if (is_cat(param_name, parameters)) {
    size   <- length(old_domain)
    type   <- "c"
    param  <- 1:length(old_domain)
    domain <- old_domain
    names  <- old_domain
  } else {
    if (is_real(param_name, parameters)) {
      # Set default size
      if (size <= 0)
        size <- 10
    } else if (size <= 0) {
      size <- old_domain[2] - old_domain[1] 
      size <- min(10L, size)
    } else if (size > (old_domain[2] - old_domain[1])) {
      cli_alert_info(paste0("{.strong Note}: step size for integer parameters should not exceed",
                            " the size of their domain. Parameter {.field {param_name}} domain size: {old_domain[2] - old_domain[1]}, provided step size: {size}. Setting step size to: {min(old_domain[2] - old_domain[1], 10L)}\n"))
      size <- min(old_domain[2] - old_domain[1], 10L)
    }
    type <- "n"
    param <- seq(1,size)
    domain <- seq(old_domain[1], old_domain[2], length.out = size+1)
    # Generate domain names
    names <- c()
    for (i in 1:(size-1))
      names <- c(names, paste0("[", domain[i], ",", domain[i+1], ")"))
    names <- c(names, paste0("[",domain[size],",", domain[size+1], "]"))
  }
  list(param_name = param_name, 
       type = type, 
       size = size,
       param = param, 
       names = names, 
       domain = domain)
}

# Assigns a vector of parameter values to a domain obtained with the
# get_domain function.
#
# @description
# 
# The `which_domain` function generates a vector a transformed parameter values.
# These correspond to values of the intervals defined by the domain.
# 
# @param param_values
# Vector, a vector of parameter values
#
# @param domain
# List, the domain definition obtained from the get_domain function
# 
# @return a vector a transformed parameter values
which_domain <- function(param_values, domain)
{
  data <- rep(NA, length(param_values))
  
  if (domain$type == "c") {
    for (i in 1:domain$size) {
      sel <- which(!is.na(param_values) & (param_values == domain$domain[i]))
      data[sel] <- i
    }
  } else if (domain$type == "n") {
    for (i in 1:(domain$size-1)) {
      sel <- which(!is.na(param_values) & (param_values >= domain$domain[i]) & 
                   (param_values < (domain$domain[i+1])))
      data[sel] <- i
    }
    # last interval should include last value
    sel <- which(!is.na(param_values) & (param_values >= domain$domain[domain$size]) & 
                 (param_values <= (domain$domain[domain$size+1])))
    data[sel] <- domain$size
  }
  
  data[is.na(param_values)] <- 0
  return(data)
}

#' Sampling heat map plot
#'
#' Heatmap that displays the frequency of sampling values of two parameters.
#'
#' @template arg_irace_results
#' @template arg_param_names
#' 
#' @param sizes
#' Numeric vector that indicated the number of intervals to be considered for numerical 
#' parameters. This argument is positional with respect to param_names. By default, 
#' numerical parameters are displayed using 10 intervals.
#' (example sizes = c(0,10))
#'
#' @param iterations
#' Numeric vector, iteration number that should be included in the plot
#' (example: iterations = c(1,4,5))
#'
#' @param only_elite
#' logical (default TRUE), only print elite configurations.
#'
#' @template arg_filename
#'
#' @return sampling heat map plot
#' @export
#'
#' @examples
#' iraceResults <- read_logfile(system.file(package="irace", "exdata",
#'                                          "irace-acotsp.Rdata", mustWork = TRUE))
#' sampling_heatmap(iraceResults, param_names=c("beta", "alpha"))
#' sampling_heatmap(iraceResults, param_names=c("beta", "alpha"), iterations = c(3,4))
#' sampling_heatmap(iraceResults, param_names=c("beta", "alpha"), only_elite = FALSE)
sampling_heatmap <- function(irace_results, param_names, sizes = c(0,0), 
                             iterations = NULL, only_elite = TRUE, 
                             filename = NULL)
{
  # Check parameter values
  param_names <- check_unknown_param_names(param_names, irace_results$parameters$names)
  if (length(param_names) != 2L) stop("'param_names' must specify two parameters")
    
  # Check iterations
  if (!is.null(iterations)) {
    it <- 1:length(irace_results$allElites)
    if (any(!(iterations %in% it))) {
      stop("The iterations entered are outside the possible range")
    }
  } else {
    iterations <- 1:length(irace_results$allElites)
  } 
  
  # Check configurations
  if (only_elite)
    id_configuration <- unlist(unique(irace_results$allElites[iterations]))
  else
    id_configuration <- unique(irace_results$experimentLog[irace_results$experimentLog[,"iteration"] %in% iterations, "configuration"])
  
  # Select data 
  config <- irace_results$allConfigurations[irace_results$allConfigurations[, ".ID."] %in% id_configuration, ,drop=FALSE]
  config <- config[, colnames(config) %in% param_names]

  domain1 <- get_domain(param_names[1], irace_results$parameters, sizes[1])
  domain2 <- get_domain(param_names[2], irace_results$parameters, sizes[2])
  
  params <- data.frame(param1 = which_domain(config[, param_names[1]], domain1), 
                       param2 = which_domain(config[, param_names[2]], domain2), 
                       stringsAsFactors = FALSE)
  
  domain_names1 <- domain1$names
  if (any(params$param1 == 0)) {
    domain_names1 <- c("NA", domain1$names)
    params$param1 <- factor(params$param1, levels=c(0,domain1$param))
  } else {
    params$param1 <- factor(params$param1, levels=domain1$param)
  }
  
  domain_names2 <- domain2$names
  if (any(params$param2 == 0)) {
    domain_names2 <- c("NA", domain2$names)
    params$param2 <- factor(params$param2, levels=c(0, domain2$param))
  } else {
    params$param2 <- factor(params$param2, levels=domain2$param)
  }
  
  param1 <- param2 <- n <- NULL # Silence warnings 
  df <- params %>% count(param1, param2, .drop = FALSE)

  p <- ggplot(df, aes(x = param1, y = param2, fill=n)) +
       geom_tile(color = "white", lwd = 0.5, linetype = 1) +
       xlab(param_names[1]) +ylab(param_names[2]) +
       scale_x_discrete(labels=domain_names1) +
       scale_y_discrete(labels=domain_names2) +
       theme_bw() + 
       theme(axis.text.x = element_text(angle = -90, vjust = 0.5, hjust=0),
             panel.border=element_blank(),
             legend.title = element_blank())

  # If the value in filename is added the pdf file is created
  if (!is.null(filename)) {
    ggsave(filename, plot = p)
    # If you do not add the value of filename, the plot is displayed
  } else {
    p
    return(p)
  }
}

#' Sampling heat map plot
#'
#' Heatmap that displays the frequency of sampling values of two parameters.
#'
#' @param configurations
#' Data frame, configurations in `irace` format 
#' (example: `configurations = iraceResults$allConfigurations`)
#' 
#' @param parameters
#' List, parameter object in irace format
#' (example: `configurations = iraceResults$parameters`)
#'
#' @param param_names
#' String vector of size 2, names of the parameters that should be included in the plot
#' (example: param_names = c("beta","alpha"))
#' 
#' @param sizes
#' Numeric vector that indicated the number of intervals to be considered for numerical 
#' parameters. This argument is positional with respect to param_names. By default, 
#' numerical parameters are displayed using 10 intervals.
#' (example sizes = c(0,10))
#'
#' @template arg_filename
#'
#' @return sampling heat map plot
#'
#' @examples
#' iraceResults <- read_logfile(system.file(package="irace", "exdata",
#'                                          "irace-acotsp.Rdata", mustWork = TRUE))
#' sampling_heatmap2(iraceResults$allConfigurations, iraceResults$parameters, 
#'                   param_names=c("beta", "alpha"))
#' @export
sampling_heatmap2 <- function(configurations, parameters, param_names, 
                              sizes = c(0,0), filename = NULL)
{
  param_names <- check_unknown_param_names(param_names, parameters$names)
  if (length(param_names) != 2L) stop("'param_names' must specify two parameters")
  
  # Select data 
  config <- configurations[, colnames(configurations) %in% param_names]
  
  domain1 <- get_domain(param_names[1], parameters, sizes[1])
  domain2 <- get_domain(param_names[2], parameters, sizes[2])
  
  params <- data.frame(param1 = which_domain(config[, param_names[1]], domain1), 
                       param2 = which_domain(config[, param_names[2]], domain2), 
                       stringsAsFactors = FALSE)
  
  domain_names1 <- domain1$names
  if (any(params$param1 == 0)) {
    domain_names1 <- c("NA", domain1$names)
    params$param1 <- factor(params$param1, levels=c(0,domain1$param))
  } else {
    params$param1 <- factor(params$param1, levels=domain1$param)
  }
  
  domain_names2 <- domain2$names
  if (any(params$param2 == 0)) {
    domain_names2 <- c("NA", domain2$names)
    params$param2 <- factor(params$param2, levels=c(0, domain2$param))
  } else {
    params$param2 <- factor(params$param2, levels=domain2$param)
  }
  param1 <- param2 <- n <- NULL 
  df <- params %>% count(param1, param2, .drop = FALSE)
  
  p <- ggplot(df, aes(x = param1, y = param2, fill=n)) +
    geom_tile(color = "white", lwd = 0.5, linetype = 1) +
    xlab(param_names[1]) + ylab(param_names[2]) +
    scale_x_discrete(labels=domain_names1) +
    scale_y_discrete(labels=domain_names2) +
    theme_bw() + 
    theme(axis.text.x = element_text(angle = -90, vjust = 0.5, hjust=0),
          panel.border=element_blank(),
          legend.title = element_blank())
  
  # If the value in filename is added the pdf file is created
  if (!is.null(filename)) {
    ggsave(filename, plot = p)
    # If you do not add the value of filename, the plot is displayed
  } else {
    p
  }
  return(p)
}
