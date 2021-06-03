#' Parallel Coordinates Plot interactive
#'
#' @description
#' The function will returns a parallel coordinates plot
#' interactive in shinyApp allowing the analysis of the set of parameters
#' allowing the visualization of the data and filter by iteration
#'
#' @template arg_irace_results
#'
#' @param id_configuration
#' Numeric vector, you need to put the configurations you want to analyze
#' (example: id_configuration = c(20,50,100,300,500,600,700))
#'
#' @param param_names
#' String vector, you need to put the parameters you want to analyze
#' (example: param_names = c("algorithm","alpha","rho","q0","rasrank"))
#'
#' @return parallel cordinates plot
#'
#'@importFrom dplyr arrange
#'@importFrom parcoords parcoords
#'@importFrom shinydashboard  dashboardBody dashboardPage dashboardHeader dashboardSidebar box
#'@importFrom shiny dataTableOutput renderDataTable shinyApp fluidRow checkboxGroupInput h3
#'
#' @examples
#' NULL

iparallel_coord <- function(irace_results, id_configuration = NULL, param_names = NULL){

  #Variable assignment
  memo  <- configuration <- dim <- choi <- cont <-NULL
  id_configuration <- unlist(id_configuration)
  param_names <- unlist(param_names)
  dim <- list()
  cont = 0

  #verify that param_names is other than null
  if(!is.null(param_names)){
    #verify that param_names contain the data entered
    if( "FALSE" %in% names(table(param_names %in% irace_results$parameters$names))){
      return("Some wrong parameter entered")
    }
    #verify that param_names contain more than one parameter
    else if(length(param_names) < 2){
      return("You must enter at least two parameters")
    }

  }

  if(!is.null(id_configuration)){

    # Verify that the entered id are within the possible range
    if(length(id_configuration[id_configuration < 1]) >= 1 || length(id_configuration[id_configuration > dim(irace_results$allConfigurations)[1]]) >= 1){
      return("IDs entered are outside the range of settings")
    }

    # Verify that the id entered are more than 1 or less than the possible total
    if(length(id_configuration) <= 1 || length(id_configuration) > dim(irace_results$allConfigurations)[1] ){
      return("You must enter more than one id")
    }

    # the table to be used and the filter with the iterations and configuration is created
    selection <- irace_results$allConfigurations[, ".ID."] %in% id_configuration
    tabla <- irace_results$allConfigurations[selection,]
    filtro <- unique(irace_results$experimentLog[,c("iteration","configuration")])
    selection2 <- filtro[, "configuration"] %in% id_configuration
    filtro <- filtro[selection2,]
  }
  # table is created with all settings
  else{
    tabla <-irace_results$allConfigurations
    filtro <- unique(irace_results$experimentLog[,c("iteration","configuration")])
  }

  # The filter table is created and ordered according to the configurations
  filtro <- as.data.frame(filtro)
  filtro <- arrange(filtro,configuration)

  # An iteration table is created and added to the table
  iteration <- sample(NA,size=dim(tabla)[1],replace = TRUE)
  tabla <- cbind(tabla,iteration)

  # The NA of the first row of the table is replaced in the iteration column
  if(tabla$.ID.[1] == filtro$configuration[1] ){
    tabla$iteration[1] = filtro$iteration[1]
  }

  # memo is assigned the value of the filter table configuration
  memo = filtro$configuration[1]

  #The NAs of the table are replaced in the iteration column
  for(i in 2:dim(filtro)[1]){

    #if the same configuration has more than one iteration, a new row is created
    if(memo == filtro$configuration[i]){
      add <- tabla[tabla$.ID. == memo,]
      add$iteration = filtro$iteration[i]
      tabla <- rbind(tabla,add)
    }
    #The iteration is assigned to the configuration
    else{
      tabla$iteration[tabla$.ID. == filtro$configuration[i]] = filtro$iteration[i]
    }
    memo = filtro$configuration[i]
  }

  # Column .ID. and .PARENT. are removed
  tabla <- tabla[, !(names(tabla) %in% c(".ID.",".PARENT."))]
  if(!is.null(param_names)){
    param_names <- c(param_names,"iteration")
    tabla <- tabla[, (names(tabla) %in% param_names )]
  }

  # NA are converted to character in the table
  # for(k in 1:(length(tabla))){
  #   tabla[[k]][is.na(tabla[[k]])] <- "NA"
  # }

  #NA data processing
  for(k in 1:(length(tabla))){
    if(class(tabla[[k]]) == "numeric" && NA %in% tabla[[k]]){
      tabla[[k]][is.na(tabla[[k]])] <- (irace_results$parameters$domain[[colnames(tabla)[k]]][2] + 1)
    }else if(class(tabla[[k]]) == "character" && NA %in% tabla[[k]]){
      tabla[[k]][is.na(tabla[[k]])] <- "NA"
    }
  }

  for (i in 1:(length(colnames(tabla))-1)) {
    if(class(tabla[[i]]) == "character"){

    }else if(class(tabla[[i]]) == "numeric"){
      cont = cont +1
      dim[colnames(tabla)[i]] <- dim[cont]
      if((as.numeric(irace_results$parameters$domain[[colnames(tabla)[i]]][2])+1) %in% tabla[[i]]){
        minimo = irace_results$parameters$domain[[colnames(tabla)[i]]][1]
        medio = round((max(tabla[[i]], na.rm = TRUE)/4),1)
        medio2 = round((max(tabla[[i]], na.rm = TRUE)/2),1)
        medio3 = round((max(tabla[[i]], na.rm = TRUE)*(3/4)),1)
        maximo = irace_results$parameters$domain[[colnames(tabla)[i]]][2] +1

        dim[[cont]] = list(
          tickValues = c(minimo,medio,medio2,medio3,maximo)
          )
      }else{
        minimo = irace_results$parameters$domain[[colnames(tabla)[i]]][1]
        medio = round((max(tabla[[i]], na.rm = TRUE)/4),1)
        medio2 = round((max(tabla[[i]], na.rm = TRUE)/2),1)
        medio3 = round((max(tabla[[i]], na.rm = TRUE)*(3/4)),1)
        tickFormat = list()
        maximo = irace_results$parameters$domain[[colnames(tabla)[i]]][2]
        dim[[cont]] = list(
          tickValues = c(minimo,medio,medio2,medio3,maximo)
          )
        }
    }
  }

  #A list is created based on the iterations of the table
  for(m in min(tabla$iteration):max(tabla$iteration)){
    texto = paste("Iteration",m)
    choi[[texto]] <- m
  }


  #shiny app body
  body <- dashboardBody(
    fluidRow(
      # parallel coordinate plot box
      box(
        title = "Irace Parcoords Plot", width = 10, status = "primary",
        parcoords::parcoordsOutput("tablaPlot")
      ),
      # iterations box
      box(
        width = 2,
        checkboxGroupInput("checkGroup",h3("Iterations"),
                           choices =  choi,
                           selected = c(min(tabla$iteration):max(tabla$iteration)))
      )
    ),
    # Data box
    fluidRow(
      box(
        title = "Extracted Data", status = "primary",
        dataTableOutput("SelectedData")
      )
    )
  )

  ui <- dashboardPage(
    dashboardHeader(title="Parcoords Plot"),
    dashboardSidebar(disable=TRUE),
    body
  )

  server = function(input, output, session) {


    #Standard parcoords plot in shiny

    output$tablaPlot <- parcoords::renderParcoords({
      tablaOut <- tabla[(tabla$iteration %in% input$checkGroup),]
      parcoords::parcoords(tablaOut,
                           rownames = FALSE,
                           reorder=TRUE,
                           brushMode = "1D",
                           color = list(
                             colorBy = "iteration",
                             colorScale = "scaleOrdinal",
                             colorScheme = "schemePaired"),
                           withD3 = TRUE,
                           autoresize = TRUE,
                           dimensions = dim,
                           tasks = list(htmlwidgets::JS(
                             "
    function(){
      // supply an array of columns to hide
      this.parcoords.hideAxis(['names','iteration'])

      this.parcoords.removeAxes();
      this.parcoords.render();

      // duplicated from the widget js code
      //  to make sure reorderable and brushes work
      if( this.x.options.reorderable ) {
        this.parcoords.reorderable();
      } else {
        this.parcoords.createAxes();
      }

      if( this.x.options.brushMode ) {
      // reset the brush with None
        this.parcoords.brushMode('None')
        this.parcoords.brushMode(this.x.options.brushMode);
        this.parcoords.brushPredicate(this.x.options.brushPredicate);
      }

      // delete title from the rownames axis
      //d3.select('#' + this.el.id + ' .dimension .axis > text').remove();
    }
    "
                           ))
      )

    })


    # Here we can access the variable input$id_rows to determine which are selected
    # we display these results in a table
    output$SelectedData <- renderDataTable({
      ids <- rownames(tabla) %in% input$tablaPlot_brushed_row_names
      tabla[ids,]
    })

  }

  shinyApp(ui, server)

}

