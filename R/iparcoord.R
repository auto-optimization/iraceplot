#' Parallel Coordinates Plot interactive
#'
#' @description
#' The function will returns a parallel coordinates plot
#' interactive in shinyApp allowing the analysis of the set of parameters
#' allowing the visualization of the data and filter by iteration
#'
#' @param iraceResults
#' The data generated when loading the Rdata file created by irace
#'
#' @param idConfiguration
#' Numeric vector, you need to put the configurations you want to analyze
#' (example: idConfiguration = c(20,50,100,300,500,600,700))
#'
#' @param param_names
#' String vector, you need to put the parameters you want to analyze
#' (example: param_names = c("algorithm","alpha","rho","q0","rasrank"))
#'
#' @return parallel cordinates plot
#' @export
#'
#'@importFrom dplyr arrange
#'@importFrom parcoords parcoords
#'@importFrom shinydashboard  dashboardBody dashboardPage dashboardHeader dashboardSidebar box
#'@importFrom shiny dataTableOutput renderDataTable shinyApp fluidRow checkboxGroupInput h3
#'
#' @examples
#' NULL

iparcoord <- function(iraceResults, idConfiguration = NULL, param_names = NULL){

  return("Arreglando algunas cosas")

  #Variable assignment
  memo  <- configuration <- dim <- choi <- NULL
  idConfiguration <- unlist(idConfiguration)
  param_names <- unlist(param_names)

  #verify that param_names is other than null
  if(!is.null(param_names)){
    #verify that param_names contain the data entered
    if( "FALSE" %in% names(table(param_names %in% iraceResults$parameters$names))){
      return("Some wrong parameter entered")
    }
    #verify that param_names contain more than one parameter
    else if(length(param_names) < 2){
      return("You must enter at least two parameters")
    }

  }

  if(!is.null(idConfiguration)){

    # Verify that the entered id are within the possible range
    if(length(idConfiguration[idConfiguration < 1]) >= 1 || length(idConfiguration[idConfiguration > dim(iraceResults$allConfigurations)[1]]) >= 1){
      return("IDs entered are outside the range of settings")
    }

    # Verify that the id entered are more than 1 or less than the possible total
    if(length(idConfiguration) <= 1 || length(idConfiguration) > dim(iraceResults$allConfigurations)[1] ){
      return("You must enter more than one id")
    }

    # the table to be used and the filter with the iterations and configuration is created
    selection <- iraceResults$allConfigurations[, ".ID."] %in% idConfiguration
    tabla <- iraceResults$allConfigurations[selection,]
    filtro <- unique(iraceResults$experimentLog[,c("iteration","configuration")])
    selection2 <- filtro[, "configuration"] %in% idConfiguration
    filtro <- filtro[selection2,]
  }
  # table is created with all settings
  else{
    tabla <-iraceResults$allConfigurations
    filtro <- unique(iraceResults$experimentLog[,c("iteration","configuration")])
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
  for(k in 1:(length(tabla))){
    tabla[[k]][is.na(tabla[[k]])] <- "NA"
  }

  #The dimension is modified for certain parameters in the plot
  # if("rasrank" %in% colnames(tabla) && "elitistants" %in% colnames(tabla)){
  #   dim = list(
  #     rasrank = list(
  #       tickValues = c(min(tabla$rasrank),
  #                    iraceResults$parameters$domain[["rasrank"]][2]/4,
  #                    iraceResults$parameters$domain[["rasrank"]][2]/2,
  #                    max(tabla$rasrank))
  #     ),
  #     elitistants = list(
  #       tickValues = c(min(tabla$elitistants),
  #                    450,
  #                    max(tabla$elitistants))
  #       )
  #     )
  # }

  #create plot dimensions
  for(i in 1:length(tabla)){

    if(colnames(tabla)[i] == "iteration"){
      dim[[i]] = list(
        range = c(min(tabla[[i]], na.rm = TRUE),max(tabla[[i]], na.rm = TRUE)),
        values = tabla[[i]],
        label = colnames(tabla)[i],
        visible = FALSE
      )
      #if the column is of type character
    }else if(class(tabla[[i]]) == "character"){
      factor_tab <- NULL
      factor_tab <- factor(tabla[[i]])
      levels(factor_tab) <- c(1:length(unique(tabla[[i]])))
      if("NA" %in% tabla[[i]]){
        tickT = c(iraceResults$parameters$domain[[colnames(tabla)[i]]],"NA")
      }else{
        tickT = iraceResults$parameters$domain[[colnames(tabla)[i]]]
      }
      dim[[i]] = list(
        label = colnames(tabla)[i],
        tickvals = c(1:length(unique(tabla[[i]]))),
        #ticktext = unique(tabla[[i]]),
        ticktext = tickT,
        values = factor_tab
      )
      #if the column is of type numeric
    }else if(class(tabla[[i]]) == "numeric"){
      if((as.numeric(iraceResults$parameters$domain[[colnames(tabla)[i]]][2])+1) %in% tabla[[i]]){
        minimo = iraceResults$parameters$domain[[colnames(tabla)[i]]][1]
        medio = round((max(tabla[[i]], na.rm = TRUE)/4),1)
        medio2 = round((max(tabla[[i]], na.rm = TRUE)/2),1)
        medio3 = round((max(tabla[[i]], na.rm = TRUE)*(3/4)),1)
        maximo = iraceResults$parameters$domain[[colnames(tabla)[i]]][2] +1
        dim[[i]] = list(
          range = c(iraceResults$parameters$domain[[colnames(tabla)[i]]][1],iraceResults$parameters$domain[[colnames(tabla)[i]]][2]+1),
          tickvals = c(minimo,medio,medio2,medio3,maximo),
          ticktext = c(minimo,medio,medio2,medio3,"NA"),
          values = tabla[[i]],
          label = colnames(tabla)[i]
        )
      }else{
        minimo = iraceResults$parameters$domain[[colnames(tabla)[i]]][1]
        medio = round((max(tabla[[i]], na.rm = TRUE)/4),1)
        medio2 = round((max(tabla[[i]], na.rm = TRUE)/2),1)
        medio3 = round((max(tabla[[i]], na.rm = TRUE)*(3/4)),1)
        maximo = iraceResults$parameters$domain[[colnames(tabla)[i]]][2]
        dim[[i]] = list(
          range = c(iraceResults$parameters$domain[[colnames(tabla)[i]]][1],iraceResults$parameters$domain[[colnames(tabla)[i]]][2]),
          tickvals = c(minimo,medio,medio2,medio3,maximo),
          ticktext = c(minimo,medio,medio2,medio3,maximo),
          values = tabla[[i]],
          label = colnames(tabla)[i])
      }

    }
    #other types
    else{
      dim[[i]] = list(
        range = c(min(tabla[[i]], na.rm = TRUE),max(tabla[[i]], na.rm = TRUE)),
        values = tabla[[i]],
        label = colnames(tabla)[i]
      )
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

