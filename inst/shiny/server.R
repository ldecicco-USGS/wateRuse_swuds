options(shiny.maxRequestSize = 10 * 1024^2)


shiny::shinyServer(function(input, output, session) {
  
  observe({
    if (input$close > 0) shiny::stopApp()    
  })
  
  output$mymap <- leaflet::renderLeaflet({
    
    isolate({
      map <- leaflet::leaflet() %>%
        leaflet::addProviderTiles("CartoDB.Positron") %>%
        leaflet::setView(lng = -83.5, lat = 44.5, zoom = 6)    
    })
    
  })
  
  output$show_table <- DT::renderDataTable({    
    validate(
      need(!is.null(raw_data$data), "Please select a data set")
    )
    
    melt_df <- melted_data()
    
    DT::datatable(melt_df,
                  extensions = 'Buttons',
                  options = list(pageLength = 10),
                  rownames = FALSE)
  })
  
  source("get_data.R",local=TRUE)$value
  
  output$hasData <- reactive(!is.null(melted_data()))
  outputOptions(output, "hasData", suspendWhenHidden = FALSE)
  
  observe({
    updateSelectInput(session, "sites", choices = unique(melted_data()$FROM_SITE_NO))
  })
  
  output$monthly_timeseries <- renderPlot({
    
    validate(
      need(!is.null(raw_data$data), "Please select a data set")
    )
    
    if(input$sites != "All"){
      month_plot <- time_series_site_monthly(melted_data(),
                             input$sites)
      return(month_plot)
    }
    
    
  })
  
})
