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
  
})