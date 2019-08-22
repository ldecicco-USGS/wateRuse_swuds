# nolint

raw_data <- reactiveValues(data = NULL)

observeEvent(input$example_data, {
  raw_data$data <- swuds_sample
})

observeEvent(input$merge_data, {
  df <- as_swuds(raw_data$dataQUANT,
                    raw_data$dataPOP)
  raw_data$data <- df
})

observeEvent(input$dataPOP, {
  path <- file.path(input$dataPOP$datapath)
  
  new_path <- paste0(input$dataPOP$datapath, "_",
                     input$dataPOP$name)
  
  new_path <- gsub(", ", "_", new_path)
  
  file.rename(from = path, to = new_path)
  
  raw_data$dataPOP <- read_swuds_pop(new_path)
  
})

observeEvent(input$dataQUANT,{
  path <- file.path(input$dataQUANT$datapath)
  
  new_path <- paste0(input$dataQUANT$datapath, "_",
                     input$dataQUANT$name)
  
  new_path <- gsub(", ", "_", new_path)
  
  file.rename(from = path, to = new_path)
  
  raw_data$dataQUANT <- read_swuds_quant(new_path)
  
})

melted_data <- reactive({
  
  return(raw_data$data)
  
})
