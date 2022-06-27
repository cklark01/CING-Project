

library(shiny)
library(DT)
library(shinydashboard)

## module UI
module_ui  <- function(id){
  ns <- NS(id)
  dashboardPage(
    #these are random things i found and added maybe we can customize them later
    dashboardHeader(title = 'Network Library', 
                    
                    dropdownMenu(
                      type = 'messages', 
                      messageItem(from = 'Mike', message = 'This is a sample message')
                    ), #static example
                    dropdownMenuOutput('messageMenu'), #dynamic example
                    dropdownMenu(
                      type = 'notifications', 
                      notificationItem(text = 'This is a test notification')
                    ),
                    dropdownMenu(
                      type = 'tasks', 
                      taskItem(text='This is a test task', value = 50)
                    )), 
    dashboardSidebar(
      sidebarMenu(
        menuItem(text='Networks', tabName = 'networks',{menuItem(text='View', tabName = 'viewer')}, {menuItem(text='Analyze', tabName = 'analyzer')}), 
        menuItem(text='')
      )
    ),
    dashboardBody(
      
      tagList(
        DT::dataTableOutput(outputId = ns("dt1"))
        
      )      
    )
    
    
  )
}

## module server
module_server <- function(input, output, session ){
  ns = session$ns
  
  myValue <- reactiveValues(check = '')
  
  shinyInput <- function(FUN, len, id, ns, ...) {
    inputs <- character(len)
    for (i in seq_len(len)) {
      inputs[i] <- as.character(FUN(paste0(ns(id), i), ...))
    }
    inputs
  }
  
  
  datatable_function <- reactive({
    tibble::tibble(
      Select = shinyInput(checkboxInput,
                          2,
                          'checkboxes',
                          label = NULL,
                          ns = ns,
                          width = 1,
      ),
      Network = c('<img src="https://cytoscape.org/cytoscape-tutorials/presentations/modules/network-visualization/data-mapping-6.png" height="150"></img>','<img src="https://cytoscape.org/cytoscape-tutorials/protocols/rna-seq-data-analysis/string-up-viz.png" height="150"></img>' ), 
      Description = c('Here is a very beautiful network', 'Another network'),
      Download = shinyInput(downloadButton,
                            2,
                            'buttons',
                            ns = ns,
                            label = "Download", 
                            onclick = sprintf("Shiny.setInputValue('%s', this.id)",ns("select_button")))
      
      
    )})
  
  lapply(1:2 , function(i){
    
    output[[paste0("buttons",i)]] <-
      downloadHandler(
        filename = "edgeList.txt",
        content = function(file){
          write.csv(datatable_function() ,file)
        }
      )
  })
  
  observeEvent(input$select_button, {
    print(input$select_button)
  })
  
  
  output$dt1 <- DT::renderDataTable({
    datatable(datatable_function(), escape = FALSE, 
              options = 
                list(
                  preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
                  drawCallback = JS('function() { Shiny.bindAll(this.api().table().node()); } ')
                )
    )
  })
}

ui <- 
  module_ui(id = "test_dt_inside_module")


server <- function(input, output, session) {
  callModule(module = module_server , id = "test_dt_inside_module")
}

shinyApp(ui, server)