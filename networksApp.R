

library(shiny)
library(DT)
library(shinydashboard)

## module UI
module_ui  <- function(id){
  ns <- NS(id)
  dashboardPage(
    #these are random things i found and added maybe we can customize them later
    dashboardHeader(
      title = 'NetHub'),
    dashboardSidebar(
      sidebarMenu(
        sidebarSearchForm(textId = "searchText", buttonId = "searchButton",
                          label = "Search..."),
        menuItem(text='Views',icon = icon("hive"), tabName = 'views',
                 {menuItem(text='Views in Uniprot', tabName = 'viewerU')},
                 {menuItem(text='Views in String', tabName = 'viewerS')},
                 {menuItem(text='Views in DisGNet', tabName = 'viewerD')}), 
        menuItem(text='Browse',icon = icon("filter"), tabName = 'analyze',
                 {menuItem(text='Proteins', tabName = 'viewerS')},
                 {menuItem(text='Sequence', tabName = 'viewerS')},
                 {menuItem(text='Genes', tabName = 'viewerD')}),
        menuItem("Diseases", icon = icon("disease"), tabName = "dis",
                 {menuItem(text='Cancer', tabName = 'can')},
                 {menuItem(text='Cancer', tabName = 'can')},
                 {menuItem(text='Cancer', tabName = 'can')}),
        sliderInput("obs", "Number of Nodes:",
                   min = 0, max = 1000, value = 500
        ),
        sliderInput("obs", "Number of Edges:",
                   min = 0, max = 1000, value = 500
        )
        
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
  
  
  #Function to show inputs in table
  shinyInput <- function(FUN, len, id, ns, ...) {
    inputs <- character(len)
    for (i in seq_len(len)) {
      inputs[i] <- as.character(FUN(paste0(ns(id), i), ...))
    }
    inputs
  }
  
  #Table Dynamic creation
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
  #Download Handler for all the download buttons
  lapply(1:2 , function(i){
    output[[paste0("buttons",i)]] <-
      downloadHandler(
        filename = "edgeList.txt",
        content = function(file){
          write.csv(datatable_function() ,file)
        }
      )
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