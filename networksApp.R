library(shiny)
library(shinydashboard)
library(DT)


ui <- dashboardPage(
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
    tabItem(tabName = 'networks', 
            DT::dataTableOutput('dt1') , 
    tabItem(tabName = 'networks', downloadButton('downloadData', 'download')))
    
    
  )      
)

server <- function(input,output, session){
  
  ns = session$ns
  
  #this function allows the use of shiny inputs in server
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
                          ),
    Network = c('<img src="https://cytoscape.org/cytoscape-tutorials/presentations/modules/network-visualization/data-mapping-6.png" height="150"></img>','<img src="https://cytoscape.org/cytoscape-tutorials/protocols/rna-seq-data-analysis/string-up-viz.png" height="150"></img>' ), 
    Description = c('Here is a very beautiful network', 'Another network'),
    Download = shinyInput(downloadButton,
                         2,
                         'button_',
                         ns = ns,
                         label = "Download", 
                         onclick = sprintf("Shiny.setInputValue('%s', this.id)",ns("select_button")))
    
                         
    )})
  

  
  lapply (1:2, function(i){
    output[[paste0("button_",i)]] <- downloadHandler(
      filename = function() {
        paste('data-', Sys.Date(), '.txt', sep='')
      },
      content = function(file) {
        readr::write_delim(x = datatable_function(), path = file, delim = "\t")
      }
    )
  })
  
  observeEvent(input$select_button, {
    print(input$select_button)
  })
  
  
  output$downloadData <-
    downloadHandler(
      filename = "edgeList.csv",
      content = function(file){
        write.csv(datatable_function(),file)
      }
    )


  output$dt1 <- DT::renderDataTable({
    datatable(datatable_function(),escape=F)
          }) 
  

}


shinyApp(ui, server)
