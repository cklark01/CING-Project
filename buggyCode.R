library(shiny)
library(plyr)
library(dplyr)
library(DT)
library(readxl)
library(cyjShiny)
library(htmlwidgets)
library(graph)
library(jsonlite)
library(later)
library(shinydashboard)
library(tidyverse)
library(shinyWidgets)


# data_one <- read_excel("/Users/KyriakiCH1/Desktop/data_one.xlsx")
data_one <- read.csv('/Users/KyriakiCH1/Desktop/NetworksApp/data/data.csv')
style.json.filename <- "/Users/KyriakiCH1/Desktop/NetworksApp/shiny/styles.json"


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Uniprot Data"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      helpText("Filters"),
      
      selectInput("databaseSelector", label = "Database", unique(data_one$Database),
                  selected = NULL, multiple = T),
      selectInput("diseaseSelector", label = "Disease", unique(data_one$Disease),
                  selected = NULL, multiple = T),
      selectInput("typeSelector", label = "Type", unique(data_one$Type),
                  selected = NULL, multiple = T)
      
    ),
    
    
    # Show a plot of the generated distribution
    
    mainPanel(
      tabsetPanel(id='tabs',
                  tabPanel("Table",  DT::dataTableOutput(outputId = ("table"))),
                  tabPanel("View",cyjShinyOutput('cyjShiny'), width = 10)
                           
                           
                  
                  
      )
    )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output,session) {
  ns = session$ns
  
  #Function to show inputs in table
  shinyInput <- function(FUN, len, id, ns, ...) {
    inputs <- character(len)
    for (i in seq_len(len)) {
      inputs[i] <- as.character(FUN(paste0(ns(id), i), ...))
    }
    inputs
  }
  
  # Function for data filtering 
  entrys <- reactive({
    
    dat <- data_one
    
    if (!is.null(input$diseaseSelector)){dat <- dat %>% filter(Disease %in% input$diseaseSelector)} 
    if (!is.null(input$databaseSelector)){dat <- dat %>% filter(Database %in% input$databaseSelector)}
    if (!is.null(input$typeSelector)){dat <- dat %>% filter(Type %in% input$typeSelector)}
    dat <- dat %>% select(-Disease)
    
    print(dat$DownloadFile[1])
    return(dat)
    
  })
  
  
  datatable_function<- reactive({
    data.frame(
      Select = shinyInput(checkboxInput,
                          nrow(entrys()),
                          'checkboxes',
                          label = NULL,
                          ns = ns,
                          width = 1,
      ),
      Network = entrys()$Network ,
      Description =  entrys()$Description,
      # "No. of Nodes" = (data_one$Nodes),
      # "No. of Edges" = (data_one$Edges),
      Download = shinyInput(downloadButton,
                            nrow(entrys()),
                            'button_',
                            ns = ns,
                            label = "Download",
                            onclick = sprintf("Shiny.setInputValue('%s', this.id)",ns("select_button"), downloadLoop())
                            
                            )
      
      
      
    )})
  
  # my_range <- 1: nrow(entrys())
  downloadLoop <- reactive({
    print(nrow(entrys()))
    for (i in  1: nrow(entrys())){
  
      lapply( 1: nrow(entrys()), function(i){
        edgefile <- read.csv(entrys()$DownloadFile[i])
        output[[paste0("button_",i)]] <-
          downloadHandler(
            filename = "edgeList.txt",
            content = function(file){
              write.csv(edgefile  ,file)
            }
          )
  
      })}
  })
  


  
  output$table <- DT::renderDataTable({ datatable(datatable_function(),
                                               escape = FALSE,
                                               rownames = FALSE,
                                               selection = "single",
                                               options = (
                                                 list(
                                                   preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
                                                   drawCallback = JS('function() { Shiny.bindAll(this.api().table().node()); } '),
                                                   ordering = FALSE,
                                                   paging = FALSE,
                                                   autoWidth = FALSE,
                                                   scrollY = "100vh",
                                                   scrollCollapse = FALSE
                                                 )
                                               ),
                                               callback = htmlwidgets::JS(
                                                 "table.on('dblclick', 'td',", 
                                                 "  function() {",
                     
                                                  "    var row = table.cell(this).index().row;",
                                                 
                                                 "    Shiny.setInputValue('dt_dblclick',  {row});",
                                                 "  }",
                                                 ");"
                                               )
                                               
  )
    
  },server = FALSE)  
  
  observeEvent(input$dt_dblclick, {
    
    
    print(input$dt_dblclick$row)
    
    
    updateTabsetPanel(session, "tabs",
                      selected = 'View')
    
    
    output$cyjShiny <- renderCyjShiny({
      graphAsJSON <- readAndStandardizeJSONNetworkFile(readLines(data_one$NetworkCyto[input$dt_dblclick$row + 1]))
      cyjShiny(graph=graphAsJSON, layoutName="preset", styleFile=data_one$StyleFile[input$dt_dblclick$row + 1])  })
    
    
    
    
  })
  
  
  
}


# Run the application 
shinyApp(ui = ui, server = server)
