library(shiny)
library(DT)
library(cyjShiny)
library(htmlwidgets)
library(graph)
library(jsonlite)
library(later)
library(shinydashboard)
library(readr)
library(xml2)
library(tidyverse)


data_one <- read.csv('/Users/KyriakiCH1/Desktop/NetworksApp/data/data.csv')
style.json.filename <- "/Users/KyriakiCH1/Desktop/NetworksApp/shiny/styles.json"

tags$style("#cyjShiny{height:300vh !important;}")

xml_address = "/Users/KyriakiCH1/Desktop/network_description_example.xml"
networkDescription = as_list(read_xml(xml_address))



ui <-

  dashboardPage(
    
    #these are random things i found and added maybe we can customize them later
    dashboardHeader(
      title = 'NetHub'),
    dashboardSidebar(
      sidebarMenu(
        selectInput("databaseSelector", label = "Database: ", unique(data_one$Database),
                    selected = NULL, multiple = T),
        selectInput("diseaseSelector", label = "Disease: ", unique(data_one$Disease),
                    selected = NULL, multiple = T),
        selectInput("typeSelector", label = "Type: ", unique(data_one$Type),
                    selected = NULL, multiple = T),
        sliderInput("obs", "Number of Nodes:",
                    min = 0, max = 1000, value = 500
        ),
        sliderInput("obs", "Number of Edges:",
                    min = 0, max = 1000, value = 500
        )
        
      )
    ),
    dashboardBody(
      tabsetPanel(id='tabs',
                  tabPanel("Table",  DT::dataTableOutput(outputId = ("dt1"))),
                  tabPanel("View", box(
                    style = "margin-bottom:-15px;",
                    width = "100px",
                    title = "Network Visualization",
                    cyjShinyOutput('cyjShiny')),
                    
                    box(
                      title = networkDescription$networks$network$description,
                      width = "100px",
                      status = "warning",
                      DT::dataTableOutput(outputId = ("dt2")),
                      br(),
                      "Node/Edge Description: ",br(),
                      verbatimTextOutput("text22"),
                      solidHeader = TRUE
                      
                    ),
                    box(
                      title = "Netowork Details",
                      width = "50px",
                      "Dataset: ",br(),
                      verbatimTextOutput("text1"),
                      
                      "Edge Formation Method: ",br(),
                      verbatimTextOutput("text12"),
                      
                      "Visualization: ",br(),
                      verbatimTextOutput("text13"),
                      
                      uiOutput("myList"),
                      status = "primary",
                      solidHeader = TRUE,
                      collapsible = FALSE,
                      collapsed = FALSE
                      
                      
                    ),
                    
                    
                    infoBox("Network Data Statistics", value = NULL, subtitle = NULL,
                            icon = shiny::icon("bar-chart"), color = "aqua", width = 14,
                            href = NULL, fill = FALSE)
                    
                    
                  )
                    ))
    )
  





server <- function(input,output, session) {
  
  
  ns = session$ns
  
  #Function to show inputs in table
  shinyInput <- function(FUN, len, id, ns, ...) {
    inputs <- character(len)
    for (i in seq_len(len)) {
      inputs[i] <- as.character(FUN(paste0(ns(id), i), ...))
    }
    inputs
  }
  
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
                            onclick = downloadLoop()
                            
      )
      
      
      
    )})
  
  datatable_function2 <- reactive({
    data.frame(
      Description = (data_one$Description[input$dt_dblclick$row + 1]),
      "No. of Nodes" = (data_one$Nodes[input$dt_dblclick$row + 1]),
      "No. of Edges" = (data_one$Edges[input$dt_dblclick$row + 1])
    )
  })
  
  # my_range <- 1: nrow(entrys())
  downloadLoop <- reactive({
    # print(nrow(entrys()))
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
  
  
  output$dt2 <- DT::renderDataTable({
    datatable(
      datatable_function2(),
      escape = FALSE,
      rownames = FALSE,
      selection = "single",
      options = (
        list(
          preDrawCallback = JS(
            'function() { Shiny.unbindAll(this.api().table().node()); }'
          ),
          drawCallback = JS(
            'function() { Shiny.bindAll(this.api().table().node()); } '
          ),
          dom = "t",
          ordering = FALSE,
          paging = FALSE,
          autoWidth = TRUE,
          scrollCollapse = FALSE
        )
      ),
    )
  })
  
  
  
  output$dt1 <-DT::renderDataTable({ datatable(datatable_function(),
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
      cyjShiny(graph=graphAsJSON, layoutName="preset", styleFile= style.json.filename ) })
    
    
    
  })
  
  
  output$text1 <- renderText({
    HTML(" Database Name:",unlist(networkDescription$networks$network$creationProtocol$dataset$database$databseName),"\n",
         "Database Version:",unlist(networkDescription$networks$network$creationProtocol$dataset$database$version),"\n",
         "Database Query:",unlist(networkDescription$networks$network$creationProtocol$dataset$database$query),"\n",
         "Filters Used:",unlist(networkDescription$networks$network$creationProtocol$dataset$database$filters),"\n",
         "Url:",unlist(networkDescription$networks$network$creationProtocol$dataset$database$url),"\n",
         "Entry Type:",unlist(networkDescription$networks$network$creationProtocol$dataset$entryType),"\n",
         "Number of Entries:",unlist(networkDescription$networks$network$creationProtocol$dataset$numEntries),"\n"
    )  
  })
  
  output$text12 <- renderText({
    HTML(" Tool:",unlist(networkDescription$networks$network$creationProtocol$edgeFormationMethod$tool),"\n",
         "Description:",unlist(networkDescription$networks$network$creationProtocol$edgeFormationMethod$description))
  })
  
  output$text13 <- renderText({
    paste(unlist(networkDescription$networks$network$visualization))
  })
  
  output$text22 <- renderText({
    HTML(" Edges Description:",unlist(networkDescription$networks$network$edges$edgeDescription),"\n",
         "Nodes Description:",unlist(networkDescription$networks$network$nodes$nodeDescription))
  })
  
  
  
}


shinyApp(ui, server)
