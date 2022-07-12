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

data_one <-
  read_csv("/Users/constantinosclark/Desktop/CING/CING-Project/NetworkFiles/data.csv")

style.json.filename <-
  "/Users/constantinosclark/Desktop/CING/CING-Project/NetworkFiles/styles.json"

tags$style("#cyjShiny{height:300vh !important;}")

xml_address = "network_description_example.xml"
networkDescription = as_list(read_xml(xml_address))

ui <-
  fluidPage(dashboardPage(
    #these are random things i found and added maybe we can customize them later
    dashboardHeader(title = 'NetHub'),
    dashboardSidebar(
      sidebarMenu(
        sidebarSearchForm(
          textId = "searchText",
          buttonId = "searchButton",
          label = "Search..."
        ),
        menuItem(
          text = 'Views',
          icon = icon("hive"),
          tabName = 'views',
          {
            menuItem(text = 'View in Uniprot', tabName = 'viewerU')
          },
          {
            menuItem(text = 'View in String', tabName = 'viewerS')
          },
          {
            menuItem(text = 'View in DisGNet', tabName = 'viewerD')
          }
        ),
        menuItem(
          text = 'Browse',
          icon = icon("filter"),
          tabName = 'analyze',
          {
            menuItem(text = 'Proteins', tabName = 'viewerS')
          },
          {
            menuItem(text = 'Sequence', tabName = 'viewerS')
          },
          {
            menuItem(text = 'Genes', tabName = 'viewerD')
          }
        ),
        menuItem(
          "Diseases",
          icon = icon("disease"),
          tabName = "dis",
          {
            menuItem(text = 'Cancer', tabName = 'can')
          },
          {
            menuItem(text = 'Cancer', tabName = 'can')
          },
          {
            menuItem(text = 'Cancer', tabName = 'can')
          }
        ),
        sliderInput(
          "obs",
          "Number of Nodes:",
          min = 0,
          max = 1000,
          value = 500
        ),
        sliderInput(
          "obs",
          "Number of Edges:",
          min = 0,
          max = 1000,
          value = 500
        )
        
      )
    ),
    dashboardBody(# cyjShinyOutput('cyjShiny'), width = 10 ,
      # tagList(
      #   DT::dataTableOutput(outputId = ("dt1"))
      # ),
      tabsetPanel(
        id = 'tabs',
        tabPanel("Table",  DT::dataTableOutput(outputId = ("dt1"))),
        tabPanel(
          "View",
          
          
          box(
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
            "Database: ",br(),
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
  ))





server <- function(input, output, session) {
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
    data.frame(
      Select = shinyInput(
        checkboxInput,
        nrow(data_one),
        'checkboxes',
        label = NULL,
        ns = ns,
        width = 1,
      ),
      Network = data_one$Network ,
      Description = (data_one$Description),
      Download = shinyInput(
        downloadButton,
        nrow(data_one),
        'button_',
        ns = ns,
        label = "Download",
        onclick = sprintf(
          "Shiny.setInputValue('%s', this.id)",
          ns("select_button")
        )
      )
      
      
    )
  })
  
  datatable_function2 <- reactive({
    data.frame(
      Description = (data_one$Description[input$dt_dblclick$row + 1]),
      "No. of Nodes" = (data_one$Nodes[input$dt_dblclick$row + 1]),
      "No. of Edges" = (data_one$Edges[input$dt_dblclick$row + 1])
    )
  })
  
  
  my_range <- 1:nrow(data_one)
  
  for (i in my_range) {
    lapply(my_range, function(i) {
      
      edgefile <- read_csv(data_one$DownloadFile[i])
      output[[paste0("button_", i)]] <-
        downloadHandler(
          filename = "edgeList.txt",
          content = function(file) {
            write.csv(edgefile  , file)
          }
        )
      
    })
  }
  
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
  
  
  output$dt1 <- DT::renderDataTable({
    datatable(
      datatable_function(),
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
    
  }, server = FALSE)
  
  
  
  observeEvent(input$dt_dblclick, {
    
    updateTabsetPanel(session, "tabs",
                      selected = 'View')
    
    print(data_one$NetworkCyto[input$dt_dblclick$row + 1])
    output$cyjShiny <- renderCyjShiny({
      graphAsJSON <-
        readAndStandardizeJSONNetworkFile(readLines(data_one$NetworkCyto[input$dt_dblclick$row + 1]))
      cyjShiny(graph = graphAsJSON,
               layoutName = "cola",
               styleFile = style.json.filename)
      
    })
    
  })
  

  
  output$text1 <- renderText({
      paste(networkDescription$networks$network$creationProtocol$dataset)
  })
  
  output$text12 <- renderText({
    paste(networkDescription$networks$network$creationProtocol$edgeFormationMethod)
  })
  
  output$text13 <- renderText({
    paste(networkDescription$networks$network$visualization)
  })
  
  output$text22 <- renderText({
    HTML("<ul><li>Edges Description:",networkDescription$networks$network$edges$edgeDescription,
         "</li><li>Nodes Description:",networkDescription$networks$network$nodes$nodeDescription,
         "</li></ul>")
  })
    

  
  
  
  
  
}


shinyApp(ui, server)
