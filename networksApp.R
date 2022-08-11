library(shiny)
library(DT)
#library(cyjShiny)
library(htmlwidgets)
#library(graph)
library(jsonlite)
library(later)
library(shinydashboard)
library(readr)
library(xml2)
library(tidyverse)
library(reactlog)

library(igraph)

library(networkD3)
library(stringr)
library(odbc)
library(RMySQL)


shiny.reactlog = TRUE

mysqlconnection = dbConnect(
  RMySQL::MySQL(),
  dbname = 'networkApp',
  host = 'localhost',
  port = 3306,
  user = 'root',
  password = 'Password123'
)

result = dbSendQuery(mysqlconnection, "select * from book2")
data_one = fetch(result)
dbDisconnect(mysqlconnection)


#MyGraph <- sif2igraph(Path = "/Users/constantinosclark/Desktop/CING/CING-Project/NetworkFiles/network1.sif", directed=TRUE)
#data_one <- read_csv('/Users/constantinosclark/Desktop/CING/CING-Project/NetworkFiles/data.csv')




#xml_address = "/Users/constantinosclark/Desktop/CING/CING-Project/network_description_example.xml"
#networkDescription = as_list(read_xml(xml_address))



ui <-
  
  dashboardPage(
    dashboardHeader(title = 'NetHub'),
    dashboardSidebar(
      sidebarMenu(
        selectInput(
          "databaseSelector",
          label = "Database: ",
          unique(data_one$DatabaseName),
          selected = NULL,
          multiple = T
        ),
        selectInput(
          "diseaseSelector",
          label = "Disease: ",
          unique(data_one$Disease),
          selected = NULL,
          multiple = T
        ),
        selectInput(
          "typeSelector",
          label = "Type: ",
          unique(data_one$Type),
          selected = NULL,
          multiple = T
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
    
    
    dashboardBody(tabsetPanel(
      id = 'tabs',
      tabPanel("Table",  DT::dataTableOutput(outputId = ("dt1"))),
      tabPanel(
        "View",
        box(
          style = "margin-bottom:-15px;",
          width = "100px",
          title = "Network Visualization",
          #cyjShinyOutput('cyjShiny'),
          forceNetworkOutput("force")
        ),
        
        box(
          title = "Network Info",
          width = "100px",
          status = "warning",
          DT::dataTableOutput(outputId = ("dt2")),
          br(),
          "Node/Edge Description: ",
          br(),
          verbatimTextOutput("text22"),
          solidHeader = TRUE
          
        ),
        box(
          title = "Netowork Details",
          width = "50px",
          "Dataset: ",
          br(),
          verbatimTextOutput("text1"),
          
          "Edge Formation Method: ",
          br(),
          verbatimTextOutput("text12"),
          
          "Visualization: ",
          br(),
          verbatimTextOutput("text13"),
          
          uiOutput("myList"),
          status = "primary",
          solidHeader = TRUE,
          collapsible = FALSE,
          collapsed = FALSE
          
          
        ),
        
        
        box(
          "Network Data Statistics",
          value = NULL,
          subtitle = NULL,
          icon = shiny::icon("chart-column"),
          color = "aqua",
          width = 14,
          href = NULL,
          fill = FALSE,
          verbatimTextOutput("text33"),
          plotOutput("graphDeg"),
          plotOutput("graphCent")
          
        )
        
        
      )
    ))
  )






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
  
  entrys <- reactive({
    dat <- data_one
    
    if (!is.null(input$diseaseSelector)) {
      dat <- dat %>% filter(Disease %in% input$diseaseSelector)
    }
    if (!is.null(input$databaseSelector)) {
      dat <- dat %>% filter(DatabaseName %in% input$databaseSelector)
    }
    if (!is.null(input$typeSelector)) {
      dat <- dat %>% filter(Type %in% input$typeSelector)
    }
    dat <- dat %>% select(-Disease)

    return(dat)
    
  })
  
  # my_range <- 1: nrow(entrys())
  downloadLoop <- reactive({
    # print(nrow(entrys()))
    for (i in  1:nrow(entrys())) {
      lapply(1:nrow(entrys()), function(i) {
        edgefile <- read_csv(entrys()$DownloadFile[i])
        output[[paste0("button_", i)]] <-
          downloadHandler(
            filename = "edgeList.txt",
            content = function(file) {
              write.csv(edgefile  , file)
            }
          )
        
      })
    }
  })
  
  output$text1 <- renderText({
    HTML(
      " Database Name:",
      paste(unlist(data_one$DatabaseName[input$dt_dblclick$row + 1])),
      "\n",
      "Database Version:",
      unlist(data_one$DatabaseVersion[input$dt_dblclick$row + 1]),
      "\n",
      #"Filters Used:",
      #unlist(data_one$DatabaseFilters),
      "\n",
      "Entry Type:",
      unlist(data_one$entryType[input$dt_dblclick$row + 1]),
      "\n",
      "Number of Entries:",
      unlist(data_one$numEntries[input$dt_dblclick$row + 1]),
      "\n"
    )
  })
  
  output$text12 <- renderText({
    HTML(
      " Tool:",
      unlist(data_one$edgeFormationMethodTool[input$dt_dblclick$row + 1]),
      "\n",
      "Description:",
      unlist(data_one$edgeFormationMethodDescription[input$dt_dblclick$row + 1])
    )
  })
  
  output$text13 <- renderText({
    paste(unlist(data_one$visualizationTool[input$dt_dblclick$row + 1]))
  })
  
  output$text22 <- renderText({
    HTML(
      " Edges Description:",
      unlist(data_one$edgeDescription[input$dt_dblclick$row + 1]),
      "\n",
      "Nodes Description:",
      unlist(data_one$nodeDescription[input$dt_dblclick$row + 1])
    )
  })
  
  output$text33 <- renderText({
    
    HTML(
    " Max Degree:", paste(unlist(max(degree(MyGraph)))),"\n",
    "Min Degree:", paste(unlist(min(degree(MyGraph)))),"\n",
    "Max K-Core",  paste(unlist(max(coreness(MyGraph)))),"\n"
    )
  })
  
  output$graphDeg <- renderPlot(
    plot(ecdf(degree_distribution(MyGraph)))
  )
  output$graphCent <- renderPlot(
    plot(ecdf(degree_distribution(MyGraph)))
  )

  
  
  
  datatable_function <- reactive({
    data.frame(
      Select = shinyInput(
        checkboxInput,
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
      Download = shinyInput(
        downloadButton,
        nrow(entrys()),
        'button_',
        ns = ns,
        label = "Download",
        onclick = downloadLoop()
        
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
          ordering = FALSE,
          paging = FALSE,
          autoWidth = FALSE,
          scrollY = "100vh",
          scrollCollapse = FALSE
        )
      ),
      filter = list(
        position = 'top', clear = FALSE
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

    #output$cyjShiny <- renderCyjShiny({
    # graphAsJSON <- readAndStandardizeJSONNetworkFile(readLines(data_one$NetworkCyto[input$dt_dblclick$row + 1]))
    # cyjShiny(graph=graphAsJSON, layoutName="preset", styleFile= data_one$NetworkStyle[input$dt_dblclick$row + 1] ) })
   
    
     MyGraph <-read_graph(data_one$NetworkGraphMl[input$dt_dblclick$row + 1], "graphml")
     wc <- cluster_walktrap(MyGraph)
     members <- membership(wc)
     gr <- igraph_to_networkD3(MyGraph, group = members)
     edgeList <- as_edgelist(MyGraph, names = TRUE)
     #F2 <- colorRampPalette(c("#FFFF00", "#FF0000"), bias = nrow(gr$links), space = "rgb", interpolate = "linear")
     output$force <- renderForceNetwork({
       forceNetwork(Links = gr$links, # data frame that contains info about edges
                    Nodes = gr$nodes, # data frame that contains info about nodes
                    Source = "source", # ID of source node 
                    Target = "target", # ID of target node
                    #Value = "Weight", # value from the edge list (data frame) that will be used to value/weight relationship amongst nodes
                    NodeID = "name", # value from the node list (data frame) that contains node description we want to use (e.g., node name)
                    #Nodesize = "nodeBetweenness",  # value from the node list (data frame) that contains value we want to use for a node size
                    Group = "group",  # value from the node list (data frame) that contains value we want to use for node color
                    height = 500, # Size of the plot (vertical)
                    width = 1000,  # Size of the plot (horizontal)
                    fontSize = 20, # Font size
                    #linkDistance = networkD3::JS("function(d) { return 10*d.value; }"), # Function to determine distance between any two nodes, uses variables already defined in forceNetwork function (not variables from a data frame)
                    #linkWidth = networkD3::JS("function(d) { return d.value/5; }"),# Function to determine link/edge thickness, uses variables already defined in forceNetwork function (not variables from a data frame)
                    opacity = 0.85, # opacity
                    zoom = TRUE, # ability to zoom when click on the node
                    opacityNoHover = 0.1 # opacity of labels when static
                    #linkColour = F2 # edge colors
       )
     
    
    })
     
  })
  
  
  
  
  
}



shinyApp(ui, server)
