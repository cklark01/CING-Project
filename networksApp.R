library(shiny)
library(DT)
library(cyjShiny)



library(shinydashboard)


library(tidyverse)


library(igraph)


library(RMySQL)
library(excelR)
library(data.table)




mysqlconnection = dbConnect(
  RMySQL::MySQL(),
  dbname = 'networkApp',
  host = 'localhost',
  port = 3306,
  user = 'root',
  password = 'Password123'
)

result = dbSendQuery(mysqlconnection, "select * from book3")
data_one = fetch(result)
dbDisconnect(mysqlconnection)


ui <-
  fluidPage(
    tags$head(tags$style(
      HTML("input[type='search']:disabled {visibility:hidden}
           ")
    )),
    
    tags$head(
      tags$style(HTML("
      
      .pre  {
      background-color: #e8e8e8;
      border: none;
      } 
      .myclass pre {
        display: flex;
        color: black;
        background-color: #e8e8e8;
        font-weight: bolder;
        border: none;

      }
      .text pre  {
      background-color: white;
      margin: auto;
      border: none;
      padding: 10px;
      font-family: Arial, Helvetica, sans-serif;
      } 
      .text h2  {
      text-align: center;
      border: none;
      font-family: Arial, Helvetica, sans-serif;
      } 
     
      "))
    ),
    
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    ),
    dashboardPage(
      dashboardHeader(title = 'NetHub'),
      dashboardSidebar(collapsed = TRUE),
      
      
      dashboardBody(
        
        tabsetPanel(
        id = 'tabs',
        tabPanel(
          "Home",
          fluidRow(
            tags$img(
              src = "Head and Neck gene_drugs.jpg",
              height="100%", width="100%"
            )
          ),
          div(class = "text",
          fluidRow(
                h2("Total Networks Statistics"),
                infoBox(
                width = "90%",
               
                verbatimTextOutput("text41" ))
                 )),
          div(class = "myclass",
          fluidRow(
            box(
              title = "Our Vision",
              status = "info",
              
              "Scientific progress depends on standard graph datasets for which claims, hypotheses, and algorithms can be compared and evaluated. Despite the importance of having standard network datasets, it is often impossible to find the original data used in published experiments, and at best it is difficult and time consuming. This site is an effort to improve and facilitate the scientific study of networks by making it easier for researchers to download, analyze, and investigate a large collection of network data. Our goal is to make these scientific graph datasets widely available to everyone while also providing a first attempt at interactive analytics on the web. "
            ),
              box(
                title = "About",
                status = "info",
              tags$img(
                src = "gen.png",
                style = "postion: flex-end",
                height="50%", width="50%"
              )
              ),
           
          )
          )
          ),
          
        tabPanel(
          "Networks Table", 
          DT::dataTableOutput(outputId = ("dt1"))
                 ),
        tabPanel(
          "Visualization",
          box(
            style = "margin-bottom:-15px;",
            width = "1000px",
            status = "info",
            title = "Network Visualization",
            cyjShinyOutput('cyjShiny'),
            
            
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
  
  
  
  downloadLoop <- reactive({
    for (i in  1:nrow(data_one)) {
      lapply(1:nrow(data_one), function(i) {
        edgefile <- data_one$DownloadFile[i]
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
  
  
  output$text41 <- renderText({
    HTML(
    
      " Total Networks:",
      paste(nrow(data_one)),
      "\n",
      "Average Nodes:",
      paste(round(mean(data_one$Nodes),0)),
      "\n",
      "Average Edges:",
      paste(round(mean(data_one$Edges),0)),
    )
  })
  
  output$text33 <- renderText({
    MyGraph <-
      read_graph(data_one$NetworkGraphMl[input$dt_dblclick$row + 1], "graphml")
    HTML(
      " Max Degree:",
      paste(unlist(max(
        degree(MyGraph)
      ))),
      "\n",
      "Min Degree:",
      paste(unlist(min(
        degree(MyGraph)
      ))),
      "\n",
      "Max K-Core",
      paste(unlist(max(
        coreness(MyGraph)
      ))),
      "\n"
    )
  })
  
  output$graphDeg <- renderPlot(plot(ecdf(degree_distribution(
    read_graph(data_one$NetworkGraphMl[input$dt_dblclick$row + 1], "graphml")
  ))))
  output$graphCent <- renderPlot(plot(ecdf(degree_distribution(
    read_graph(data_one$NetworkGraphMl[input$dt_dblclick$row + 1], "graphml")
  ))))
  
  
  
  
  datatable_function <- reactive({
    data.frame(
      Network = paste0(
        "<img src=\"",
        data_one$Image,
        "\" height=\"150\" data-toggle=\"tooltip\" data-placement=\"right\" title=\"",
        "\"></img>"
      ),
      Disease = data_one$Disease,
      Nodes = data_one$Nodes,
      Edges = data_one$Edges,
      Database = data_one$DatabaseName,
      Download = shinyInput(
        downloadButton,
        nrow(data_one),
        'button_',
        ns = ns,
        label = "Download",
        onclick = downloadLoop()
        
      )
      
      
      
    )
  })
  
  datatable_function3 <- reactive({
    data.frame(
      
      "Total Netwroks" = nrow(data_one),

      "Average Nodes" =(round(mean(data_one$Nodes),0)),
     
      "Average Edges" =(round(mean(data_one$Edges),0))
      
      
    )
  })
  
  output$v1 <- renderExcel({
    excelTable(datatable_function3())
  })
  
  datatable_function2 <- reactive({
    data.frame(
      Description = (data_one$networkDescription[input$dt_dblclick$row + 1]),
      "Number of Nodes" = (data_one$Nodes[input$dt_dblclick$row + 1]),
      "No. of Edges" = (data_one$Edges[input$dt_dblclick$row + 1]),
      "Node Legend Size" = (data_one$nodeLegendSize[input$dt_dblclick$row + 1]),
      "Node Legend Colour" = (data_one$nodeLegendColour[input$dt_dblclick$row + 1]),
      "Edge Legend Thickness" = (data_one$edgeLegendThickness[input$dt_dblclick$row + 1]),
      "Notes/Remarks" = (data_one$notes[input$dt_dblclick$row + 1])
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
          dom = "t",
          ordering = FALSE,
          paging = FALSE,
          autoWidth = TRUE,
          scrollCollapse = FALSE
        )
      )
    )
  })
  

  
  output$dt1 <- DT::renderDataTable({
   
    datatable(
      datatable_function(),
      escape = FALSE,
      rownames = FALSE,
      selection = "single",
      
      style = "default",
      options = (
        list(
          
          columnDefs = list(list(targets = c(0,5), searchable = FALSE)),
       
          ordering = FALSE,
          autoWidth = TRUE,
          scrollY = "100vh",
          scrollCollapse = FALSE,
          pageLength = 10,
          lengthMenu = FALSE,
          lengthChange = FALSE
        )
      ),
    
      filter = list(position = 'top', clear = FALSE),
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
                      selected = 'Visualization')
    
    if (data_one$NetworkStyle[input$dt_dblclick$row + 1] != '') {
      style <- data_one$NetworkStyle[input$dt_dblclick$row + 1]
    }
    else {
      style <-  "/Users/constantinosclark/Downloads/defaultStyles.json"
    }
    output$cyjShiny <- renderCyjShiny({
      graphAsJSON <-
        readAndStandardizeJSONNetworkFile(readLines(data_one$NetworkCyto[input$dt_dblclick$row + 1]))
      cyjShiny(graph = graphAsJSON,
               layoutName = "cola",
               styleFile = style)
    })
    
    
  })
  
  
}




shinyApp(ui, server)
