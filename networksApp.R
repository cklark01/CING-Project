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
    tags$style('.container-fluid {
                             background-color: #007BA7;
              }'),
    
    tags$head(tags$style(
      HTML(
        "

      .pre  {
      display: flex;
      background-color: #e8e8e8;
      border: none;
      margin: 20px;
      padding: 20px;
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
      background-color: #e8e8e8;
      text-align: center;
      border: none;
      font-family: Arial, Helvetica, sans-serif;
      }

        /* body */
      .content-wrapper, .right-side {
      background-color: #e8e8e8;
      }

      "
      )
    )),
    
    
    
    dashboardPage(
      dashboardHeader(title = 'NetHub'),
      dashboardSidebar(collapsed = TRUE),
      
      
      dashboardBody(tabsetPanel(
        id = 'tabs',
        tabPanel(
          "Home",
          fluidRow(
            tags$img(
              src = "Head and Neck gene_drugs.jpg",
              height = "100%",
              width = "100%"
            )
          ),
          div(
            class = "text",
            
            h2("Total Networks Statistics"),
            
            
            infoBox(width = "50%",
                    verbatimTextOutput("text41")),
            box(
              width = "100%",
              
              
              div(
                style = "position:relative; ",
                
                selectInput("plotType", "Categorize by",
                            c(Type = "type", Disease ="disease" )),
                actionButton("Enter", "Enter")
              ),
              DT::dataTableOutput(outputId = ("dt3"))
            )
          ),
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
                    height = "50%",
                    width = "50%"
                  )
                ),
                
              ))
        ),
        
        tabPanel("Networks Table",
                 DT::dataTableOutput(outputId = ("dt1"))),
        tabPanel(
          "Visualization",
          box(
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
            title = "Network Details",
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
            "Notes/Remarks: ",
            br(),
            verbatimTextOutput("text14"),
            
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
            box(
            plotOutput("graphDeg"),
            plotOutput("graphDegHIST")
            ),
            box(
            plotOutput("coreness"),
            plotOutput("closeness")
            )
            
          )
          
          
        )
      ))
    )
  )





server <- function(input, output, session) {
  ns = session$ns
  
  
  shinyInput <- function(FUN, len, id, ns, ...) {
    inputs <- character(len)
    for (i in seq_len(len)) {
      inputs[i] <- as.character(FUN(paste0(ns(id), i), ...))
    }
    inputs
  }
  
  
  
  lapply(1:nrow(data_one), function(i){
    edgefile <- read.csv(data_one$DownloadFile[i])
    output[[paste0("button_",i)]] <- downloadHandler(
      filename = function() {
        paste('data-', Sys.Date(), '.csv', sep='')
      },
      content = function(file) {
      write.csv(edgefile, file)
      }
    )
  })
  
  output$text1 <- renderText({
    HTML(
      " Database Name: ",
      paste(unlist(data_one$DatabaseName[input$dt_dblclick$row + 1])),
      "\n",
      "Database Version: ",
      unlist(data_one$DatabaseVersion[input$dt_dblclick$row + 1]),
      "\n",
      "Filters Used: ",
      unlist(data_one$DatabaseFilters[input$dt_dblclick$row + 1]),
      "\n",
      "Query: ",
      unlist(data_one$Query[input$dt_dblclick$row + 1]),
      "\n",
      "Entry Type: ",
      unlist(data_one$datasetEntryType[input$dt_dblclick$row + 1]),
      "\n",
      "Number of Entries: ",
      unlist(data_one$datasetNumEntries[input$dt_dblclick$row + 1]),
      "\n"
    )
  })
  
  output$text12 <- renderText({
    HTML(
      " Tool:",
      unlist(data_one$edgeFormationMethodTool[input$dt_dblclick$row + 1]),
      "\n",
      "Description:",
      unlist(data_one$EdgeFormationDescription[input$dt_dblclick$row + 1])
    )
  })
  
  output$text13 <- renderText({
    HTML(" ", paste(unlist(data_one$visualizationTool[input$dt_dblclick$row + 1])))
  })
  output$text14 <- renderText({
    HTML(" ", paste(unlist(data_one$notes[input$dt_dblclick$row + 1])))
  })
  
  output$text22 <- renderText({
    HTML(
      " Edges Description: ",
      unlist(data_one$edgeDescription[input$dt_dblclick$row + 1]),
      "\n",
      "Edge Legend Thickness: ",
      unlist((data_one$edgeLegendThickness[input$dt_dblclick$row + 1])),
      "\n",
      "Nodes Description: ",
      unlist(data_one$nodeDescription[input$dt_dblclick$row + 1]),
      "\n",
      "Node Legend Size: ",
      unlist((data_one$nodeLegendSize[input$dt_dblclick$row + 1])),
      "\n",
      "Node Legend Colour: ",
      unlist((data_one$nodeLegendColour[input$dt_dblclick$row + 1]))
    )
  })
  
  
  output$text41 <- renderText({
    HTML(
      " Total Networks: ",
      paste(nrow(data_one)),
      "\n",
      "Average Nodes: ",
      paste(round(mean(data_one$Nodes), 0)),
      "\n",
      "Average Edges: ",
      paste(round(mean(data_one$Edges), 0)),
    )
  })
  
  Disease <- unique(data_one$DiseaseCat)
  uniqType <- unique(data_one$networkType)
  df2 <- as.data.frame(t(data_one$DiseaseCat))
  df1 <- as.data.frame(t(data_one$networkType))
  uniqTypeSum <- replicate(length(uniqType), 0)
  DiseaseSum <- replicate(length(Disease), 0)
  for (i in 1:length(uniqType)) {
    uniqTypeSum[i] <- rowSums(df1 == uniqType[i])
  }
  
  for (i in 1:length(Disease)) {
    DiseaseSum[i] <- rowSums(df2 == Disease[i])
  }
  
  classType <- data.frame(uniqType, uniqTypeSum)
  classDis <- data.frame(Disease, DiseaseSum)
  colnamesType <- c("Network Type", "Number of Networks")
  colnamesDis <-
    c("Disease related with Network", "Number of Networks")
  data_choice <- reactiveVal(classDis)
  colnames <- reactiveVal(colnamesDis)
  
  observeEvent(input$Enter, {
    if (input$plotType == "disease") {
      data_choice(classDis)
      colnames(colnamesDis)
    } else{
      data_choice(classType)
      colnames(colnamesType)
    }
  })
  
  
  output$dt3 <- DT::renderDataTable({
    datatable(
      data_choice(),
      colnames = colnames(),
      escape = FALSE,
      rownames = FALSE,
      selection = "single",
      options = (
        list(
          pageLength = 10,
          lengthMenu = FALSE,
          lengthChange = FALSE,
          searching = FALSE,
          order = list(1, 'desc'),
          paging = TRUE,
          autoWidth = TRUE,
          scrollCollapse = FALSE
        )
      )
    )
  })
  output$text33 <- renderText({
    MyGraph <-
      read_graph(data_one$NetworkGraphMl[input$dt_dblclick$row + 1], "graphml")
    HTML(
        " Max Degree:", paste(unlist(max(degree(MyGraph)))),"\n",
        "Min Degree:", paste(unlist(min(degree(MyGraph)))),"\n",
        #"Max K-Core:",  paste(unlist(max(coreness(MyGraph)))),"\n",
        "Average degree:",paste(unlist(mean(degree(MyGraph)))),"\n",
        "Density:",paste(unlist(edge_density(MyGraph, loops=F))),"\n",
        "Number of triangles:",paste(unlist(transitivity(MyGraph, type="global"))),"\n",
        "Max Betweenness:",paste(unlist(max((betweenness(MyGraph))))),"\n",
        "Max Edge Betweenness:",paste(unlist(max(edge_betweenness(MyGraph)))),"\n",
        "Strength:",paste(unlist(max(strength(MyGraph)))),"\n",
        "Clustering Coefficient:",paste(unlist((transitivity(MyGraph)))),"\n",
    )
  })
  
  output$graphDeg <- renderPlot(
    plot((betweenness(read_graph(data_one$NetworkGraphMl[input$dt_dblclick$row + 1], "graphml"))),main="Betweeness",xlab="X axis of degree distribution",ylab="Y axis of degree distribution",
         font.main=4, type="l", font.xlab=4, font.ylab=4.7, bg = "red",   # Fill color
         col = "blue", # Border color
         cex = 1,      # Symbol size
         lwd = 3)
    
    
    
  )
  
  output$graphDegHIST <- renderPlot(
    
    hist(degree(read_graph(data_one$NetworkGraphMl[input$dt_dblclick$row + 1], "graphml")), main="Histogram of node degree",xlab="x axis of node degree"))
  
  

  
  output$coreness<-renderPlot(
    plot(strength(read_graph(data_one$NetworkGraphMl[input$dt_dblclick$row + 1], "graphml")),type="l",main="Strength",xlab="X axis of Strength",ylab="Y axis of Strength",
         font.main=4, font.lab=4, font.sub=4, bg = "red",   # Fill color
         col = "blue", # Border color
         cex = 1,      # Symbol size
         lwd = 3)
  )
  
  output$closeness<-renderPlot(
    plot(closeness(read_graph(data_one$NetworkGraphMl[input$dt_dblclick$row + 1], "graphml")),main="closeness",xlab="X axis of closeness",ylab="Y axis of closeness", 
         bg = "red",
         type="l",
         col = "blue", # Border color
         cex = 1,      # Symbol size
         lwd = 3
    )
  )
  
  
  
  
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
      NetworkType = data_one$networkType,
      
      Download = shinyInput(downloadButton, 
                            nrow(data_one),
                            'button_',
                            ns = ns,
                            label = "Download",
                            onclick = sprintf("Shiny.setInputValue('%s', this.id)",ns("select_button"))
      )
      
      
      
    )
  })
  
  observeEvent(input$select_button, {
    print(input$select_button)
  })
  
  
  
  datatable_function2 <- reactive({
    data.frame(
      Description = (data_one$networkDescription[input$dt_dblclick$row + 1]),
      "Number of Nodes" = (data_one$Nodes[input$dt_dblclick$row + 1]),
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
      #extensions = 'Buttons',
      style = "default",
      options = (
        list(
          columnDefs = list(list(
            targets = c(0, 6), searchable = FALSE
          )),
          #buttons = c('copy', 'csv', 'excel'),
          #dom = 'Bfrtip',
          ordering = FALSE,
          autoWidth = TRUE,
          scrollY = "100vh",
          scrollCollapse = FALSE,
          pageLength = 10,
          lengthMenu = FALSE,
          lengthChange = FALSE,
          preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
          drawCallback = JS('function() { Shiny.bindAll(this.api().table().node()); } ')
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
      style <-  "www/defaultStyles.json"
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
