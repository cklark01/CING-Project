library(shiny)
library(DT)
library(cyjShiny)
library(htmlwidgets)
library(graph)
library(jsonlite)
library(later)
library(shinydashboard)


data_one <- read.csv('/Users/KyriakiCH1/Desktop/lets see/data.csv')


style.json.filename <- "/Users/KyriakiCH1/Desktop/styles.json"




ui <-
  
  dashboardPage(
    
    #these are random things i found and added maybe we can customize them later
    dashboardHeader(
      title = 'NetHub'),
    dashboardSidebar(
      sidebarMenu(
        sidebarSearchForm(textId = "searchText", buttonId = "searchButton",
                          label = "Search..."),
        menuItem(text='Views',icon = icon("hive"), tabName = 'views',
                 {menuItem(text='View in Uniprot', tabName = 'viewerU')},
                 {menuItem(text='View in String', tabName = 'viewerS')},
                 {menuItem(text='View in DisGNet', tabName = 'viewerD')}), 
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
      # cyjShinyOutput('cyjShiny'), width = 10 ,
      # tagList(
      #   DT::dataTableOutput(outputId = ("dt1"))
      # ),
      tabsetPanel(id='tabs',
                  tabPanel("Table",  DT::dataTableOutput(outputId = ("dt1"))),
                  tabPanel("View",cyjShinyOutput('cyjShiny'), width = 10)
      )
      
    )
    
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
  
  #Table Dynamic creation
  datatable_function<- reactive({
    data.frame(
      Select = shinyInput(checkboxInput,
                          nrow(data_one),
                          'checkboxes',
                          label = NULL,
                          
                          ns = ns,
                          width = 1,
      ),
      Network = data_one$Network ,
      Description = (data_one$Description),
      View = shinyInput(actionButton,
                        nrow(data_one),
                        'view_button',
                        ns = ns,
                        label = "View Network",
                        onclick = sprintf("Shiny.setInputValue('%s', this.id)",ns("select_button"))),
      # "No. of Nodes" = (data_one$Nodes),
      # "No. of Edges" = (data_one$Edges),
      Download = shinyInput(downloadButton,
                            nrow(data_one),
                            'button_',
                            ns = ns,
                            label = "Download",
                            onclick = sprintf("Shiny.setInputValue('%s', this.id)",ns("select_button")))
      
      
    )})
  
  my_range <- 1: nrow(data_one)
  
  for (i in my_range){
    
    lapply(my_range, function(i){
      edgefile <- read.csv(read.csv('/Users/KyriakiCH1/Desktop/lets see/data.csv')$DownloadFile[i])
      output[[paste0("button_",i)]] <-
        downloadHandler(
          filename = "edgeList.txt",
          content = function(file){
            write.csv(edgefile  ,file)
          }
        )
      
    })}
  
  
  
  output$dt1 <-DT::renderDataTable({ datatable(datatable_function(),
                                               escape = FALSE,
                                               rownames = FALSE,
                                               selection = "single",
                                               options = (
                                                 list(
                                                   preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
                                                   drawCallback = JS('function() { Shiny.bindAll(this.api().table().node()); } '),
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
    
  },server = FALSE)
  

  
    observeEvent(input$dt_dblclick, {


    print(input$dt_dblclick$row)

    updateTabsetPanel(session, "tabs",
                      selected = 'View')


    output$cyjShiny <- renderCyjShiny({
    graphAsJSON <- readAndStandardizeJSONNetworkFile(readLines(data_one$NetworkCyto[input$dt_dblclick$row + 1]))
    cyjShiny(graph=graphAsJSON, layoutName="preset", styleFile= style.json.filename ) })



      })

  
    
  
}


shinyApp(ui, server)
