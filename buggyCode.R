library(shiny)
library(plyr)
library(dplyr)
library(DT)
library(readxl)

# data_one <- read_excel("/Users/KyriakiCH1/Desktop/data_one.xlsx")
data_one <- read.csv('/Users/KyriakiCH1/Desktop/lets see/data.csv')


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Uniprot Data"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      helpText("Filters"),
      
      selectInput("pickvalue", label = "Disease", unique(data_one$Disease),
                  selected = NULL, multiple = T),
      selectInput("pickvalue2", label = "Database", unique(data_one$Database),
                  selected = NULL, multiple = T)
      
    ),
    
    
    # Show a plot of the generated distribution
    mainPanel(
      dataTableOutput('table')
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
  entrys <- reactive({
    
    dat <- data_one
    
    if (!is.null(input$pickvalue)){dat <- dat %>% filter(Disease %in% input$pickvalue)} 
    if (!is.null(input$pickvalue2)){dat <- dat %>% filter(Database %in% input$pickvalue2)}
    dat <- dat %>% select(-Disease)
    
    return(dat)
    
  })
  datatable_function<- reactive({
    data.frame(
      Select = shinyInput(checkboxInput,
                          length(entrys()),
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
                            length(entrys()),
                            'button_',
                            ns = ns,
                            label = "Download",
                            onclick = sprintf("Shiny.setInputValue('%s', this.id)",ns("select_button")))
      
      
    )})
  
  
  output$table <-DT::renderDataTable({ datatable(datatable_function(),
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
    
  },server = FALSE) }

# Run the application 
shinyApp(ui = ui, server = server)
