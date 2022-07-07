#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#



library(shiny)
library(plyr)
library(dplyr)

library(DT)
library(readxl)
dataset <- read_excel("dataFromUniprot.xlsx")



# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Uniprot Data"),
  
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            helpText("Filters"),
       
           selectInput("pickvalue", label = "Entrys", unique(dataset$Entry),
                       selected = NULL, multiple = T),
           selectInput("pickvalue2", label = "Length", unique(dataset$Length),
                       selected = NULL, multiple = T)
    
        ),
       

        # Show a plot of the generated distribution
        mainPanel(
          dataTableOutput('table')
        )
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
          
        entrys <- reactive({
            
            dat <- dataset
            
            if (!is.null(input$pickvalue)){dat <- dat %>% filter(Entry %in% input$pickvalue)} 
            if (!is.null(input$pickvalue2)){dat <- dat %>% filter(Length %in% input$pickvalue2)}
            dat <- dat %>% select(-Entry)
            
            return(dat)
            
        })
        
        
          
        output$table <- renderDataTable({ 
          datasets <- entrys()        
          },
          filter = 'top',
          rownames = FALSE)    
        }


# Run the application 
shinyApp(ui = ui, server = server)
