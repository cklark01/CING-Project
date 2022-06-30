library(shiny)
library(cyjShiny)
library(htmlwidgets)
library(graph)
library(shiny)
library(DT)
library(shinydashboard)
library(jsonlite)
#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
demo.directory <- system.file(package="cyjShiny", "extdata", "demoGraphsAndStyles")
styles <- c("",
            "default style" = "default style",
            "simple"      = file.path(demo.directory, "smallDemoStyle.json"),
            "galFiltered" = file.path(demo.directory, "galFiltered-style.json"))

networks <- c(
              "simple"      = file.path(demo.directory, "stringinteractions.cyjs")
              )


#----------------------------------------------------------------------------------------------------
graph.json.filename <- "NetworkFiles/stringinteractions.cyjs"
style.json.filename <- "NetworkFiles/smallDemoStyle.json"
#----------------------------------------------------------------------------------------------------

ui = shinyUI(fluidPage(
  tags$style("#cyjShiny{height:100vh !important;}"),
  dashboardPage(
    #these are random things i found and added maybe we can customize them later
    dashboardHeader(title = 'Network Library'
                ), 
    dashboardSidebar(
      sidebarMenu(
        menuItem(text='Networks', tabName = 'networks',{menuItem(text='View', tabName = 'viewer')}, {menuItem(text='Analyze', tabName = 'analyzer')}), 
        menuItem(text='')
      )
    ),
    dashboardBody(
      fluidRow(
        box(cyjShinyOutput('cyjShiny')),
        box(
          title = "Box content here", br(),status = "primary",solidHeader = TRUE, "More box content",
          sliderInput("slider", "Slider input:", 1, 100, 50),
          textInput("text", "Text input:")
        ),
        box(
          title = "Network Creation Protocol",status = "warning",solidHeader = TRUE, br(), "More box content",
          sliderInput("slider", "Slider input:", 1, 100, 50),
          textInput("text", "Text input:")
        )
      
      )
    )
  
)
)
)

#----------------------------------------------------------------------------------------------------
server = function(input, output, session)
{
  output$cyjShiny <- renderCyjShiny({
    graphAsJSON <- readAndStandardizeJSONNetworkFile(graph.json.filename)
    cyjShiny(graph=graphAsJSON, layoutName="preset", styleFile=style.json.filename)
  })

  
  
  
} # server
#----------------------------------------------------------------------------------------------------
runApp(shinyApp(ui=ui, server=server), port=9999)

