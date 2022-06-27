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

networks <- c("",
              "simple"      = file.path(demo.directory, "stringinteractions.cyjs"),
              "galFiltered" = file.path(demo.directory, "galFiltered.cyjs"))


#----------------------------------------------------------------------------------------------------
graph.json.filename <- "stringinteractions.cyjs"
style.json.filename <- "/Users/constantinosclark/Desktop/CING/CING-Project/cyjShiny/inst/demos/fromCytoscapeDesktop/simple/smallDemoStyle.json"
#----------------------------------------------------------------------------------------------------




mymtcars = mtcars
mymtcars$id = 1:nrow(mtcars)
#----------------------------------------------------------------------------------------------------
ui = shinyUI(fluidPage(
  dashboardPage(
    #these are random things i found and added maybe we can customize them later
    dashboardHeader(title = 'Network Library', 
                    
                    dropdownMenu(
                      type = 'messages', 
                      messageItem(from = 'Mike', message = 'This is a sample message')
                    ), #static example
                    dropdownMenuOutput('messageMenu'), #dynamic example
                    dropdownMenu(
                      type = 'notifications', 
                      notificationItem(text = 'This is a test notification')
                    ),
                    dropdownMenu(
                      type = 'tasks', 
                      taskItem(text='This is a test task', value = 50)
                    )), 
    dashboardSidebar(
      sidebarMenu(
        menuItem(text='Networks', tabName = 'networks',{menuItem(text='View', tabName = 'viewer')}, {menuItem(text='Analyze', tabName = 'analyzer')}), 
        menuItem(text='')
      )
    ),
    dashboardBody(
      cyjShinyOutput('cyjShiny'),
      width=10,
      DT::dataTableOutput('mytable'),
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
  
  
  output$mytable = DT::renderDataTable({
    addCheckboxButtons <- paste0('<input type="checkbox" name="row', mymtcars$id, '" value="', mymtcars$id, '">',"")
    #Display table with checkbox buttons
    DT::datatable(cbind(Pick=addCheckboxButtons, mymtcars[, input$show_vars, drop=FALSE]),
                  options = list(orderClasses = TRUE,
                                 lengthMenu = c(5, 25, 50),
                                 pageLength = 25, 
                                 callback = JS("function(table) {
    table.on('change.dt', 'tr td input:checkbox', function() {
          setTimeout(function () {
          Shiny.onInputChange('rows', $(this).add('tr td input:checkbox:checked').parent().siblings(':last-child').map(function() {
          return $(this).text();
          }).get())
          }, 10); 
          });
          }")),escape = FALSE,
                  
    )
  } 
  )
  
  
} # server
#----------------------------------------------------------------------------------------------------
runApp(shinyApp(ui=ui, server=server), port=9999)

