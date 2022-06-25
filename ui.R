dashboardPage(
      dashboardHeader(title = 'Baseball app'), 
      dashboardSidebar(
        sidebarMenu(
          menuItem(text='Player data', tabName = 'playerData', {menuItem(text='Data', tabName = 'datatable')}, {menuItem(text='Plot', tabName = 'plot')}), 
          menuItem(text='Data per Team/Year', tabName = 'dpt'), 
          menuItem(text = 'Yearly leaders', tabName = 'yearlyLeaders'),
          #COOL*** because the data the options will be many - i chose selectizeinput for a server side making it much more efficeint
          selectizeInput(inputId = 'playerSelector', label='Select a player:', choices = NULL), 
          selectInput(inputId = 'statSelector', label = 'Select which stat to display: ', choices = columnChoices, selected = 'HR'), 
          selectInput(inputId = 'teams', label ='Select which team to view: ', choices = unique(read.csv('/Users/KyriakiCH1/Downloads/allBaseballData.csv')$franchName), selected='New York Yankees'), 
          selectInput(inputId = 'year', label= 'Select which year to view: ', choices = unique(read.csv('/Users/KyriakiCH1/Downloads/allBaseballData.csv')$yearID))
        )
      ),
      
      dashboardBody(
        tabItems(
          tabItem(tabName = 'datatable', DTOutput('dt3'), DTOutput('dt1')), 
          tabItem(tabName = 'plot', plotOutput('p1')),
          tabItem(tabName = 'dpt', DTOutput('dt2')), 
          tabItem(tabName = 'yearlyLeaders', plotOutput('p2'))
        )
      )
)
