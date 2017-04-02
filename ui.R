dashboardPage(
  dashboardHeader(title = "Housing Prices App"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Housing Prices", tabName = "housing_prices", icon = icon("home"))
    ),
    sliderInput(inputId = 'year', 
                label = 'Time Range',
                sep = '',
                width = "1000px",
                step = .25,
                min = min(HPI_AT_metro$Year_Quarter_num),
                max = max(HPI_AT_metro$Year_Quarter_num),
                value = range(HPI_AT_metro$Year_Quarter_num)),
    selectInput(inputId = 'state_a',
                selectize = FALSE,
                size = 1,
                label = 'State A',
                choices = unique(HPI_AT_metro$State)[order(unique(HPI_AT_metro$State))],
                selected = c('FL')),
    uiOutput('Area_ui_a'),
    selectInput(inputId = 'state_b',
                selectize = FALSE,
                size = 1,
                label = 'State B',
                choices = unique(HPI_AT_metro$State)[order(unique(HPI_AT_metro$State))],
                selected = c('CT')),
    uiOutput('Area_ui_b')),
  
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "housing_prices")),
    fluidRow(
      plotOutput('housing_plot', hover = 'housing_plot_hover'), 
      htmlOutput('housing_plot_hover')
    )
  )
)
