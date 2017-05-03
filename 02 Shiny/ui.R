#ui.R
require(shiny)
require(shinydashboard)
require(DT)
require(leaflet)
require(plotly)

dashboardPage(
  dashboardHeader(
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Barcharts, Table Calculations", tabName = "barchart", icon = icon("dashboard")),
      menuItem("Scatterplots", tabName = "Scatterplot", icon = icon("dashboad"))
    )
  ),
  dashboardBody(    
    tabItems(
      # Begin Barchart tab content.
      tabItem(tabName = "barchart",
        tabsetPanel(
          tabPanel("Data",  
             radioButtons("rb2", "Get data from:",
                          c("SQL" = "SQL",
                            "CSV" = "CSV"), inline=T),
             uiOutput("races2"), # See http://shiny.rstudio.com/gallery/dynamic-ui.html
             actionButton(inputId = "click2",  label = "To get data, click here"),
             hr(), # Add space after button.
             'Here is data for the "Barchart with Table Calculation" tab',
             hr(),
             DT::dataTableOutput("barchartData1"),
             hr(),
             'Here is data for the "Medium Bachelors Degree Level" tab',
             hr(),
             DT::dataTableOutput("barchartData2"),
             hr(),
             'Here is data for the "Missing by Year" tab',
             hr(),
             DT::dataTableOutput("barchartData3")
          ),
          tabPanel("Race of Missing Children per State with Table Calculation", "Black = Count of Children per Race, Red = Average Count of Children per missingfromstate, and  Blue = (Average Count of Children per Race - Count of Children per Race)", plotOutput("barchartPlot1", height=6000)),
          tabPanel("Medium Bachelors Degree Level", leafletOutput("barchartMap1"), height=900 ),
          tabPanel("Missing by Year", plotlyOutput("barchartPlot2",width=1300,height=800) )
        )
      ),
      # End Barchart tab content.
      # Start of Scatterplot Content
      tabItem(tabName = "Scatterplot",
              tabsetPanel(
                tabPanel("Data",  
                         radioButtons("rb2", "Get data from:",
                                      c("SQL" = "SQL", inline=T),
                         uiOutput("races2"), # See http://shiny.rstudio.com/gallery/dynamic-ui.html
                         actionButton(inputId = "click2",  label = "To get data, click here"),
                         hr(), # Add space after button.
                         'Here is data for the "Scatterplot" tab',
                         hr(),
                         DT::dataTableOutput("ScatterData1"))
                         )
                     )
      # End Barchart tab content.
    )
  )
)
)


