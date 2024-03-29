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
      menuItem("Box Plots", tabName = "boxplot", icon = icon("dashboard")),
      menuItem("Histograms", tabName = "histogram", icon = icon("dashboard")),
      menuItem("Scatter Plots", tabName = "scatter", icon = icon("dashboard")),
      menuItem("Crosstabs, KPIs, Parameters", tabName = "crosstab", icon = icon("dashboard")),
      menuItem("Barcharts, Table Calculations", tabName = "barchart", icon = icon("dashboard"))
    )
  ),
  dashboardBody(
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"
    ),    
    tabItems(
      # Begin Box Plots tab content.
      tabItem(tabName = "boxplot",
              tabsetPanel(
                tabPanel("Data",  
                         
                         DT::dataTableOutput("boxplotData1")
                ),
                tabPanel("Simple Box Plot", 
                         
                         plotlyOutput("boxplotPlot1", height=500))
              )
      ),
      # End Box Plots tab content.
      # Begin Histogram tab content.
      tabItem(tabName = "histogram",
              tabsetPanel(
                tabPanel("Data",  
                         
                         DT::dataTableOutput("histogramData1"),
                         hr(),
                         DT::dataTableOutput("histogramData2")
                ),
                tabPanel("Histogram 1", plotOutput("histogramPlot1", height=900)),
                tabPanel("Histogram 2 ", plotOutput("histogramPlot2", height=900))
              )
      ),
      # End Histograms tab content.
      # Begin Scatter Plots tab content.
      tabItem(tabName = "scatter",
              tabsetPanel(
                tabPanel("Data",  
                         radioButtons("rb3", "Get data from:",
                                      c("SQL" = "SQL",
                                        "CSV" = "CSV"), inline=T),
                         uiOutput("scatterStates"), # See http://shiny.rstudio.com/gallery/dynamic-ui.html,
                         actionButton(inputId = "click3",  label = "To get data, click here"),
                         hr(), # Add space after button.
                         DT::dataTableOutput("scatterData1")
                ),
                tabPanel("Simple Scatter Plot", plotOutput("scatterPlot1", height=900))
              )
      ),
      # End Scatter Plots tab content.
      # Begin Crosstab tab content.
      tabItem(tabName = "crosstab",
              tabsetPanel(
                tabPanel("Data",  
                         radioButtons("rb1", "Get data from:",
                                      c("SQL" = "SQL"), inline=T),
                         sliderInput("income_below_25k_low", "income_below_25k_Low:", 
                                     min = 0, max = .15,  value = .15),
                         sliderInput("income_below_25k_medium", "income_below_25k_Medium:", 
                                     min = .15, max = .3,  value = .3),
                         actionButton(inputId = "click1",  label = "To get data, click here"),
                         hr(), # Add space after button.
                         DT::dataTableOutput("data1")
                ),
                tabPanel("Crosstab", plotOutput("plot1", height=1000))
              )
      ),
      # End Crosstab tab content.
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
                         hr()
                ),
                tabPanel("Race of Missing Children per State with Table Calculation", "Black = Count of Children per Race, Red = Average Count of Children per missingfromstate, and  Blue = (Average Count of Children per Race - Count of Children per Race)", plotOutput("barchartPlot1", height=6000)),
                tabPanel("Medium Bachelors Degree Level", leafletOutput("barchartMap1"), height=900 )
              )
      )
      # End Barchart tab content.
    )
  )
)