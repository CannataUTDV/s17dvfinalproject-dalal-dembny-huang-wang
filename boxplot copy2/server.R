# server.R
require(ggplot2)
require(dplyr)
require(shiny)
require(shinydashboard)
require(data.world)
require(readr)
require(DT)
require(leaflet)
require(plotly)
require(lubridate)

online0 = TRUE

# Server.R structure:
#   Queries that don’t need to be redone
#   shinyServer
#   widgets
#   tab specific queries and plotting

# The following query is for the select list in the Boxplots -> Simple Boxplot tab, and Barcharts -> Barchart with Table Calculation tab.

############################### Start shinyServer Function ####################

shinyServer(function(input, output) {   
  
  # Begin Box Plot Tab ------------------------------------------------------------------
  dfbp1 <- 

      query(
        data.world(propsfile = "www/.data.world"),
        dataset="carolhuang0502/s-17-dv-final-project", type="sql",
        query="select weight,race,missingfromdate
        from MediaReadyActiveCases
        where weight<400") # %>% View()
    
  dfbp3 <- eventReactive(c(input$click5, input$range5a), {
    dfbp1 %>% dplyr::filter(lubridate::year(missingfromdate) == as.integer(input$range5a)) 
    })
  output$boxplotData1 <- renderDataTable({DT::datatable(dfbp3, rownames = FALSE,
                                                extensions = list(Responsive = TRUE, 
                                                FixedHeader = TRUE)
  )
  })
  

  output$boxplotPlot1 <- renderPlotly({
    
    p <- ggplot(dfbp3) + 
      geom_boxplot(aes(x=race, y=weight))
    ggplotly(p)
  })
  # End Box Plot Tab ___________________________________________________________
  
})
