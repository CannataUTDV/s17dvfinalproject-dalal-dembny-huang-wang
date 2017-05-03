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

  
  output$boxplotData1 <- renderDataTable({DT::datatable(dfbp1, rownames = FALSE,
                                                extensions = list(Responsive = TRUE, 
                                                FixedHeader = TRUE)
  )
  })
  


  output$boxplotPlot1 <- renderPlotly({
    means <- aggregate(weight ~ race,dfbp1, mean)
    
    rounded_means <- round(means$weight,1)
    
    means$weight <- rounded_means
    p <- ggplot(dfbp1, aes(x=race,y=weight)) + 
      geom_boxplot() + 
      stat_summary(fun.y = "mean", colour="darkred", geom="point", shape=18, size=2,show_guide = FALSE) +
      geom_text(data = means, aes(label = weight, y = weight+10))
    ggplotly(p)
  })
  # End Box Plot Tab ___________________________________________________________
  
})
