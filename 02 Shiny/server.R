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

shinyServer(function(input, output) { 
  
  # These widgets are for the Barcharts tab.
  online2 = reactive({input$rb2})
  output$races2 <- renderUI({selectInput("selectedraces", "Choose Races:", race_list, multiple = TRUE, selected='All') })
  
  
# The following query is for the select list in the Barcharts -> Barchart with Table Calculation tab.
races = query(
  data.world(propsfile = "www/.data.world"),
  dataset="carolhuang0502/s-17-dv-project-6", type="sql",
  query="select distinct race as D, race as R
  from MediaReadyActiveCases
  order by 1"
) # %>% View()
if(races[1] == "Server error") {
  print("Getting races from csv")
  file_path = "../01 Data/MediaReadyActiveCases.csv"
  df <- readr::read_csv(file_path) 
  tdf1 = df %>% dplyr::distinct(race) %>% arrange(race) %>% dplyr::rename(D = race)
  tdf2 = df %>% dplyr::distinct(race) %>% arrange(race) %>% dplyr::rename(R = race)
  races = bind_cols(tdf1, tdf2)
}
race_list <- as.list(races$D, races$R)
race_list <- append(list("All" = "All"), race_list)

# The following query is for the select list in the Barcharts -> Medium Bachelors Degree Level
degrees = query(
  data.world(propsfile = "www/.data.world"),
  dataset="carolhuang0502/s-17-dv-project-6", type="sql",
  query="SELECT sex, casetype, missingfromcity, educationqueryresult.State, bachelors_degree, state_lat_long.State as state, Latitude, Longitude, sum(bachelors_degree) as sum_bac
  FROM MediaReadyActiveCases s inner join
  educationqueryresult a
  ON (s.missingfromstate = a.State) inner join 
  state_lat_long c
  ON (s.missingfromstate = c.State)
  where c.State in (a.State)
  group by missingfromstate
  having sum(bachelors_degree)> 32000000 and sum(bachelors_degree)< 490000000"
  
)  # %>% View()

# The following query is for hte select list in the Barcharts -> Missing By Year
sales = query(
  data.world(propsfile = "www/.data.world"),
  dataset="carolhuang0502/s-17-dv-project-6", type="sql",
  query="select CAST (year(CAST (missingreporteddate AS date)) as string) as year, count(childid) as record
      from MediaReadyActiveCases
  group by CAST (year(CAST (missingreporteddate AS date)) as string)
  order by year"
) # %>% View()


  # Begin Barchart Tab ------------------------------------------------------------------
  df2 <- eventReactive(input$click2, {
    if(input$selectedraces == 'All') race_list <- input$selectedraces
    else race_list <- append(list("Skip" = "Skip"), input$selectedraces)
    if(online2() == "SQL") {
      print("Getting from data.world")
      tdf = query(
        data.world(propsfile = "www/.data.world"),
        dataset="carolhuang0502/s-17-dv-project-6", type="sql",
        query="select missingfromstate, race, count(childid) count_childid
        from MediaReadyActiveCases
        where ? = 'All' or race in (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
        group by missingfromstate, race",
        queryParameters = race_list
      ) # %>% View()
    }
    else {
      print("Getting from csv")
      file_path = "../01 Data/MediaReadyActiveCases.csv"
      df <- readr::read_csv(file_path)
      tdf = df %>% dplyr::filter(race %in% input$selectedraces | input$selectedraces == "All") %>%
        dplyr::group_by(missingfromstate, race) %>% 
        dplyr::summarize(count_childid = count(childid)) # %>% View()
    }
    # The following two lines mimic what can be done with Analytic SQL. Analytic SQL does not currently work in data.world.
    tdf2 = tdf %>% group_by(missingfromstate) %>% summarize(window_count_childid = mean(count_childid))
    dplyr::inner_join(tdf, tdf2, by = "missingfromstate")
    # Analytic SQL would look something like this:
    # select missingfromstate, race, count_childid, avg(count_childid) 
    # OVER (PARTITION BY missingfromstate ) as window_avg_childid
    # from (select missingfromstate, race, count(childid) count_childid
    #       from MediaReadyActiveCases
    #      group by missingfromstate, race)
  })
  
  output$barchartData1 <- renderDataTable({DT::datatable(df2(),
                                                         rownames = FALSE,
                                                         extensions = list(Responsive = TRUE, FixedHeader = TRUE) )
  })
  output$barchartData2 <- renderDataTable({DT::datatable(degrees,
                                                         rownames = FALSE,
                                                         extensions = list(Responsive = TRUE, FixedHeader = TRUE) )
  })
  output$barchartData3 <- renderDataTable({DT::datatable(sales,
                                                         rownames = FALSE,
                                                         extensions = list(Responsive = TRUE, FixedHeader = TRUE) )
  })
  output$barchartPlot1 <- renderPlot({ggplot(df2(), aes(x=race, y=count_childid)) +
      scale_y_continuous(labels = scales::comma) + # no scientific notation
      theme(axis.text.x=element_text(angle=0, size=12, vjust=0.5)) + 
      theme(axis.text.y=element_text(size=12, hjust=0.5)) +
      geom_bar(stat = "identity") + 
      facet_wrap(~missingfromstate, ncol=1) + 
      coord_flip() + 
      # Add count_childid, and (count_childid - window_avg_childid) label.
      geom_text(mapping=aes(x=race, y=count_childid, label=round(count_childid, digits = 1)),colour="black", hjust=-.5) +
      geom_text(mapping=aes(x=race, y=count_childid, label=round(window_count_childid - count_childid, digits = 1)),colour="blue", hjust=-2) +
      # Add reference line with a label.
      geom_hline(aes(yintercept = round(window_count_childid)), color="red") +
      geom_text(aes( -1, window_count_childid, label = window_count_childid, vjust = -.5, hjust = -.25), color="red")
  })
  output$barchartMap1 <- renderLeaflet({leaflet(width = 400, height = 800) %>% 
      setView(lng = -98.35, lat = 39.5, zoom = 4) %>% 
      addTiles() %>% 
      addMarkers(lng = degrees$Longitude,
                 lat = degrees$Latitude,
                 options = markerOptions(draggable = TRUE, riseOnHover = TRUE),
                 popup = as.character(paste("City: " , degrees$missingfromcity, 
                                            ", State: ", degrees$missingfromcity,
                                            ", Case Type: ", degrees$casetype,
                                            ", Sex: ", degrees$sex
                                            )) )
  })
  output$barchartPlot2 <- renderPlotly({p <- ggplot(sales, aes(x=as.character(year), y=record)) +
    theme(axis.text.x=element_text(angle=0, size=7, vjust=0.5)) + 
    theme(axis.text.y=element_text(size=12, hjust=0.5)) +
    geom_bar(stat = "identity")+
    xlab("\nYear")+ylab("\nRecord")
  ggplotly(p)
  })
  
  # End Barchart Tab ___________________________________________________________
  
  })