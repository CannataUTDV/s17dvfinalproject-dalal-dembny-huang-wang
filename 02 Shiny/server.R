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
  
  online1 = reactive({input$rb1})
  KPI_Low = reactive({input$KPI1})     
  KPI_Medium = reactive({input$KPI2})
  
  # These widgets are for the Barcharts tab.
  online2 = reactive({input$rb2})
  output$races2 <- renderUI({selectInput("selectedraces", "Choose Races:", race_list, multiple = TRUE, selected='All') })
  
  
  # The following query is for the select list in the Barcharts -> Barchart with Table Calculation tab.
  races = query(
    data.world(propsfile = "www/.data.world"),
    dataset="carolhuang0502/s-17-dv-final-project", type="sql",
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
    dataset="carolhuang0502/s-17-dv-final-project", type="sql",
    query="select CAST (year(CAST (missingreporteddate AS date)) as string) as year, count(childid) as record
    from MediaReadyActiveCases
    group by CAST (year(CAST (missingreporteddate AS date)) as string)
    order by year"
  ) # %>% View()
  

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

  # Begin Histogram Tab ------------------------------------------------------------------
  dfh1 <-   query(
    data.world(propsfile = "www/.data.world"),
    dataset="carolhuang0502/s-17-dv-final-project", type="sql",
    query="select year
    from Year_query") # %>% View()
  output$histogramData1 <- renderDataTable({DT::datatable(dfh1,
                                                       rownames = FALSE,
                                                       extensions = list(Responsive = TRUE, FixedHeader = TRUE) )
  })
  output$histogramPlot1 <- renderPlot({ggplot(dfh1) + 
      geom_histogram(aes(x=dfh1), binwidth = 10, fill = I("blue")) + labs(x = "Decade", y = "Count") + 
      theme(axis.text.x=element_text(angle=90, size=10, vjust=0.5))
  })
  
  dfh2 <-   query(
    data.world(propsfile = "www/.data.world"),
    dataset="carolhuang0502/s-17-dv-final-project", type="sql",
    query="select weight, race 
    from MediaReadyActiveCases
    where weight < 500") # %>% View()
  output$histogramData2 <- renderDataTable({DT::datatable(dfh2,
                                                          rownames = FALSE,
                                                          extensions = list(Responsive = TRUE, FixedHeader = TRUE) )
  })
  output$histogramPlot2 <- renderPlot({ggplot(dfh2) + geom_histogram(aes(x=weight, fill=race, color="black"), binwidth = 10) + labs(x = "Weight (lbs)", y = "Count")+
    theme(axis.text.x=element_text(angle=90, size=10, vjust=0.5))
    
  })

  # End Histogram Tab ___________________________________________________________
  # Begin Scatterplot Tab ------------------------------------------------------------------
  dfs1 <- eventReactive(input$click3, {
    print("Getting from data.world")
    query(
      data.world(propsfile = "www/.data.world"),
      dataset="carolhuang0502/s-17-dv-final-project", type="sql",
      query="select `height-in` as height, weight, sex
      from MediaReadyActiveCases"
    ) 
  })
  
  output$scatterData1 <- renderDataTable({DT::datatable(dfs1(), rownames = FALSE, extensions = list(Responsive = TRUE, FixedHeader = TRUE))})
  output$scatterPlot1 <- renderPlot({ggplot(dfs1(),aes(x=weight, y=height, color = sex))+
      geom_point(shape = 1) +
      geom_smooth(method=lm, se=FALSE, fullrange=TRUE)
  })
  # End Scatterplot Tab ___________________________________________________________
  # Begin Crosstab 1 Tab ------------------------------------------------------------------
  df1 <- eventReactive(input$click1, {
    
    print("Getting from data.world")
    query(
      data.world(propsfile = "www/.data.world"),
      dataset="carolhuang0502/s-17-dv-final-project", type="sql",
      query="select missingfromstate, race, count, hh_0k_to_25k,total_households, hh_0k_to_25k/total_households as Perc_Below_25k,
      
      case
      when hh_0k_to_25k/total_households < ? then '03 Low'
      when hh_0k_to_25k/total_households < ? then '02 Medium'
      else '01 High'
      end AS kpi
      
      from cleanedracedata
      left join incomequeryresult
      on missingfromstate= State",
      queryParameters = list(KPI_Low(), KPI_Medium())
      
    ) 
    
    
  })
  output$data1 <- renderDataTable({DT::datatable(df1(), rownames = FALSE,
                                                 extensions = list(Responsive = TRUE, FixedHeader = TRUE)
  )
  })
  output$plot1 <- renderPlot({ggplot(df1()) + 
      theme(axis.text.x=element_text(angle=90, size=16, vjust=0.5)) + 
      theme(axis.text.y=element_text(size=16, hjust=0.5)) +
      geom_tile(aes(x=race, y=missingfromstate, fill = kpi), size=6)+
      geom_text(aes(x=race, y=missingfromstate, label=count), size=6) 
    
  })
  # End Crosstab 1 Tab ___________________________________________________________
  
  # Begin Barchart Tab ------------------------------------------------------------------
  df2 <- eventReactive(input$click2, {
    if(input$selectedraces == 'All') race_list <- input$selectedraces
    else race_list <- append(list("Skip" = "Skip"), input$selectedraces)
    if(online2() == "SQL") {
      print("Getting from data.world")
      tdf = query(
        data.world(propsfile = "www/.data.world"),
        dataset="carolhuang0502/s-17-dv-final-project", type="sql",
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
