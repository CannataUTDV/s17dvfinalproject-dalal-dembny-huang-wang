require(ggplot2)
require(dplyr)
require(shiny)
require(shinydashboard)
require(data.world)
require(readr)
require(DT)
require(plotly)

dftest = query(
  data.world(propsfile = "www/.data.world"),
  dataset="carolhuang0502/s-17-dv-final-project", type="sql",
  query="select weight, race, missingfromstate
  from MediaReadyActiveCases
  where weight<400")
  
means <- aggregate(weight ~ race,dftest, mean)

rounded_means <- round(means$weight,1)

means$weight <- rounded_means

plot1 <- ggplot(dftest, aes(x=race,y=weight)) + 
  geom_boxplot() + 
  stat_summary(fun.y = "mean", colour="darkred", geom="point", shape=18, size=2,show_guide = FALSE) +
  geom_text(data = means, aes(label = weight, y = weight+10))

plot2 <- ggplotly(plot1)

print(plot2)
