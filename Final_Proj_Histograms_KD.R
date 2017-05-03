require(ggplot2)
require(dplyr)
require(data.world)
require(readr)
require(DT)

df1 <- {
  
  print("Getting from data.world")
  query(
    data.world(propsfile = "www/.data.world"),
    dataset="carolhuang0502/s-17-dv-final-project", type="sql",
    query="select year
    
    from Year_query"
  ) 
}

summary(df1)

head(df1)

p1 = ggplot(df1) + geom_histogram(aes(x=df1), binwidth = 10, fill = I("blue")) + labs(x = "Decade", y = "Count") + theme(axis.text.x=element_text(angle=90, size=10, vjust=0.5))
print(p1)

df2 = {
  
  print("Getting from data.world")
  query(
    data.world(propsfile = "www/.data.world"),
    dataset="carolhuang0502/s-17-dv-final-project", type="sql",
    query="select weight, race 

    from MediaReadyActiveCases
    
    where weight < 500"
  ) 
}

summary(df2)

head(df2)

p2 = ggplot(df2) + geom_histogram(aes(x=weight, fill=race, color="black"), binwidth = 10) + labs(x = "Weight (lbs)", y = "Count")
  theme(axis.text.x=element_text(angle=90, size=10, vjust=0.5))
print(p2)
