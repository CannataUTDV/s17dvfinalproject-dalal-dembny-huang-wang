require(readr)
require(lubridate)
require(dplyr)
require(data.world)

online0 = TRUE

if(online0) {
  globals = query(
    data.world(propsfile = "www/.data.world"),
    dataset="carolhuang0502/s-17-dv-final-project", type="sql",
    query="select missingfromdate
    from MediaReadyActiveCases
    where weight <= 400"
  ) 
} else {
  file_path = "www/SuperStoreOrders.csv"
  df <- readr::read_csv(file_path) 
  globals <- df %>% dplyr::select(Order_Date, Sales) %>% dplyr::distinct()
}
globals$missingfromdate <- lubridate::year(globals$missingfromdate)

