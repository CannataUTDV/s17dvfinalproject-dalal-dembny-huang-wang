require(readr)
require(plyr)

# Set the Working Directory to the 00 Doc folder

file_path = "../01 Data/PreETL_MediaReadyActiveCases.csv"
df <- readr::read_csv(file_path)


measures <- c("childid", "height-in", "weight","ncmeccasenumber")
dimensions <- setdiff(names(df), measures)


for(n in names(df)) {
  df[n] <- data.frame(lapply(df[n], gsub, pattern="[^ -~]",replacement= ""))
}


if( length(dimensions) > 0) {
  for(d in dimensions) {
    df[d] <- data.frame(lapply(df[d], gsub, pattern="-",replacement=""))
    df[d] <- data.frame(lapply(df[d], gsub, pattern="&",replacement= " and "))
    df[d] <- data.frame(lapply(df[d], gsub, pattern="NULL",replace=""))
  }
}

na2zero <- function (x) {
  x[is.na(x)] <- "0"
  return(x)
}

if( length(measures) > 1) {
  for(m in measures) {
    df[m] <- data.frame(lapply(df[m], na2zero))
    df[m] <- data.frame(lapply(df[m], gsub, pattern="[^.,0-9,e]",replacement= ""))
    df[m] <- data.frame(lapply(df[m], function(x) as.numeric(as.character(x)))) 
    
  }
}

write.csv(df, gsub("PreETL_", "", file_path), row.names=FALSE, na = "")

