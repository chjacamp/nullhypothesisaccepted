library(lubridate)
head(date_list)
fucking_floor <- function(date){
for(i in 1:length(date))
  if(month(date[i])==3){
    if(day(date[i]) < 3){
      month(date[i]) <- 2
      day(date[i]) <- 16
    }
    else if(day(date[i]) <= 16){
      day(date[i]) <- 3
    }
    else {
      day(date[i]) <- 17
    }
  }
  else if(day(date[i]) <= 15){
    day(date[i]) <- 1
  }
  else{
    day(date[i]) <- 16
  }
  return(date)
}
date_list
sup <- data.frame(floor = fucking_floor(date_list),  date = date_list) 
