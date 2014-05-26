clearTripAdvisor <- function(){
  library(DAAG)
  D3 <- read.csv("TripAdvisor//TripAdvisor.csv",header=FALSE,nrows = 607)
  names(D3) <- c("date","sessions","festival","energy.consumption")
  D3$date <- as.Date(D3$date)
  weekday <- as.numeric(format(D3$date,'%w'))
  nthweek <- as.numeric(format(D3$date,'%W'))
  month <- as.numeric(format(D3$date,'%m'))
  index<- 1:607
  D3<-data.frame(D3,weekday,nthweek,month,index)
  write.csv(D3, "TripAdvisor//tidyData.csv", row.names=FALSE)
  return (D3)
}