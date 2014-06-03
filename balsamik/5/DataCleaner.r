
#dd <-getTidyData() 






  data <- read.csv("balsamik.csv",header=TRUE)
  data[is.na(data)] <- c("x")
  #"date","TypeWeekDay","length","channel","DAYPART","MMDAYPART","ChaineEcranWD","ChaineDaypartWD","ChaineMMDaypartWD","crea",
  #"budgetbrut","budgetnet","grp","grpref","tav","tavtap","EmavShort","hour","year","month","dayofweek","Visite.gain","datetime"
  s <- c("date","Visite.gain","TypeWeekDay","length","channel","DAYPART","crea","screen"
         ,"budgetbrut","budgetnet","grp","grpref","tav","tavtap","emav","emap",
         "hour","month","dayofweek","consumption","nthweek")
  
  library(reshape2)
  data <- data[c(1:2200,3037:3069),]
  data <- data[data$isole == 1,]
  data$Visite.gain <- data$Visites.Apres - data$Visites.Avant
  #print(tail(data,n = 50))
  #19/01/2014  07:13:00
  
  energy <- read.csv("energy_byhours.csv",header=TRUE)
  names(energy) <- gsub("X", "", names(energy))
  names(energy) <- gsub("\\.", ":", names(energy))
  #energy <- melt(energy, date=c("date"))
  
  newenergy <- reshape(energy, 
                       varying = names(energy)[-1], 
                       v.names = "consumption",
                       timevar = "time", 
                       times = names(energy)[-1], 
                       direction = "long")
  energy <- newenergy[order(newenergy$Date),]
  
  energy$Date <- paste(energy$Date,energy$time)
 
  energy <- energy[,c(-2,-4)]
  rownames(energy) <- NULL
  
  energy$Date <- format(as.POSIXlt(energy$Date,format="%d/%m/%Y %H:%M"), "%Y-%m-%d %H:%M")
  
  #data$datetime <- paste(data$date,data$datetime)

  data <- within(data,{
    posb <- as.POSIXlt(datetime,format="%d/%m/%Y %H:%M")
    
    x <- (posb$min < 30)
    posb$min[x] <- 30
    y <- (posb$min > 30 )
    posb$min[y] <- 0
    posb$hour[y] <- posb$hour[y] + 1
    
    
    Date <- format(posb, "%Y-%m-%d %H:%M")
    #nthweek <- format(as.Date(posb), "%W")
    posb <- NULL  # cleanup
    x <- NULL
    y <- NULL
  })
  print(head(data))
  data$ord <- 1:nrow(data) 
  data <-merge(data,energy,by="Date")
  data <- data[order(data$ord),]
  print(head(data))
  data <- data[,s]
 
  #add festival
  festival <- read.csv("festival.csv",header=FALSE)
  names(festival) <- c("Holidays","CntToHolidays","CntAfterHolidays","date")
  data$ord <- 1:nrow(data) 
  data<-merge(data,festival,by = "date")
  data <- data[order(data$ord),]

  print(head(data))
  
#   #add gta
#   gta <- read.csv("advisor.csv",header=FALSE)
#   names(gta) <- c("date","gta")
#   data<-merge(data,gta,by = "date")
#   print(nrow(data))
  #add hour_visit
  data$ord <- 1:nrow(data) 
  hh <- read.csv("Book1.csv",header=TRUE)
  names(hh) <- c("hour","visitByhour")
  data<-merge(data,hh,by = "hour")
  data <- data[order(data$ord),]
  data$ord <- NULL
  #print(nrow(data))
  creation <- unique(data$crea)
  data$cumGrp <- 0
  data$cumGrpRef <- 0
  data$cumGrpTotal <- cumsum(data[,c('grp')])
  data$cumGrpRefTotal <- cumsum(data[,c('grpref')])
  for (crea in  creation){
    data[data$crea == crea,c('cumGrp')] <- cumsum(data[data$crea == crea,c('grp')])
    data[data$crea == crea,c('cumGrpRef')] <- cumsum(data[data$crea == crea,c('grpref')])
  }
 
  print(head(data))
  write.csv(data, "tidyData.csv", row.names=FALSE)



#ddd<-getTidyData()


