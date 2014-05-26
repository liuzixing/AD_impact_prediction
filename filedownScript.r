#fileUrl <- paste("http://leadsmonitor.mymedia.fr/?download_log=",format(Sys.time(), "%Y%m%d%H%M%S"), "-Balsamik-leads-2014-04-13.log")
#download.file(fileUrl,dest = "./balsamik/2014-04-13.log")
#list.files("./balsamik")
library(data.table)
downloadFromBalsamik <- function(from,days){
  d<- seq(as.Date(from),by='days',length=days)
  res <- lapply(d,fdown)
}

fdown <- function (s){
  #x<-fread(paste0("data//",s,".tsv//187070.tsv"), sep='\t',header=FALSE)
  #fileUrl <- paste0("http://leadsmonitor.mymedia.fr/?download_log=",format(Sys.time(), "%Y%m%d%H%M00"), "-Balsamik-leads-",s,".log")
  download.file(fileUrl,dest = paste0("./balsamik/",s,".log"))
  #return (nrow(x))
}
#fileUrl <- "http://leadsmonitor.mymedia.fr/?page=calendrier&what=datespots&client=Balsamik&date=20131102"
#download.file(fileUrl,dest = paste0("./balsamik/20131102.log"))