library(RMySQL)

splitCommaSeparateColumn = function(maxrows) {
  con <- dbConnect(MySQL(), user="root", password="", dbname="vesale", host="localhost")
  
  query=gsub("maxrows",maxrows,"select id,comma from vesale.testTable limit maxrows");
  rs <- dbSendQuery(con,query)
  data <- fetch(rs, n=-1)
  
  #Transform comma separated column
  data2<-do.call(rbind, strsplit(gsub('[ ]|, |,[ ]',',',data[,2]),','))
  data2<-matrix(as.numeric(unlist(data2)),nrow=nrow(data2))
  data3<-cbind(data[,1],data2)
  View(data3)
  dbClearResult(rs)
  dbDisconnect(con)
}
