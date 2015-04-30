#' Test Split a comma separated column into a matrix.
#' @param maxrows Maximum numbers of rows to pull from mysql.
#' @return matrix resulted from the split.
#' @examples
#' splitCSVColumn("1,2,4,5")
#' splitCSVColumn(data[,2])
testCSVSQL = function(maxrows) {
  library(RMySQL)
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

#' Split a comma separated column into a matrix.
#' @param column or vector with the comma separated values.
#' @return matrix resulted from the split.
#' @examples
#' splitCSVColumn("1,2,4,5")
#' splitCSVColumn(data[,2])
splitCSVColumn= function(column) {
  tdata<-do.call(rbind, strsplit(gsub('[ ]|, |,[ ]',',',column),','))
  tdata<-matrix(as.numeric(unlist(tdata)),nrow=nrow(tdata))
}
  
#' Fetch the data for a given image Descriptor (imgDescriptor) for a page range
#' @param imgDescriptor in {SIFT, etc.}
#' @param startPage, start page (of the book) of the range (>=startPage)
#' @param endPage, end page (of the book) of the range (<endPage)
#' @return matrix resulted from the split.
#' @examples
#' getSubset("SIFT")
#' getSubset(data[,2])
getSubset = function(imgDescriptor,startPage,endPage) {
  library(RMySQL)
  con <- dbConnect(MySQL(), user="root", password="", dbname="vesale", host="localhost")
  query=gsub("startPage",startPage,"select * from vesale.image_index where page>=startPage and page<endPage");
  query=gsub("endPage",endPage,query);
  
  rs <- dbSendQuery(con,query)
  data <- fetch(rs, n=-1)
  
  dbClearResult(rs)
  dbDisconnect(con)
  
  #Transform comma separated column
  #data<-cbind(data[,1],splitCSVColumn(data[,2]))
  #colnames(data)<-c("imageId",colnames(data, do.NULL = FALSE)[-1])
  
  View(data)
}


