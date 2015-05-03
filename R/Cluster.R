
#-------------------------------------
# 1. Process subset
#-------------------------------------

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
  con <- connectDB()
  query=gsub("startPage",startPage,"SELECT imageID, vector FROM vesale.imgDescriptor join image_index using(imageID) where page>=startPage and page<endPage");
  query=gsub("endPage",endPage,query);
  query=gsub("imgDescriptor",imgDescriptor,query);
  rs <- dbSendQuery(con,query)
  data <- fetch(rs, n=-1)
  
  dbClearResult(rs)
  dbDisconnect(con)
  
  #Transform comma separated column
  data<-cbind(data[,1],splitCSVColumn(data[,2]))
  colnames(data)<-c("imageId",colnames(data, do.NULL = FALSE)[-1])
  data
}


#-------------------------------------
# 2. CLUSTERING
#-------------------------------------


#-------------------------------------
# KMEANS
#-------------------------------------

#' KMean cluster over a dataset
#' @param sData subset of the data
#' @param k number of clusters
#' @param iter number of iterations
#' @return kmean cluster 
#' @examples
#' cl<-clusterKM(sData,50,100)
#' gcl<-clusterKM(sData,20,100
clusterKM = function(sData,k,iter) {
  cl <-kmeans(scale(sData[,-1]), k, iter.max = iter, nstart = 1, algorithm = c("Hartigan-Wong", "Lloyd", "Forgy","MacQueen"), trace=FALSE)
}

#' Create a label of the kmean
#' @param cl kmean cluster
#' @param data dataframe
#' @param label of the cluster: E.g kmeans k: 50 between: 982683.05 withinss: 222461.94 totss: 1205145
#' @example
#' labelKMean(cl)
#' @return kmeans k: 50 between: 982683.05 withinss: 222461.94 totss: 1205145
labelKMean=function(cl)
{
  label<-paste (attr(cl,"class"),"k:",length(cl$size),"between:",cl$between,"withinss:",cl$tot.withinss,"totss:",cl$totss, sep = " ", collapse = NULL)
}

#-------------------------------------
# CLARA PAM
#-------------------------------------


#' Clara: Clustering algorithm for large datasets using PAM 
#' @param sample size <1000 (performance issues)
#' @param data dataframe
#' @example
#' claraPAM(100)
clusterClaraPAM=function(sample)
{
  nSim <- 100
  nCl <- 3 # = no.classes
  set.seed(421)# (reproducibility)
  cl <- matrix(NA,nrow(xclara), nSim)
  for(i in 1:nSim)
    cl[,i] <- clara(xclara, nCl, medoids.x = FALSE, rngR = TRUE)$cluster
  
}

#-------------------------------------
# 3. VALIDATION
#-------------------------------------
#' Calculate cluster metrics
#' http://www.jstatsoft.org/v25/i04/paper
#' @param cl kmean cluster
#' @param data dataframe
#' @param label of the cluster: E.g kmeans k: 50 between: 982683.05 withinss: 222461.94 totss: 1205145
#' @example
#' labelKMean(cl)
#' @return kmeans k: 50 between: 982683.05 withinss: 222461.94 totss: 1205145
clusterValidation=function(data,cluster)
{
  dl<-dist(data[,-1])
  sil<<-cluster::silhouette(x = cluster, dist = dl)  
  index.list<-cls.scatt.data(data[,-1], cl$cluster, dist="euclidean")
  intraclust = c("complete","average","centroid")
  interclust = c("single", "complete", "average")
  dunn1 <- clv.Dunn(index.list, intraclust, interclust)
  summary(dunn1)
}

#-------------------------------------
# 3. UPLOAD CLUSTER
#-------------------------------------

#' Upload a cluster into the database
#' @param cluster Vector of the cluster assigment for each point in the dataset
#' @param data dataframe
#' @param label of the cluster: E.g kmeans k: 50 between: 982683.05 withinss: 222461.94 totss: 1205145
#' @examples
#' uploadClusterDB(l,data,"k mean k=10,descriptor)
#' uploadClusterDB(l,data,"k mean",descriptor)
uploadClusterDB=function(cluster,data,label,descriptor)
{
  #Connect to database
  con <- connectDB()
  
  #Insert new cluster result into cluster_index table
  
  query=gsub("labK",label,"INSERT INTO vesale.cluster_index(cluster_label,activity_date) VALUES ('labK',now())")
  query=gsub("descriptorV",descriptor,query)
  
  rs<-dbSendQuery(con,query)
  
  #Get the id of the new cluster
  rs <- dbSendQuery(con,"SELECT LAST_INSERT_ID()")
  cid <- fetch(rs, n=-1)
  
  #Insert new cluster values into image_cluster table
  data<-cbind(data[,1],cluster,cid)
  colnames(data)<-c("imageId","cluster","clusterID")
  dbWriteTable(con,"image_cluster",data,overwrite=T)
  
  #Close mysql connection
  dbClearResult(rs)
  dbDisconnect(con)
}

#-------------------------------------
# Orchestrator
#-------------------------------------
orchestrator=function()
{
  #Example KMEANS
  getDescriptors()
  #Get subset
  data<-getSubset(descriptors[1,3],5,10)
  cl<-clusterKM(data,50,100)
  uploadClusterDB(cl$cluster,data,labelKMean(cl),descriptors[1,3])
}

#-------------------------------------
# Miscelaneos
#-------------------------------------

#' Remove all variables from memory
#' @example
#' clearVariables()
clearVariables=function()
{
  rm(list=ls())
}

getDescriptors=function()
{
  con <- connectDB()
  query="SELECT * FROM vesale.descriptors";
  rs <- dbSendQuery(con,query)
  descriptors <<- fetch(rs, n=-1)
  print(descriptors)
}

connectDB=function()
{
   library(RMySQL)
   #dbConnect(MySQL(), user="root", password="", dbname="vesale", host="localhost")
   con <- dbConnect(MySQL(), user="mysqluser", password="", dbname="vesale", host="164.15.78.25")
}
  
