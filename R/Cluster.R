
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
#' Fetch the data for a given cluster algorithm
#' @param clusterID id of the clustering algorithm
#' @return matrix resulted from the split.
#' @examples
#' getValidationDataset(1000011)
#' getValidationDataset(1000011)
getValidationDataset = function(clusterID,descriptor) {
  con <- connectDB()
  query=gsub("desc2",descriptor,"SELECT imageid,cluster,vector FROM vesale.desc2 join image_cluster using(imageID) where clusterID=cl2");
  query=gsub("cl2",clusterID,query);
  rs <- dbSendQuery(con,query)
  data <- fetch(rs, n=-1)
  
  dbClearResult(rs)
  dbDisconnect(con)
  
  #Transform comma separated column
  data<-cbind(data[,1],splitCSVColumn(data[,3]))
  colnames(data)<-c("imageId","clusterID",colnames(data, do.NULL = FALSE)[-c(1,2)])
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
#' @examples
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
#' @examples
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
#' @examples
#' labelKMean(cl)
#' @return kmeans k: 50 between: 982683.05 withinss: 222461.94 totss: 1205145
clusterValidation=function(data,cluster)
{
  library(clusterSim)
  
  dl<-dist(data)
  
  DB<<-index.DB(x=data, cl=cluster, d=dl, centrotypes="centroids", p=2, q=2)
  print(c("Dboudin",DB$DB))
  
  sil<<-cluster::silhouette(x = cluster, dist = dl)  
  print(c("silhouette",mean(sil[,"sil_width"])))
  
  
  #index.list<-cls.scatt.data(data[,-1], cl$cluster, dist="euclidean")
  #intraclust = c("complete","average","centroid")
  #interclust = c("single", "complete", "average")
  #dunn1 <- clv.Dunn(index.list, intraclust, interclust)
  #summary(dunn1)
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
  
  query=gsub("labK",label,"INSERT INTO vesale.cluster_index(cluster_label,activity_date,descriptor) VALUES ('labK',now(),'descV')")
  query=gsub("descV",descriptor,query)
  
  rs<-dbSendQuery(con,query)
  
  #Get the id of the new cluster
  rs <- dbSendQuery(con,"SELECT LAST_INSERT_ID()")
  cid <- fetch(rs, n=-1)
  
  #Insert new cluster values into image_cluster table
  data<-cbind(data[,1],cluster,cid)
  colnames(data)<-c("imageId","cluster","clusterID")
  dbWriteTable(con,"image_cluster",data,append=T)
  
  #Close mysql connection
  dbClearResult(rs)
  dbDisconnect(con)
}

uploadClusterValidation=function(cluster,silhouette,dbouldin)
{
  #Connect to database
  con <- connectDB()
  
  #Insert new cluster result into cluster_index table
  
  query=gsub("silV",silhouette,"update vesale.cluster_index set silhouette=silV, dbouldin=dbV where clusterID=cl2")
  query=gsub("dbV",dbouldin,query)
  query=gsub("cl2",cluster,query)
  rs<-dbSendQuery(con,query)
  
  #Close mysql connection
  dbClearResult(rs)
  dbDisconnect(con)
}

#-------------------------------------
# Orchestrator
#-------------------------------------
#' Manage the complete process: get subset, cluster, validate, upload cluster to database
#' @examples
#' orchestrator()
#' orchestrator()
orchestratorKM=function(descriptor,startPage,endPage,k)
{
  #Get subset
  data<-getSubset(descriptor,startPage,endPage)
  cl<-clusterKM(data,k,100)
  uploadClusterDB(cl$cluster,data,labelKMean(cl),descriptor)
  
}

orchestratorValidation=function(clusterID)
{
  print("Start orchestratorValidation")
  
  print("getValidationDataset")
  dataV<-getValidationDataset(clusterID,getClusterAlgorithmsDescriptor(clusterID))
  print("ClusterValidation")
  clusterValidation(dataV[,-c(1,2)],dataV[,2])
  
  print("uploadClusterValidation")
  uploadClusterValidation(clusterID,mean(sil[,"sil_width"]),DB$DB)
  
  print("End orchestratorValidation")
  
}

#-------------------------------------
# Miscelaneos
#-------------------------------------

#' Remove all variables from memory
#' @examples
#' clearVariables()
clearVariables=function()
{
  rm(list=ls())
}
#' Get the list of available descriptors
#' @examples
#' getDescriptors()
getDescriptors=function()
{
  con <- connectDB()
  query="SELECT * FROM vesale.descriptors";
  rs <- dbSendQuery(con,query)
  descriptors <<- fetch(rs, n=-1)
  print(descriptors)
}

getClusterAlgorithms=function()
{
  con <- connectDB()
  query="SELECT clusterID,substring(cluster_label,1,14) as label, activity_date,descriptor,num_images,silhouette,dbouldin FROM vesale.cluster_index;";
  rs <- dbSendQuery(con,query)
  descriptors <<- fetch(rs, n=-1)
  print(descriptors)
}

getClusterAlgorithmsDescriptor=function(clusterID)
{
  con <- connectDB()
  query="SELECT descriptor FROM vesale.cluster_index where clusterID=cl2";
  query=gsub("cl2",clusterID,query);
  rs <- dbSendQuery(con,query)
  descriptor <<- fetch(rs, n=-1)
  descriptor
}

#' Connect to mysql database
#' @examples
#' connectDB()
connectDB=function()
{
   library(RMySQL)
   con <- dbConnect(MySQL(), user="mysqluser", password="userul8mys9l", dbname="vesale", host="164.15.78.25")
   con
}
  
