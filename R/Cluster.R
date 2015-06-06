
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
getSubset = function(imgDescriptors,startPage,endPage) {
  con <- connectDB()
   
  query="SELECT imageid FROM image_index" ;
  for (counter in imgDescriptors) 
  {
    query=gsub("FROM",paste(",",counter,".vector FROM",sep=""),query);
    query<-paste(query," join vesale.",counter," using(imageID)")
  }
  query<-paste(query,"where page>=",startPage,"and page<",endPage,sep=" ")
  print(query)
  rs <- dbSendQuery(con,query)
  data <- fetch(rs, n=-1)
  
  dbClearResult(rs)
  dbDisconnect(con)
  
  #Transform comma separated column
  i=2
  dataVector<-numeric()
  for (counter in imgDescriptors) 
  {
    dataVector<-cbind(dataVector,splitCSVColumn(data[,i]))
    i=i+1
  } 
  data<-cbind(data[,1],dataVector)
  
  colnames(data)<-c("imageId",colnames(data, do.NULL = FALSE)[-1])
  data
}

#' Fetch the data for a given sampleid and image descriptors
#' @param imgDescriptor in {SIFT, etc.}
#' @param sampleid, id of the requested sample
#' @return matrix resulted from the split.
#' @examples
#' getSubset("SIFT")
#' getSubset(data[,2])
getSampleData = function(imgDescriptors,sampleid) {
  con <- connectDB()
  query="SELECT * FROM vesale.sample";
  for (counter in imgDescriptors) 
  {
    query<-paste(query," join vesale.",counter," using(imageid)", sep = "")
  }
  query<-paste(query,"where sampleid=",sampleid, sep = " ")
  print(query)
  rs <- dbSendQuery(con,query)
  data <- fetch(rs, n=-1)
  
  dbClearResult(rs)
  dbDisconnect(con)
  
 #Transform comma separated column
 i=3
 dataVector<-numeric()
 for (counter in imgDescriptors) 
 {
   dataVector<-cbind(dataVector,splitCSVColumn(data[,i]))
   print(counter)
   i=i+1
 } 
 
 data<-cbind(data[,1],dataVector)
 colnames(data)<-c("imageId",colnames(data, do.NULL = FALSE)[-1])
 data
}

#' Fetch the data for a given cluster algorithm
#' @param clusterID id of the clustering algorithm
#' @return matrix resulted from the split.
#' @examples
#' getValidationDataset(1000011)
#' getValidationDataset(1000011)
getValidationDataset = function(clusterID,imgDescriptors) {
  con <- connectDB()
  query="SELECT imageid,cluster FROM image_cluster" ;
  for (counter in imgDescriptors) 
  {
    query=gsub("FROM",paste(",",counter,".vector FROM",sep=""),query);
    query<-paste(query," join vesale.",counter," using(imageID)")
  }
  query<-paste(query,"where clusterID=",clusterID,sep=" ")
  print(query)
  rs <- dbSendQuery(con,query)
  data <- fetch(rs, n=-1)
  
  dbClearResult(rs)
  dbDisconnect(con)
  
  #Transform comma separated column
  i=3
  dataVector<-numeric()
  for (counter in imgDescriptors) 
  {
    dataVector<-cbind(dataVector,splitCSVColumn(data[,i]))
    i=i+1
  } 
  data<-cbind(data[,1],data[,2],dataVector)
  
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
  
  dl<-dist(data,method="minkowski",diag = FALSE, upper = FALSE,p=3)
  print("distance matrix done")
  
  sil<<-cluster::silhouette(x = cluster, dist = dl)  
  print(c("silhouette",mean(sil[,"sil_width"])))
  
  #DB<<-index.DB(x=data, cl=cluster, d=dl, centrotypes="centroids", p=2, q=2)
  #print(c("Dboudin",DB$DB))
   
  
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
#' Manage the complete process: get subset page range, cluster, validate, upload cluster to database
#' @param startPage
#' @param endPage
#' @examples
#' orchestrator()
#' orchestrator()
orchestratorKMPageRange=function(imgDescriptors,startPage,endPage,k)
{
  #Get subset
  data<-getSubset(imgDescriptors,startPage,endPage)
  print("Get subset finish")
  
  cl<-clusterKM(data,k,100)
  
  print("Clustering finish")
  
  uploadClusterDB(cl$cluster,data,labelKMean(cl),labelDescriptors(imgDescriptors))
  print("Uploading cluster to database finish")
  
  #Update the cluster stats
  updateDBStats()
}

#' Manage the complete process: get subset , cluster, validate, upload cluster to database
#' @k Number of clusters
#' @sampleid id of the sample to cluster
#' @imgDescritors List of image descriptors
#' @examples
#' orchestrator(c("CENTROIDS_H30","AREA_H30"),1,100)
orchestratorKMSample=function(imgDescriptors,sampleid,k)
{
  #Get sample data 
  data<-getSampleData(imgDescriptors,sampleid)
  print("Get sample data finish")
  
  cl<-clusterKM(data,k,100)
  print("Clustering finish")
  
  uploadClusterDB(cl$cluster,data,labelKMean(cl),labelDescriptors(imgDescriptors))
  print("Uploading cluster to database finish")
  
  #Update the cluster stats 
  updateDBStats()
}


#' Manage the complete process: get subset , cluster, validate, upload cluster to database
#' @k Number of clusters
#' @sampleid id of the sample to cluster
#' @imgDescritors List of image descriptors
#' @examples
#' orchestratorClaraSample("DF_H30_Z5x5_D5x5",1,100)
orchestratorClaraSample=function(imgDescriptors,sampleid,k) {
  library(cluster)
  #Get sample data 
  data<-getSampleData(imgDescriptors,sampleid)
  print("Get sample data finish")
  
  cl<-clara(data[,-1],k)
  print("Clustering finish")
  
  uploadClusterDB(cl$cluster,data,paste(labelClara(cl),"Descriptors:",imgDescriptors),labelDescriptors(imgDescriptors))
  print("Uploading cluster to database finish")
  
  #Update the cluster stats 
  updateDBStats()
}

# Get description for clara result
labelClara <- function(cl) {
  label = paste("Clara", "sampleSize:", length(cl$cluster) ,"k:", nrow(cl$clusinfo), "objective:", cl$objective, sep = " ")
  return (label)
}

orchestratorValidation=function(clusterID)
{
  print("Start orchestratorValidation")
  
  print("getValidationDataset")
  dataV<-getValidationDataset(clusterID,getClusterAlgorithmsDescriptor(clusterID))
  print(NROW(dataV))
  print("ClusterValidation")
  clusterValidation(dataV[,-c(1,2)],dataV[,2])
  
  print("uploadClusterValidation")
  uploadClusterValidation(clusterID,mean(sil[,"sil_width"]),DB$DB)
  
  print("End orchestratorValidation")

}

#-------------------------------------
# Miscelaneos
#-------------------------------------

labelDescriptors=function(imgDescriptors)
{
  labelD<-""
 
  for (counter in imgDescriptors) 
  {  
    if(labelD=="")
      labelD<-counter
    else 
      labelD<-paste(labelD,",",counter,sep="")
  }
  labelD
}

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
  dbClearResult(rs)
  dbDisconnect(con)
}

getClusterAlgorithms=function()
{
  con <- connectDB()
  query="SELECT clusterID,substring(cluster_label,1,14) as label, activity_date,descriptor,num_images,silhouette,dbouldin FROM vesale.cluster_index;";
  rs <- dbSendQuery(con,query)
  algorithms <<- fetch(rs, n=-1)
  print(algorithms)
  dbClearResult(rs)
  dbDisconnect(con)
}

getClusterAlgorithmsDescriptor=function(clusterID)
{
  con <- connectDB()
  query="SELECT descriptor FROM vesale.cluster_index where clusterID=cl2";
  query=gsub("cl2",clusterID,query);
  rs <- dbSendQuery(con,query)
  descriptor <<- fetch(rs, n=-1)
  
  descriptorV<-do.call(rbind, strsplit(gsub('[ ]|, |,[ ]',',',descriptor),','))
  
  dbClearResult(rs)
  dbDisconnect(con)
  
  descriptorV
  
}

printSampleDataDescriptor=function(imgDescriptor)
{
  con <- connectDB()
  query="SELECT * FROM vesale.idesc limit 10";
  query=gsub("idesc",imgDescriptor,query);
  rs <- dbSendQuery(con,query)
  sampleDataDescriptor <<- fetch(rs, n=-1)
  
  dbClearResult(rs)
  dbDisconnect(con)
  sampleDataDescriptor
}

updateDBStats=function()
{
  con <- connectDB()
  query="CALL vesale.update_cluster_stats()";
  dbSendQuery(con,query)
  dbDisconnect(con)
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
  
