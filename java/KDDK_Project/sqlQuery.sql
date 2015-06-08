SELECT CL.clusterID, CL.label, DF.imageid, CONCAT(DF.vector, ',', CH.vector)
FROM vesale.cluster_label CL, vesale.image_cluster IC, 
	vesale.DF_H30_Z5x5_D5x5 DF, vesale.CVHULLYPROFILE_H30 CH
WHERE CL.cluster=1000063 and IC.clusterID=1000063 
		and CL.clusterID=IC.cluster
		and IC.imageid=DF.imageid and CL.clusterID=CH.imageid
order by CL.clusterID asc
limit 10000000
INTO OUTFILE '/tmp/clustersLabeled.csv'
FIELDS TERMINATED BY '\t'
LINES TERMINATED BY '\n';




SELECT CL.clusterID, count(*)
FROM vesale.cluster_label CL, vesale.image_cluster IC, 
	vesale.DF_H30_Z5x5_D5x5 DF, vesale.CVHULLYPROFILE_H30 CH
WHERE CL.cluster=1000063 and IC.clusterID=1000063 
		and CL.clusterID=IC.cluster
		and IC.imageid=DF.imageid and CL.clusterID=CH.imageid
group by CL.clusterID
order by count(*) asc
;


SELECT CL.clusterID, CL.label, DF.imageid, CONCAT(DF.vector, ',', CH.vector)
FROM vesale.cluster_label CL, vesale.image_cluster IC, 
	vesale.DF_H30_Z5x5_D5x5 DF, vesale.CVHULLYPROFILE_H30 CH
WHERE CL.cluster=1000063 and IC.clusterID=1000063 
		and IC.cluster=141
		and CL.clusterID=IC.cluster
		and IC.imageid=DF.imageid and CL.clusterID=CH.imageid
;


SELECT *
FROM vesale.cluster_label CL
where CL.cluster=1000063 and CL.label like '%DownS%'
limit 1000000;

SELECT *
FROM vesale.cluster_label CL
where CL.cluster=1000063 and CL.clusterID=94;


