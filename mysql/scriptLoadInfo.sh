for i in {4..20}; do ./mysql -e 'LOAD DATA LOCAL INFILE "/Users/alejandrorodriguez/Documents/DataMining/BD-Vesale/Google/info-'${i}'.txt"
 INTO TABLE vesale.image_index FIELDS TERMINATED BY '\',\'' (page, lc_x,lc_y,rc_x,rc_y,url)' ; done

./mysql -e 'update vesale.image_index set imageID=replace(SUBSTRING_INDEX( url , "-", -1 ),".png","")'
./mysql -e 'update vesale.image_index set url=TRIM(url)'

 

