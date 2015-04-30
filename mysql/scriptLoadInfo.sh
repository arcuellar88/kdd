for i in {4..30}; do ./mysql -u mysqluser -puserul8mys9l -h164.15.78.25 -e 'LOAD DATA LOCAL INFILE "/Users/alejandrorodriguez/Documents/DataMining/BD-Vesale/Google/info-'${i}'.txt"
 INTO TABLE vesale.image_index FIELDS TERMINATED BY '\',\'' (page, lc_x,lc_y,rc_x,rc_y,url)' ; done

./mysql -umysqluser -puserul8mys9l -h164.15.78.25 -e 'update vesale.image_index set imageID=replace(SUBSTRING_INDEX( url , "-", -1 ),".png","")'
./mysql -umysqluser -puserul8mys9l -h164.15.78.25 -e 'update vesale.image_index set url=TRIM(url)'

 

