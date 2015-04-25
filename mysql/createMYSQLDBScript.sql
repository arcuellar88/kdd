CREATE DATABASE `vesale` /*!40100 DEFAULT CHARACTER SET latin1 */

CREATE TABLE `image_index` (
  `idindex` int(11) NOT NULL AUTO_INCREMENT,
  `page` int(11) DEFAULT NULL,
  `lc_x` int(11) DEFAULT NULL,
  `lc_y` int(11) DEFAULT NULL,
  `rc_x` int(11) DEFAULT NULL,
  `rc_y` int(11) DEFAULT NULL,
  `url` varchar(50) DEFAULT NULL,
  `imageID` int(11) DEFAULT NULL,
  PRIMARY KEY (`idindex`)
) ENGINE=InnoDB AUTO_INCREMENT=111602 DEFAULT CHARSET=latin1
