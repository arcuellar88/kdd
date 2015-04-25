library(EBImage)
library(RMySQL)
library(png)
#Set up working directory (make sure the thumbs directory is in there!)
setwd("/Users/alejandrorodriguez/Documents/DataMining/kdd/")


#Function to resize the images
resizeImage = function(im, w.out, h.out) {
  # function to resize an image 
  # im = input image, w.out = target width, h.out = target height
  # Bonus: this works with non-square image scaling.
  
  # initial width/height
  w.in = nrow(im)
  h.in = ncol(im)
  
  # Create empty matrix
  im.out = matrix(rep(0,w.out*h.out), nrow =w.out, ncol=h.out )
  
  # Compute ratios -- final number of indices is n.out, spaced over range of 1:n.in
  w_ratio = w.in/w.out
  h_ratio = h.in/h.out
  
  # Do resizing -- select appropriate indices
  im.out <- im[ floor(w_ratio* 1:w.out), floor(h_ratio* 1:h.out)]
  
  return(im.out)
}

resizeImagesPageDB = function(page, w, h) {
  con <- dbConnect(MySQL(), user="root", password="", dbname="vesale", host="localhost")
  query=gsub("page",page,"select url from vesale.image_index where page='page'");
  rs <- dbSendQuery(con,query)
  data <- fetch(rs, n=-1)

  for(i in 1:length(data[,]))
  {
    im<-readImage(data[i,1])
    imr<-resizeImage(im,w,h)
    writePNG(imr,data[i,1])
  }
  dbClearResult(rs)
  dbDisconnect(con)
}

#Resize Images
#start page 
#end page
resizeImagesDB = function(startPage,endPage,w,h) {
  for(i in startPage:endPage)
  {
    resizeImagesPageDB(i,w,h)
  }
}