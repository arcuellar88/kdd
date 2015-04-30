#Function to resize the images
resizeImageF = function(im, w.out, h.out) {
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
  im.out <- im[floor(h_ratio* 1:h.out),floor(w_ratio* 1:w.out)]
  
  return(im.out)
}

#' Resize the patterns (images) of a page in a book
#' @param page Number of the page in the book
#' @param x, height of the image
#' @examples
#' resizeImagesPageDB(4,30)
resizeImagesPageDB = function(page, x) {
  library(EBImage)
  library(RMySQL)
  con <- dbConnect(MySQL(), user="root", password="", dbname="vesale", host="localhost")
  query=gsub("page1",page,"select url from vesale.image_index where page='page1'");
  View(query)
  rs <- dbSendQuery(con,query)
  data <- fetch(rs, n=-1)

  for(i in 1:length(data[,]))
  {
    im<-readImage(data[i,1])
    imr<-resize(im,h=x)
    
    writeImage(imr,data[i,1])
  }
  dbClearResult(rs)
  dbDisconnect(con)
}

#' Resize the patterns (images) of range of pages in a book
#' @param startPage Number of the start page of the range in the book
#' @param endPage Number of the end page of the range in the book
#' @param x, height of the image
#' @examples
#' resizeImagesPageDB(4,30)
resizeImagesDB = function(startPage,endPage,x) {
  #Set up working directory (make sure the thumbs directory is in there!)
  setwd("/Users/alejandrorodriguez/Documents/DataMining/kdd/")
  
  for(i in startPage:endPage)
  {
    resizeImagesPageDB(i,x)
  }
}