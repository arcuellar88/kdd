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
resizeImagesPageDB = function(page, x , imgPath) {
  library(EBImage)
  library(RMySQL)
  con <- dbConnect(MySQL(), user="mysqluser", password="userul8mys9l", dbname="vesale", host="localhost")
  #con <- dbConnect(MySQL(), user="root", password="", dbname="vesale", host="localhost")
  query=gsub("page1",page,"select url from vesale.image_index where page='page1'");
  #View(query)
  rs <- dbSendQuery(con,query)
  data <- fetch(rs, n=-1)

  for(i in 1:length(data[,]))
  {
    
    imgNameLength = as.integer(nchar(data[i,1]))
    imageNamePath = substring(data[i,1], 3, imgNameLength)
        
    fullImagePath = paste(imgPath, imageNamePath , sep="/")
    
    im<-readImage(fullImagePath)
    imr<-resize(im,h=x)
    
    writeImage(imr, fullImagePath)
  }
  dbClearResult(rs)
  dbDisconnect(con)
}

# Resize all image in a folder
resizeImagesLocal <- function(folderPath, height) {
    library(EBImage)
    images = list.files(folderPath)
    for(image in images) {
        fullImagePath = paste(folderPath, image, sep = "\\")
        im <- readImage(fullImagePath)
        imr <- resize(im, h = height)
        writeImage(imr, fullImagePath)
    }
}

#' Resize the patterns (images) of range of pages in a book
#' @param startPage Number of the start page of the range in the book
#' @param endPage Number of the end page of the range in the book
#' @param x, height of the image
#' @examples
#' resizeImagesPageDB(4,30)
resizeImagesDB = function(startPage, endPage , x, imgPath) {
  #Set up working directory (make sure the thumbs directory is in there!)
  #setwd("D:/IT4BI/Semester 2/Knowledge Discovery and Data Mining/Project/kdd/")
  
  for(i in startPage:endPage)
  {
    print(i)
    resizeImagesPageDB(i,x, imgPath)
  }
}