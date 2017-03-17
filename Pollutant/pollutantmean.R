pollutantmean <- function(directory, pollutant, id=1:332) {
 
  filenames <- list.files(directory, pattern="*.csv", full.names=TRUE)
  sel_names <- filenames[id];
  

  mycols <- rep("NULL", 4);
  data <- vector(mode="double", length=0)
  
  for(i in seq_along(sel_names))
  {
    if(pollutant == "sulfate")
    {
      mycols[c(2)] <- NA;
      newdata <- read.table(sel_names[i], skip=1,header=FALSE, sep=",", colClasses = mycols)
      data<-rbind(data,newdata)
    }  
    else if (pollutant == "nitrate")
    {
      mycols[c(3)] <- NA;
      newdata <- read.table(sel_names[i], skip=1,header=FALSE, sep=",", colClasses = mycols)
      data<-rbind(data,newdata)
    }   
  }
  
  means <- as.numeric(colMeans(data, na.rm=TRUE))
  
  means
  #res <- c(means[2])
  #res
  #sel_names
}