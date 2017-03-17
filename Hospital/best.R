best  <- function(state, outcome)
{
  hospital <- read.csv("outcome-of-care-measures.csv", colClasses="character")[ ,c(2,7,11,17,23)];
  names(hospital)[1]<-"Name";
  names(hospital)[3]<-"heart attack";
  names(hospital)[4]<-"heart failure";
  names(hospital)[5]<-"pneumonia";
  hospital[,3]  <- as.numeric(hospital[,3])
  hospital[,4]  <- as.numeric(hospital[,4])
  hospital[,5]  <- as.numeric(hospital[,5])
  hospital <- subset(hospital, State == state)
  hospital  <- hospital[complete.cases(hospital),]
  states  <- unique(hospital[,2])
  
  if(outcome == "heart attack" | outcome == "heart failure" | outcome == "pneumonia")
  {
  }
  else
  {
    stop("invalid outcome")
  }
  
  if(state %in% states)
  {    
  }
  else
  {
    stop("invalid state")
  }
  
  
  
  #result <- hospital[order(outcome,"Name"),][,1]
  #head(result)
  #str(hospital)
  sort_list <- c(outcome,"Name")
  hospital  <- hospital[do.call(order, hospital[, match(sort_list, names(hospital))]),] [1,1]
  hospital
}