rankall  <- function(outcome, num="best")
{
  hospital <- read.csv("outcome-of-care-measures.csv", colClasses="character")[ ,c(2,7,11,17,23)];
  names(hospital)[1]<-"hospital";
  names(hospital)[2]<-"state";
  names(hospital)[3]<-"heart_attack";
  names(hospital)[4]<-"heart_failure";
  names(hospital)[5]<-"pneumonia";
  hospital[,3]  <- as.numeric(hospital[,3])
  hospital[,4]  <- as.numeric(hospital[,4])
  hospital[,5]  <- as.numeric(hospital[,5])
  #hospital <- subset(hospital, State == state)
  #hospital  <- hospital[complete.cases(hospital),]
  #states  <- unique(hospital[,2])
  
  if(outcome == "heart attack" | outcome == "heart failure" | outcome == "pneumonia")
  {
  }
  else
  {
    stop("invalid outcome")
  }
  
  
  #sort_list <- c("state", outcome,"Name")
  #hospital  <- hospital[do.call(order, hospital[, match(sort_list, names(hospital))]),]
  if(outcome=="heart attack")
  {
    hospital  <- hospital[!(is.na(hospital$heart_attack)),]
    sort_list <- c("state", "heart_attack","hospital")
    hospital  <- hospital[do.call(order, hospital[, match(sort_list, names(hospital))]),]
    hospital  <- transform(hospital, rank = ave(heart_attack, state, FUN = function(x) rank(x, ties.method = "first")))
  }
  else if(outcome=="heart failure")
  {
    hospital  <- hospital[!(is.na(hospital$heart_failure)),]
    sort_list <- c("state", "heart_failure","hospital")
    hospital  <- hospital[do.call(order, hospital[, match(sort_list, names(hospital))]),]
    hospital  <- transform(hospital, rank = ave(heart_failure, state, FUN = function(x) rank(x, ties.method = "first")))   
  }
  else if(outcome=="pneumonia")
  {
    hospital  <- hospital[!(is.na(hospital$pneumonia)),]
    sort_list <- c("state", "pneumonia","hospital")
    hospital  <- hospital[do.call(order, hospital[, match(sort_list, names(hospital))]),]
    hospital  <- transform(hospital, rank = ave(pneumonia, state, FUN = function(x) rank(x, ties.method = "first")))

  }
 
  #hospital  <- subset(hospital, rank==num)
 
  #hospital[,c(1,2)] 
  if(num=="worst")
  {
    hospital  <- do.call(rbind,lapply(split(hospital,hospital$state),function(chunk) chunk[which.max(chunk$rank),]))
    hospital[,c(1,2)] 
  }
 
}