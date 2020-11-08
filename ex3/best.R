best<-function(state, outcome) {
  ## Read outcome data
  data<-read.csv("outcome-of-care-measures.csv", colClasses = "character")
  outcome_id<-NULL
  ## Check that state and outcome are valid
  if(outcome == "heart attack")
  {
    outcome_id <- 11
  }
  else if(outcome == "heart failure")
  {
    outcome_id <- 17
  }
  else if(outcome == "pneumonia")
  {
    outcome_id <- 23
  } else {
    stop("invalid outcome")
  }
  if(!(state %in% data$State ) ){
    stop("invalid state")
  }
  data[, outcome_id] <- as.numeric(data[, outcome_id])
  subset<- data[data$State ==state ,]
  bests<-subset[which.min(subset[,outcome_id]),]
  names<-bests$Hospital.Name
  sort(names)
  names[1]
}