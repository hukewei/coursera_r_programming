rankhospital <- function(state, outcome, num = "best") {
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
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  data[, outcome_id] <- as.numeric(data[, outcome_id])
  subset<- data[data$State ==state ,]
  cleaned <- subset[complete.cases(subset[ , outcome_id]),]
  ordered<-cleaned[order(cleaned[,outcome_id],cleaned$Hospital.Name),]
  if(num == "best"){
    ordered$Hospital.Name[1]
  } else if(num == "worst") {
    tail(ordered$Hospital.Name,1)
  } else if (num <= length(ordered$Hospital.Name)) {
    ordered$Hospital.Name[num]
  } else {
    NA
  }
}