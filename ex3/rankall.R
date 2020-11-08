rankall <- function(outcome, num = "best") {
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
  data[, outcome_id] <- as.numeric(data[, outcome_id])
  ## For each state, find the hospital of the given rank
  hospital<-vector()
  state<-vector()
  hlist<-split(data, data$State)
  for (states in hlist)
  {
    cleaned <- states[complete.cases(states[ , outcome_id]),]
    ordered<-cleaned[order(cleaned[,outcome_id],cleaned$Hospital.Name),]
    hospital_name<-NA
    state_name<-states$State[1]
    if(num == "best"){
      hospital_name<-ordered$Hospital.Name[1]
    } else if(num == "worst") {
      hospital_name<-tail(ordered$Hospital.Name,1)
    } else if (num <= length(ordered$Hospital.Name)) {
      hospital_name<-ordered$Hospital.Name[num]
    }
    hospital<-append(hospital,hospital_name)
    state<-append(state,state_name)
  }
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  data.frame(hospital,state)
}