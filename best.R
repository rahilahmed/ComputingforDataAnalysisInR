best <- function(state, outcome) {
  possible_outcome=c("heart failure","heart attack","pneumonia")
  ## Read outcome data
  outcome_read <-read.csv("outcome-of-care-measures.csv", colClasses = "character",header=TRUE)
  
  states<-unique(outcome_read$State)
  if(!state%in%states){
    cat(sprintf("Error in best(\"%s\" \"%s\": invalid state.\n",state,outcome))    
  }
  
  if(!outcome%in%possible_outcome){
    cat(sprintf("Error in best(\"%s\" \"%s\": invalid outcome.\n",state,outcome))    
  }
  
  data_r<-na.omit(subset(outcome_read,State==state))
  
  
  if (outcome=="heart failure") {
    outcome_index = 17
  } else if(outcome=="heart attack") {
    outcome_index = 11
  } else if (outcome=="pneumonia") {
    outcome_index = 23
  } else {
    cat(sprintf("Error in best(\"%s\" \"%s\": invalid outcome.\n",state,outcome))    
  }
  
  data_r2<-subset(data_r,data_r[outcome_index]!="Not Available")
  min_val <- min(as.numeric(data_r2[, outcome_index]))
  best_name=sort(data_r2[as.numeric(data_r2[,outcome_index])==min_val,]$Hospital.Name)[1]
  
    
  ## Check that state and outcome are valid
  ## Return hospital name in that state with lowest 30-day death
  return(best_name)
}
