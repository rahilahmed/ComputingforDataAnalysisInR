rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  
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
  data_rank<-order(as.numeric(data_r2[,outcome_index]),data_r2[,2])
  
  if(num=="worst"){
    num<-length(data_rank)
  }
  
  if(num=="best"){
    num<-1
  }
  
  if(num>length(data_rank)){
     hostpital_name <- "NA"
  } else {
     hostpital_name<-data_r2[data_rank[num],][,2]
  }
  
  return(hostpital_name)
}
