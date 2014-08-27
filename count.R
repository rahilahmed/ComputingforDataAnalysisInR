count <- function(cause = NULL) {
  ## Check that "cause" is non-NULL; else throw error
  if(length(cause)==0 | is.null(cause)){
    stop("Please provide cause")
  }
  ## Check that specific "cause" is allowed; else throw error
  allowed_cause=c("asphyxiation","blunt force","other","shooting","stabbing","unknown")
  
  if(!cause%in%allowed_cause){
    stop("Not a valid cause")
  }
  ## Read "homicides.txt" data file
  homicides<-readLines("homicides.txt")
  ## Extract causes of death
  
  cause_count<-grep()
  ## Return integer containing count of homicides for that cause
}
