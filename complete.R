complete <- function(directory, id = 1:332) {
  
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  
  dataset <- data.frame(id =numeric() , nobs=numeric() )
  
  for(i in id){
    path = sprintf("%s/%03d.%s",directory,i,'csv')
    df<-read.csv(path,header = TRUE)
    nadf<-df[rowSums(is.na(df)) == 0,]
    dataset[nrow(dataset)+1,] <- c(i,nrow(nadf))
  }
  return(dataset)
}

