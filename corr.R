corr <- function(directory, threshold = 0) {
 corrs  <- c()
  filenames <- list.files(directory, pattern="*.csv",full.names=TRUE)
  for(file in filenames){
    
    df<-read.csv(file,header = TRUE)
    nadf<-df[rowSums(is.na(df)) == 0,]
    
    if (nrow(nadf) > threshold){
       corValue = cor(nadf$nitrate,nadf$sulfate)
       corrs <- c(corrs,corValue)
    }
  }
  
  return(corrs)
}

