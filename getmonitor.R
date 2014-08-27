getmonitor <- function(id, directory, summarize = FALSE) {
  idn <- as.numeric(id)
  path = sprintf("%s/%03d.%s",directory,idn,'csv')
  readings<-read.csv(path,header = TRUE)
  
  if(isTRUE(summarize)){
    suma<-summary(readings)
    print(suma)
  }
  
  return (readings)
}

