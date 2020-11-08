complete <-function(dir, id=1:332){
  nobs<-integer(length(id))
  idx<-1
  for(i in id){
    prefix<-"00"
    if(i>=100){
      prefix<-""
    }
    else if(i>=10){
      prefix<-"0"
    }
    f<-read.csv(paste(dir, "/",prefix,i,".csv", sep = ""))
    nobs[idx]<-sum(complete.cases(f),na.rm = TRUE)
    idx<-idx+1
  }
  data.frame(id, nobs)
}