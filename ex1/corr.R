corr<-function(dir, threshold=0){
  my_obs<-complete(dir)
  id<- my_obs$id[my_obs$nobs>threshold]
  corrs<-numeric(length(id))
  idx = 1
  for(i in id){
    prefix<-"00"
    if(i>=100){
      prefix<-""
    }
    else if(i>=10){
      prefix<-"0"
    }
    f<-read.csv(paste(dir, "/",prefix,i,".csv", sep = ""))
    x<-f$sulfate[!is.na(f$sulfate) & !is.na(f$nitrate)]
    y<-f$nitrate[!is.na(f$sulfate) & !is.na(f$nitrate)]
    corrs[idx]<-cor(x,y)
    idx<-idx+1
  }
  corrs
}