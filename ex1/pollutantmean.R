pollutantmean<-function(dir, pollutant,id=1:332){
  pol_values<-numeric(0)
  for(i in id){
    prefix<-"00"
    if(i>=100){
      prefix<-""
    }
    else if(i>=10){
      prefix<-"0"
    }
    f<-read.csv(paste(dir, "/",prefix,i,".csv", sep = ""))
    if(pollutant == "sulfate"){
      pol_values<- c(pol_values,f$sulfate)
    } else {
      pol_values<- c(pol_values,f$nitrate)
    }
  }
  mean(pol_values, na.rm=TRUE)
}