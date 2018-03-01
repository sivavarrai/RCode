
#Code Review.

scores<-c(4,8,7,10,15,16,20)
sizeof(scores)
lowerlimits<-c(2,4,6,8)
upperlimits<- c(8,4,10,12)

joboffers<-function(scores,lowerlimits,upperlimits) {
  count1<-matrix(nrow=length(scores),ncol=length(lowerlimits))
for(i in 1:length(scores))
  {
    for(j in 1:length(lowerlimits))
    {
      a=0
    if (scores[i]>=lowerlimits[j] && scores[i]<=upperlimits[j])
         a=a+1
    else 
      a=a+0
      count1[i,j]<-a
    }
   #print(a)
   #count1<-a
  }
  return(colSums(count1))
}

joboffers(scores,lowerlimits,upperlimits)

####################################################

startdate<-"2013-01-01"

enddate<- "2013-01-01"

days<-function(startdate,enddate){
  days<-c(as.Date(startdate):as.Date(enddate))
  return(as.Date(days,origin="1970-01-01"))
}

dates<- (as.character(days(startdate,enddate)))

hours<- c(0:23)

dataset<- merge(dates,hours,all=TRUE)
names(dataset)<-c("date","hour")
dataset$date<-as.character.factor(dataset$date)
dataset$hour<- as.numeric(dataset$hour)

knowtimestamps <- c("2013-01-01 00:00","2013-01-01 01:00","2013-01-01 02:00","2013-01-01 03:00",
                    "2013-01-01 04:00","2013-01-01 05:00","2013-01-01 06:00","2013-01-01 08:00",
                    "2013-01-01 10:00","2013-01-01 11:00","2013-01-01 12:00","2013-01-01 13:00",
                    "2013-01-01 16:00","2013-01-01 17:00","2013-01-01 18:00","2013-01-01 19:00",
                    "2013-01-01 20:00","2013-01-01 21:00","2013-01-01 23:00")
humidity <- c(0.62,0.64,0.62,0.63,0.63,0.64,0.63,0.64,0.48,0.46,0.45,0.44,0.46,
              0.47,0.48,0.49,0.51,0.52,0.52)

testset<-as.data.frame(cbind(knowtimestamps,humidity))

testset<- testset %>%
  mutate(date=substr(knowtimestamps,1,10),hour=as.numeric(substr(knowtimestamps,12,13))) %>%
  select(date,hour,humidity)

testset$humidity<-as.character.factor(testset$humidity)

testset$humidity<- as.numeric(testset$humidity)

linearmod<-lm(humidity~hour,data=testset)

totalset<-
dataset %>%
  left_join(testset,by=c("date","hour")) 


finalset<-function(totalset) {
  for(i in 1:nrow(totalset))
    if(is.na(totalset$humidity[i]))
      totalset$humidity[i]<- sum(coalesce(totalset$humidity[i-1],totalset$humidity[i+1]),coalesce(totalset$humidity[i+1],totalset$humidity[i-1]),na.rm=TRUE)/2
return(totalset)
}

finaldata<-finalset(totalset)

ggplot(finaldata,aes(x=hour,y=humidity)) +
  geom_point()
