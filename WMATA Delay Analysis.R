library(stringr)
library(plyr)
library (dplyr)
library(data.table)
library(lubridate)
### Read in Data ###
### Read in Data ###
### Read in Data ###
classes = c(
  "character",   
  "character",   
  "character",  
  "character",   
  "character",   
  "character",  
  "integer")    
wmataRaw<-read.csv('/Users/katerabinowitz/Documents/DataLensDC/WMATA-Delay/disruptions,6-24.csv',na.strings=c("", "NA"),
                   strip.white=TRUE, colClasses = classes)[c(1:2,4,7)] 

#Date variables
wmataRaw$Date<- as.Date(wmataRaw$Date, "%m/%d/%Y")
wmataRaw$YM <- format(wmataRaw$Date, '%m-%y')

wmataRaw$Year<-year(wmataRaw$Date)
wmataRaw$Month<-month(wmataRaw$Date)

wmataRaw <- wmataRaw[which(wmataRaw$Year>2012),] 
wmataH1<- wmataRaw[which(wmataRaw$Month<7),] 
wmataH1$H1Year<-ifelse(wmataH1$Year=='2013','First Half 2013',
                        ifelse(wmataH1$Year=='2014','First Half 2014',
                          ifelse(wmataH1$Year=='2015','First Half 2015','')))

###Aggregate by H1 and yearmonth for trend###
###Aggregate by H1 and yearmonth for trend###
###Aggregate by H1 and yearmonth for trend###
YMdelaySum<-ddply(wmataRaw, c("YM"),nrow)
colnames(YMdelaySum)<-c("YM","DelayCountYM")

to.merge<-wmataRaw[c('YM','Year','Month')]
YMD<-merge(x=YMdelaySum, y = to.merge, by = "YM")
YMdelaySum<-unique(YMD[duplicated(YMD),])
YMdelaySum<-YMdelaySum[order(YMdelaySum$Year,YMdelaySum$Month), ]
YMdelaySum<-YMdelaySum[c("YM","DelayCountYM")]

H1delaySum<-ddply(wmataH1, c("H1Year"),nrow)
colnames(H1delaySum)<-c("H1","DelayCountH1")

write.csv(YMdelaySum, 
          file="/Users/katerabinowitz/Documents/DataLensDC/WMATA-Delay/Wmata-Delay/YM-Delays.csv")
write.csv(H1delaySum, 
          file="/Users/katerabinowitz/Documents/DataLensDC/WMATA-Delay/Wmata-Delay/H1-Delays.csv")

YMdelayTime<-aggregate(Delay ~ YM, wmataRaw, mean)
H1delayTime<-aggregate(Delay ~ Year, wmataH1, mean)

#Total Delay in 2015H1
wmataD<- wmataRaw[which(!is.na(wmataRaw$Delay)),]
wmataD<- wmataD[which(wmataD$Year==2015),]
sum(wmataD$Delay)

###Weekday and time for accidents###
###Weekday and time for accidents###
###Weekday and time for accidents###
wmataDT<- wmataRaw[which(!is.na(wmataRaw$Time)),] 

#Time variables
hour<-strsplit(wmataDT$Time, ":")
ampm<-strsplit(wmataDT$Time, " ")
hour<-as.data.frame(matrix(unlist(hour), ncol=2, byrow=TRUE,))
colnames(hour)<-c("Hr","Rest")
ampm<-as.data.frame(matrix(unlist(ampm), ncol=2, byrow=TRUE,))
colnames(ampm)<-c("Time","AMPM")

wmata<-cbind(wmataDT,hour,ampm)
wmata$Hour<-tolower(paste(wmata$Hr,wmata$AMPM))
wmata$Hour<-ifelse(substring(wmata$Hour, 1, 1)=='0',gsub('0','',wmata$Hour),wmata$Hour)
wmata$HourN<- ifelse(wmata$Hour=='1 a.m.',1,
              ifelse(wmata$Hour=='2 a.m.',2,
              ifelse(wmata$Hour=='3 a.m.',3,
                ifelse(wmata$Hour=='4 a.m.',4, 
                ifelse(wmata$Hour=='5 a.m.',5,
                ifelse(wmata$Hour=='6 a.m.',6,
                  ifelse(wmata$Hour=='7 a.m.',7,
                  ifelse(wmata$Hour=='8 a.m.',8,
                  ifelse(wmata$Hour=='9 a.m.',9,
                    ifelse(wmata$Hour=='10 a.m.',10,
                    ifelse(wmata$Hour=='11 a.m.',11,
                    ifelse(wmata$Hour=='12 p.m.',12,
                      ifelse(wmata$Hour=='1 p.m.',13,
                      ifelse(wmata$Hour=='2 p.m.',14,
                      ifelse(wmata$Hour=='3 p.m.',15,
                      ifelse(wmata$Hour=='4 p.m.',16,
                        ifelse(wmata$Hour=='5 p.m.',17,
                        ifelse(wmata$Hour=='6 p.m.',18,
                        ifelse(wmata$Hour=='7 p.m.',19,
                             ifelse(wmata$Hour=='8 p.m.',20,
                              ifelse(wmata$Hour=='9 p.m.',21,
                              ifelse(wmata$Hour=='10 p.m.',22, 
                                    ifelse(wmata$Hour=='11 p.m.',23,
                                    ifelse(wmata$Hour=='12 a.m.',24,''))))))))))))))))))))))))

wmata$Weekday  <- as.factor(weekdays(wmata$Date))
wmata$Weekday  <- factor(wmata$Weekday,  levels  =	c("Sunday","Monday",	"Tuesday",	"Wednesday",	
                                                               "Thursday",	"Friday",	"Saturday"))
wmata$Day<-ifelse(wmata$Weekday=="Sunday",1,
                       ifelse(wmata$Weekday=="Monday",2,
                              ifelse(wmata$Weekday=="Tuesday",3,
                                     ifelse(wmata$Weekday=="Wednesday",4,
                                            ifelse(wmata$Weekday=="Thursday",5,
                                                   ifelse(wmata$Weekday=="Friday",6,
                                                          ifelse(wmata$Weekday=="Saturday",7,'')))))))

###Commuting Hours###
###Commuting Hours###
###Commuting Hours###
wmata$morningCommute<-ifelse(((wmata$Hour=="7 a.m." | wmata$Hour=="8 a.m." | wmata$Hour=="9 a.m.") &
                                (wmata$Weekday!="Saturday" & wmata$Weekday!="Sunday")),'1','0') 
wmata$eveningCommute<-ifelse(((wmata$Hour=="5 p.m." | wmata$Hour=="6 p.m." | wmata$Hour=="7 p.m.") &
                                (wmata$Weekday!="Saturday" & wmata$Weekday!="Sunday")),'1','0') 
wmata$Commute<-ifelse(wmata$morningCommute=='1' | wmata$morningCommute=='1','1','0')

wmata2015<-wmata[which(wmata$Year>2014),] 
tomerge<-wmata2015[c('Date','Day')]
DelayCount<-ddply(wmata2015, c("Date","HourN"),nrow, .drop=FALSE)
Delayweekday<-merge(x=DelayCount, y = tomerge, by = "Date", all.x = TRUE)

#Sum
AvgDelay<-aggregate(V1 ~ Day + HourN, Delayweekday, mean)
AvgDelay$HourN<-as.numeric(AvgDelay$HourN)
AvgDelay$Day<-as.numeric(AvgDelay$Day)
AvgDelay<-AvgDelay[order(AvgDelay$Day,AvgDelay$HourN), ]
AvgDelay5+<-AvgDelay[which(wmata$Year>2014),] 

write.csv(AvgDelay, 
          file="/Users/katerabinowitz/Documents/DataLensDC/WMATA-Delay/Wmata-Delay/DT-Delays.csv",row.names=FALSE)

###Metro Line Analysis###
###Metro Line Analysis###
###Metro Line Analysis###
YMLine15<-ddply(wmata2015, c("Line"),nrow)
CLine15<-ddply(wmata2015, c("Line","Commute"),nrow)

LineDelay<-aggregate(Delay ~ Line, wmata2015, mean)
CLineDelay<-aggregate(Delay ~ Line + Commute, wmata2015, mean)
