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
wmataRaw$YM <- format(wmataRaw$Date, '%Y-%m')

wmataRaw$Year<-year(wmataRaw$Date)
wmataRaw$Month<-month(wmataRaw$Date)

wmataH1 <- wmataRaw[which(wmataRaw$Month<7 | wmataRaw$Year>2012),] 
wmataH1$H1Year<-ifelse(wmataH1$Year=='2013','First Half 2013',
                        ifelse(wmataH1$Year=='2014','First Half 2014',
                          ifelse(wmataH1$Year=='2015','First Half 2015','')))

###Aggregate by H1 and yearmonth for trend###
###Aggregate by H1 and yearmonth for trend###
###Aggregate by H1 and yearmonth for trend###
YMdelaySum<-ddply(wmataRaw, c("YM"),nrow)
colnames(YMdelaySum)<-c("YM","DelayCountYM")
H1delaySum<-ddply(wmataH1, c("H1Year"),nrow)
colnames(H1delaySum)<-c("H1","DelayCountH1")


write.csv(YMDelay, 
          file="/Users/katerabinowitz/Documents/DataLensDC/WMATA-Delay/Wmata-Delay/YM-Delays.csv")
write.csv(H1delaySum, 
          file="/Users/katerabinowitz/Documents/DataLensDC/WMATA-Delay/Wmata-Delay/H1-Delays.csv")


YMdelayTime<-aggregate(Delay ~ YM, wmataRaw, mean)
H1delayTime<-aggregate(Delay ~ Year, wmataH1, mean)

YMLine<-ddply(wmataRaw, c("Line"),nrow)
H1Line<-ddply(wmataH1,c("Line", "Year"),nrow)

###Weekday and time for accidents###
###Weekday and time for accidents###
###Weekday and time for accidents###
wmataDT<- wmataRaw[which(!is.na(wmataRaw$Time)),] 
wmataDT$Weekday  <- as.factor(weekdays(wmataDT$Date))
wmataDT$Weekday  <- factor(wmataDT$Weekday,  levels	=	c("Sunday","Monday",	"Tuesday",	"Wednesday",	
                                                          "Thursday",	"Friday",	"Saturday"))
wmataDT$Day<-ifelse(wmataDT$Weekday=="Sunday",1,
                ifelse(wmataDT$Weekday=="Monday",2,
                  ifelse(wmataDT$Weekday=="Tuesday",3,
                    ifelse(wmataDT$Weekday=="Wednesday",4,
                      ifelse(wmataDT$Weekday=="Thursday",5,
                        ifelse(wmataDT$Weekday=="Friday",6,
                            ifelse(wmataDT$Weekday=="Saturday",7,'')))))))
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
#Sum
DateTimeDelay<-ddply(wmata, c("Day","HourN"),nrow,.drop=FALSE)
DateTimeDelay$Day<-as.numeric(DateTimeDelay$Day)
DateTimeDelay$HourN<-as.numeric(DateTimeDelay$HourN)
rownames(DateTimeDelay) <- NULL
DateTimeDelay<-DateTimeDelay[order(DateTimeDelay$Day,DateTimeDelay$HourN), ]

write.csv(DateTimeDelay, 
          file="/Users/katerabinowitz/Documents/DataLensDC/WMATA-Delay/Wmata-Delay/DT-Delays.csv",row.names=FALSE)

###Commuting Hours###
###Commuting Hours###
###Commuting Hours###
wmata$morningCommute<-ifelse(((wmata$Hour=="7 a.m." | wmata$Hour=="8 a.m." | wmata$Hour=="9 a.m.") &
                                (wmata$Weekday!="Saturday" & wmata$Weekday!="Sunday")),'1','0') 
wmata$eveningCommute<-ifelse(((wmata$Hour=="5 p.m." | wmata$Hour=="6 p.m." | wmata$Hour=="7 p.m.") &
                                (wmata$Weekday!="Saturday" & wmata$Weekday!="Sunday")),'1','0') 
wmata$Commute<-ifelse(wmata$morningCommute=='1' | wmata$morningCommute=='1','1','0')