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
wmataRaw$Year.Month <- format(wmataRaw$Date, '%Y-%m')

wmataRaw$Year<-year(wmataRaw$Date)
wmataRaw$Month<-month(wmataRaw$Date)

wmataH1 <- wmataRaw[which(wmataRaw$Month<7),] 

###Aggregate by H1 and yearmonth for trend###
###Aggregate by H1 and yearmonth for trend###
###Aggregate by H1 and yearmonth for trend###
YMdelaySum<-ddply(wmataRaw, c("Year.Month"),nrow)
H1delaySum<-ddply(wmataH1, c("Year"),nrow)
write.csv(YMdelaySum, 
          file="/Users/katerabinowitz/Documents/DataLensDC/WMATA-Delay/Wmata-Delay/YM-Delays.csv")
write.csv(H1delaySum, 
          file="/Users/katerabinowitz/Documents/DataLensDC/WMATA-Delay/Wmata-Delay/H1-Delays.csv")


YMdelayTime<-aggregate(Delay ~ Year.Month, wmataRaw, mean)
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
wmata$Hour<-as.factor(wmata$Hour)
wmata$Hour  <- factor(wmata$Hour,  
                levels  =	c('12 a.m.', '1 a.m.', '2 a.m.', '3 a.m.', '4 a.m.', '5 a.m.', '6 a.m.', '7 a.m.',
                            '8 a.m.', '9 a.m.', '10 a.m.', '11 a.m.', '12 p.m.', '1 p.m.', '2 p.m.', '3 p.m.',
                            '4 p.m.', '5 p.m.', '6 p.m.', '7 p.m.', '8 p.m.', '9 p.m.', '10 p.m.', '11 p.m.'))
#Sum
DateTimeDelay<-ddply(wmata, c("Weekday","Hour"),nrow)
write.csv(DateTimeDelay, 
          file="/Users/katerabinowitz/Documents/DataLensDC/WMATA-Delay/Wmata-Delay/DT-Delays.csv")

###Commuting Hours###
###Commuting Hours###
###Commuting Hours###
wmata$morningCommute<-ifelse(((wmata$Hour=="7 a.m." | wmata$Hour=="8 a.m." | wmata$Hour=="9 a.m.") &
                                (wmata$Weekday!="Saturday" & wmata$Weekday!="Sunday")),'1','0') 
wmata$eveningCommute<-ifelse(((wmata$Hour=="5 p.m." | wmata$Hour=="6 p.m." | wmata$Hour=="7 p.m.") &
                                (wmata$Weekday!="Saturday" & wmata$Weekday!="Sunday")),'1','0') 
wmata$Commute<-ifelse(wmata$morningCommute=='1' | wmata$morningCommute=='1','1','0')