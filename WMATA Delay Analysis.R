library(stringr)
library(plyr)
library (dplyr)
library(data.table)
### Read in Data ###
### Read in Data ###
### Read in Data ###
wmataRaw<-read.csv('/Users/katerabinowitz/Documents/DataLensDC/WMATA-Delay/disruptions,6-24.csv',na.strings=c("", "NA"),
                   strip.white=TRUE)[c(1:2,4,7)] 
str(wmataRaw)
### Check completeness of data ###
### Check completeness of data ###
### Check completeness of data ###


### Standardize Dates ###
### Standardize Dates ###
### Standardize Dates ###
wmataRaw$chardate<-as.character(wmataRaw$Date)
temp  <- strsplit(wmataRaw$chardate, "/")
tempdate<-as.data.frame(matrix(unlist(temp), ncol=3, byrow=TRUE))
colnames(tempdate)<-c('month','day','year')

tempdate$fullmonth<-ifelse(!tempdate$month %in% c('10','11','12'),
                paste('0',tempdate$month, sep=''), tempdate$month)

tempdate$month<-ifelse(substr(tempdate$month,1,1) %in% c('0','1'),
                       tempdate$month,paste('0',tempdate$month, sep=''))
wmata<-cbind(wmataRaw,tempdate)
wmata$ym<-paste(wmata$year,wmata$month)

### Explore delays by date, time and delays ###
### Explore delays by date, time and delays ###
### Explore delays by date, time and delays ###
delaySum<-ddply(wmata, c("ym"), nrow)
plot(delaySum$ym, delaySum$V1)
delaySum<-ddply(wmata, c("year"), nrow)
