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

#Time variables
temp<-strsplit(wmataRaw$Time, ":")
temptime<-as.data.frame(matrix(unlist(temp), ncol=2, byrow=TRUE,))
temptime$ap <- strsplit(V1, ":")
V1<-as.character(temptime$V2)

### Explore delays by date, time and delays ###
### Explore delays by date, time and delays ###
### Explore delays by date, time and delays ###
delaySum<-ddply(wmataRaw, c("Year.Month"),nrow)
lineSum<-ddply(wmataRaw,c("Line", "Year"),nrow)

delayAvg<-ddply(wmataRaw),c("Year.Month"),
plot(delaySum$Year.Month, delaySum$V1)
delaySum<-ddply(wmataRaw, c("year"), nrow)
