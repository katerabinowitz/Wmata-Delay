library(stringr)
library(plyr)
Delays<-read.csv("/Users/katerabinowitz/Documents/DataLensDC/WMATA-Delay/Wmata-Delay/WMATAService.csv", 
                 stringsAsFactors=FALSE, strip.white=TRUE)

Delays<-subset(Delays,grepl('[a-z]',Delays$Incident) & !(grepl('Report Archives',Delays$Incident)))

### Dates and Times ###
Delays$LongDay<-gsub("Daily Service Report for ", "",Delays$Date)
Date<-as.data.frame(strsplit(Delays$LongDay,", "))
Date<-as.data.frame(t(Date))
colnames(Date)<-c("Day","MD","Yr")
DelayV2<-cbind(Delays,Date)

DelayV2<-subset(DelayV2,DelayV2$Incident!="Wednesday November 18, 2014    ")
DelayV2$DayType<-ifelse(DelayV2$Day %in% c("Saturday","Sunday"), "Weekend","Weekday")
DelayV2$Month<-gsub( " .*$", "", DelayV2$MD)

DelayV2$Time<-substr(DelayV2$Incident,1,10)
DelayV2$Hour<-ifelse(DelayV2$Time=="\n5 57 p.m.",5,
                     ifelse(DelayV2$Time=="\n* 5:00 p.",5,
                            as.numeric(gsub(":.*$","",DelayV2$Time))))

DelayV2$Hour24<-ifelse((grepl("p",DelayV2$Time) & DelayV2$Hour==12),DelayV2$Hour,
                        ifelse(grepl("p",DelayV2$Time),DelayV2$Hour+12,DelayV2$Hour))

DelayV2$TimeGroup<-ifelse(DelayV2$DayType=="Weekday" & DelayV2$Hour24 %in% c(6,7,8,9),"Rush",
                          ifelse(DelayV2$DayType=="Weekday" & DelayV2$Hour24 %in% c(16,17,18,19),"Rush",
                                  "Off Rush"))
DelayV2<-DelayV2[c(3,5:9,11:12)]

### Incident Type ###
DelayV2$Suspend<-ifelse(grepl("suspend",DelayV2$Incident),1,
                        ifelse(grepl("Suspend",DelayV2$Incident),1,0))
DelayV2$Closed<-ifelse(grepl("close",DelayV2$Incident),1,
                       ifelse(grepl("Close",DelayV2$Incident),1,0))
DelayV2$Offload<-ifelse(grepl("offload",DelayV2$Incident),1,
                        ifelse(grepl("Offload",DelayV2$Incident),1,0))
DelayV2$Shuttle<-ifelse(grepl("Shuttle",DelayV2$Incident),1,
                        ifelse(grepl("shuttle",DelayV2$Incident),1,0))

###Delay Time###
DelayV2$ShortInc<-substr(DelayV2$Incident,11,1000)
DelayV2$ShortInc<-gsub('minute.*','',DelayV2$ShortInc)
DelayV2$ShortInc<-gsub(".*\\.m.","",DelayV2$ShortInc)

DelayV2$Delay<-as.numeric(str_extract(DelayV2$ShortInc,"[[:digit:]]+"))

#where no delay time reported, estimate based on type of incident
offload<-subset(DelayV2,DelayV2$Offload==1 & !is.na(DelayV2$Delay))
median(offload$Delay)
suspend<-subset(DelayV2,DelayV2$Suspend==1 & !is.na(DelayV2$Delay))
median(suspend$Delay)
shuttle<-subset(DelayV2,DelayV2$Shuttle==1 & !is.na(DelayV2$Delay))
median(shuttle$Delay)

DelayV2$Delay<-ifelse((is.na(DelayV2$Delay)& DelayV2$Offload==1),7,
                  ifelse((is.na(DelayV2$Delay)& DelayV2$Suspend==1),40,
                    ifelse((is.na(DelayV2$Delay)& DelayV2$Shuttle==1),40,
                      ifelse((is.na(DelayV2$Delay)& grepl("ignificant delay",DelayV2$Incident)),40,
                        DelayV2$Delay))))

###Lines, Routes, and Stations###
DelayV2$Silver<-ifelse(grepl("Silver",DelayV2$Incident),1,0)
DelayV2$Orange<-ifelse(grepl("Orange",DelayV2$Incident),1,0)
DelayV2$Blue<-ifelse(grepl("Blue",DelayV2$Incident),1,0)
DelayV2$Red<-ifelse(grepl("Red",DelayV2$Incident),1,0)
DelayV2$Yellow<-ifelse(grepl("Yellow",DelayV2$Incident),1,0)
DelayV2$Green<-ifelse(grepl("Green",DelayV2$Incident),1,0)

DelayV2$Bound<-ifelse(grepl("Largo Town Center-bound",DelayV2$Incident),"Largo",
              ifelse(grepl("Largo-bound",DelayV2$Incident),"Largo",
                ifelse(grepl("New Carrollton-bound",DelayV2$Incident),"New Carrollton",
                ifelse(grepl("New Carrolton-bound",DelayV2$Incident),"New Carrollton",
                  ifelse(grepl("Greenbelt-bound",DelayV2$Incident),"Greenbelt",
                    ifelse(grepl("Glenmont-bound",DelayV2$Incident),"Glenmont",
                      ifelse(grepl("Shady Grove-bound",DelayV2$Incident),"Shady Grove",
                      ifelse(grepl("Shady Grove- bound",DelayV2$Incident),"Shady Grove",
                        ifelse(grepl("Wiehle-Reston East-bound",DelayV2$Incident),"Wiehle-Reston East",
                          ifelse(grepl("Vienna-bound",DelayV2$Incident),"Vienna",
                            ifelse(grepl("Franconia-Springfield-bound",DelayV2$Incident),"Franconia-Springfield",
                              ifelse(grepl("Branch Avenue-bound",DelayV2$Incident),"Branch Avenue",
                              ifelse(grepl("Branch Ave-bound",DelayV2$Incident),"Branch Avenue",
                                ifelse(grepl("Huntington-bound",DelayV2$Incident),"Huntington",
                                  ifelse(grepl("Grosvenor-Strathmore-bound",DelayV2$Incident),"Grosvenor-Strathmore",
                                  ifelse(grepl("Grosvenor-bound",DelayV2$Incident),"Grosvenor-Strathmore",
                                    ifelse(grepl("Mt. Vernon Square-bound",DelayV2$Incident),"Mt. Vernon Square",
                                    ifelse(grepl("Mt. Vernon-bound",DelayV2$Incident),"Mt. Vernon Square",
                                    ifelse(grepl("Mt. Vernon Sq-bound",DelayV2$Incident),"Mt. Vernon Square",
                                      ifelse(grepl("Silver Spring-bound",DelayV2$Incident),"Silver Spring",
                                        ifelse(grepl("Fort Totten-bound",DelayV2$Incident),"Fort Totten",
                                        ifelse(grepl("Ft Totten-bound",DelayV2$Incident),"Fort Totten",
                                        ifelse(grepl("Ft. Totten-bound",DelayV2$Incident),"Fort Totten",
                                          ifelse(grepl("NoMa-Gallaudet-bound",DelayV2$Incident),"NoMa-Gallaudet",
                                            ifelse(grepl("Medical Center-bound",DelayV2$Incident),"Medical Center",
                                              ifelse(grepl("Friendship Heights-bound",DelayV2$Incident),"Friendship Height",
                                                ifelse(grepl("Van Dorn Street-bound",DelayV2$Incident),"Van Dorn Street",
                                                  ifelse(grepl("Judiciary Square-bound",DelayV2$Incident),"Judiciary Square",
                                                    ifelse(grepl("Grosvenor -bound",DelayV2$Incident),"Grosvenor-Strathmore",
                                                      ifelse(grepl("Branch Ave -bound",DelayV2$Incident),"Branch Avenue",
                                                        ifelse(grepl("Greenbelt Ave-bound",DelayV2$Incident),"Greenbelt Avenue",
                                                          ifelse(grepl("Van Ness-bound",DelayV2$Incident),"Van Ness",
                                        "N/A"))))))))))))))))))))))))))))))))


### Delay Stats ###
aggregate(DelayV2$Delay, by=list(DelayV2$Bound), FUN=mean, na.rm=TRUE)
aggregate(DelayV2$Delay, by=list(DelayV2$TimeGroup), FUN=mean, na.rm=TRUE)
count(DelayV2$Delay, c('DelayV2$TimeGroup'))
aggregate(DelayV2$Delay, by=list(DelayV2$DayType), FUN=mean, na.rm=TRUE)
count(DelayV2$Delay, c('DelayV2$DayType'))
aggregate(DelayV2$Delay, by=list(DelayV2$Day), FUN=mean, na.rm=TRUE)
count(DelayV2$Delay, c('DelayV2$Day'))
aggregate(DelayV2$Delay, by=list(DelayV2$Yr,DelayV2$Month), FUN=mean, na.rm=TRUE)
count(DelayV2$Delay, c('DelayV2$Yr','DelayV2$Month'))

Saturday<-subset(DelayV2,DelayV2$Day=="Saturday")

DelaySums<-DelayV2[c("Silver","Red","Orange","Blue","Yellow","Green","Delay")]
colSums(DelaySums, na.rm=TRUE)
Silver<-subset(DelaySums,DelaySums$Silver==1 & !is.na(DelaySums$Delay))
mean(Silver$Delay)
Red<-subset(DelaySums,DelaySums$Red==1 & !is.na(DelaySums$Delay))
mean(Red$Delay)
Orange<-subset(DelaySums,DelaySums$Orange==1 & !is.na(DelaySums$Delay))
mean(Orange$Delay)
Blue<-subset(DelaySums,DelaySums$Blue==1 & !is.na(DelaySums$Delay))
mean(Blue$Delay)
Yellow<-subset(DelaySums,DelaySums$Yellow==1 & !is.na(DelaySums$Delay))
mean(Yellow$Delay)
Green<-subset(DelaySums,DelaySums$Green==1 & !is.na(DelaySums$Delay))
mean(Green$Delay)
