setwd("/Users/katerabinowitz/Documents/DataLensDC/WMATA-Delay/Wmata-Delay")
library(stringr)
library(plyr)
library(reshape)
DelayRaw1<-read.csv("/Users/katerabinowitz/Documents/DataLensDC/WMATA-Delay/Wmata-Delay/WMATAService.csv", 
                 stringsAsFactors=FALSE, strip.white=TRUE)
DelayRaw2<-read.csv("/Users/katerabinowitz/Documents/DataLensDC/WMATA-Delay/Wmata-Delay/WMATAServiceV2.csv", 
                   stringsAsFactors=FALSE, strip.white=TRUE)
DelayRaw<-rbind(DelayRaw1,DelayRaw2)
DelayRaw<-subset(DelayRaw,grepl('[a-z]',DelayRaw$Incident) & !(grepl('Report Archives',DelayRaw$Incident)))

### Dates and Times ###
DelayRaw$LongDay<-gsub("Daily Service Report for ", "",DelayRaw$Date)
Date<-as.data.frame(strsplit(DelayRaw$LongDay,", "))
Date<-as.data.frame(t(Date))
colnames(Date)<-c("Day","MD","Yr")
Delays<-cbind(DelayRaw,Date)

Delays<-subset(Delays,Delays$Incident!="Wednesday November 18, 2014    ")
Delays$DayType<-ifelse(Delays$Day %in% c("Saturday","Sunday"), "Weekend","Weekday")
Delays$Month<-gsub( " .*$", "", Delays$MD)
Delays$DayN<-as.numeric(str_extract(Delays$MD,"[[:digit:]]+"))

Delays$Time<-substr(Delays$Incident,1,10)

Delays$Hour<-ifelse(Delays$Time=="\n5 57 p.m.",5,
                     ifelse(Delays$Time=="\n* 5:00 p.",5,
                      ifelse(Delays$Time=="\n803 a.m. ",8,
                            as.numeric(gsub(":.*$","",Delays$Time)))))

Delays<-subset(Delays,!is.na(Delays$Hour) & !(Delays$Month=="December") & !(Delays$Yr=="2012"))

Delays$Hour24<-ifelse((grepl("p",Delays$Time) & Delays$Hour==12),Delays$Hour,
                        ifelse(grepl("p",Delays$Time),Delays$Hour+12,Delays$Hour))

Delays$TimeGroup<-ifelse(Delays$DayType=="Weekday" & Delays$Hour24 %in% c(6,7,8,9),"Rush",
                          ifelse(Delays$DayType=="Weekday" & Delays$Hour24 %in% c(16,17,18,19),"Rush",
                                  "Off Rush"))
Delays<-Delays[c(3,5:9,13:14)]

### Incident Type ###
#Outcome
Delays$Suspend<-ifelse(grepl("suspend",Delays$Incident),1,
                        ifelse(grepl("Suspend",Delays$Incident),1,0))
Delays$Closed<-ifelse(grepl("close",Delays$Incident),1,
                       ifelse(grepl("Close",Delays$Incident),1,0))
Delays$Offload<-ifelse(grepl("offload",Delays$Incident),1,
                        ifelse(grepl("Offload",Delays$Incident),1,0))
Delays$Shuttle<-ifelse(grepl("Shuttle",Delays$Incident),1,
                        ifelse(grepl("shuttle",Delays$Incident),1,0))
Delays$SingleTrack<-ifelse(grepl("ingle",Delays$Incident),1,0)
Delays$Express<-ifelse(grepl("xpress",Delays$Incident),1,0)
Delays$Reroute<-ifelse(grepl("eroute",Delays$Incident),1,
                       ifelse(grepl("changed to", Delays$Incident),1,0))
#Issue
Equip<-subset(Delays,grepl("equipment problem",Delays$Incident))

Delays$Issue<-ifelse(grepl("perational problem",Delays$Incident),"Operational",
                ifelse((grepl("aintenance",Delays$Incident) | grepl("unscheduled track work",Delays$Incident) | grepl("late",Delays$Incident) |
                grepl("disabled work unit",Delays$Incident) | grepl("ingle tracking",Delays$Incident)),"Maintenance",
                  ifelse((grepl("signal",Delays$Incident)|grepl("switch problem",Delays$Incident) | 
                  grepl("escalator",Delays$Incident)|grepl("elevator",Delays$Incident)|grepl("station overrun",Delays$Incident)
                  |grepl("crowded",Delays$Incident)),"Mechnical-Station",
                    ifelse((grepl("track problem",Delays$Incident)|grepl("cracked rail",Delays$Incident)|grepl("cracked third",Delays$Incident)
                    |grepl("insulator",Delays$Incident)|grepl("disabled track equipment",Delays$Incident) | grepl("3rd rail",Delays$Incident)
                    | grepl("moving track equipment",Delays$Incident)),"Mechnical-Track",
                    ifelse((grepl("equipment problem",Delays$Incident) | grepl("power problem",Delays$Incident) | 
                    grepl("mechanical problem",Delays$Incident)),"Mechnical-General",
                      ifelse((grepl("disabled No",Delays$Incident) | grepl("disabled no",Delays$Incident) | grepl("derail",Delays$Incident) 
                      | grepl("door problem",Delays$Incident) | grepl("brake problem",Delays$Incident) | grepl("break problem",Delays$Incident) 
                      | grepl("disabled train",Delays$Incident) | grepl("disabled Green",Delays$Incident)),"Mechnical-Train",
                          ifelse((grepl("fire",Delays$Incident) | grepl("smoke",Delays$Incident) | grepl("police",Delays$Incident) | 
                          grepl("emergency",Delays$Incident) | grepl("MTPD",Delays$Incident) | grepl("burning",Delays$Incident) | 
                          grepl("smolder",Delays$Incident) | grepl("unattended package",Delays$Incident) | grepl("unattended bag",Delays$Incident) |
                          grepl("suspicious package",Delays$Incident)),"FireCrime",
                            ifelse((grepl("medical",Delays$Incident) | grepl("sick",Delays$Incident) | grepl("unauthorized person",Delays$Incident) | 
                            grepl("passenger interfering",Delays$Incident) | grepl("deer",Delays$Incident) | grepl("weather",Delays$Incident)| 
                            grepl("Presidential",Delays$Incident)| grepl("slippery",Delays$Incident) | grepl("person struck",Delays$Incident) |
                            grepl("passenger struck",Delays$Incident) | grepl("obstruction",Delays$Incident) | grepl("customer",Delays$Incident) |
                            grepl("assist a passenger",Delays$Incident) | grepl("assisting a passenger",Delays$Incident) | 
                            grepl("passenger falling",Delays$Incident) | grepl("child",Delays$Incident) | grepl("bird",Delays$Incident) | 
                            grepl("holding the doors",Delays$Incident)|grepl("dog",Delays$Incident)|grepl("basketball",Delays$Incident)
                            | grepl("passenger incident",Delays$Incident) | grepl("tree",Delays$Incident)),"External",
                              ifelse(grepl("id not operate",Delays$Incident),"Train did not operate",
                                ifelse((grepl("chedule",Delays$Incident) | grepl("eroute",Delays$Incident)),"Schedule Adherence",
                                "NA"))))))))))                               
NAN<-subset(Delays,Delays$Issue=="NA")
sched<-subset(Delays,grepl("schedule adherence",Delays$Incident))
###Delay Time###
Delays$ShortInc<-substr(Delays$Incident,11,1000)
Delays$ShortInc<-gsub('minute.*','',Delays$ShortInc)
Delays$ShortInc<-gsub(".*\\.m.","",Delays$ShortInc)

Delays$Delay<-as.numeric(str_extract(Delays$ShortInc,"[[:digit:]]+"))

#where no delay time reported, estimate based on type of incident
offload<-subset(Delays,Delays$Offload==1 & !is.na(Delays$Delay))
median(offload$Delay)
suspend<-subset(Delays,Delays$Suspend==1 & !is.na(Delays$Delay))
median(suspend$Delay)
shuttle<-subset(Delays,Delays$Shuttle==1 & !is.na(Delays$Delay))
median(shuttle$Delay)
ST<-subset(Delays,Delays$SingleTrack==1 & !is.na(Delays$Delay))
median(ST$Delay)

Delays$Delay<-ifelse((is.na(Delays$Delay)& Delays$Offload==1),8,
                  ifelse((is.na(Delays$Delay)& Delays$Suspend==1),40,
                    ifelse((is.na(Delays$Delay)& Delays$Shuttle==1),40,
                      ifelse((is.na(Delays$Delay)& Delays$SingleTrack==1),20,
                          ifelse((is.na(Delays$Delay)& grepl("ignificant delay",Delays$Incident)),40,
                            ifelse((is.na(Delays$Delay)& grepl("delay",Delays$Incident)),8,
                        Delays$Delay))))))

###Lines and Routes###
Delays$Silver<-ifelse(grepl("Silver",Delays$Incident),1,0)
Delays$Orange<-ifelse(grepl("Orange",Delays$Incident),1,0)
Delays$Blue<-ifelse(grepl("Blue",Delays$Incident),1,0)
Delays$Red<-ifelse(grepl("Red",Delays$Incident),1,0)
Delays$Yellow<-ifelse(grepl("Yellow",Delays$Incident),1,0)
Delays$Green<-ifelse(grepl("Green",Delays$Incident),1,0)

Delays$Bound<-ifelse(grepl("Largo Town Center-bound",Delays$Incident),"Largo",
              ifelse(grepl("Largo-bound",Delays$Incident),"Largo",
                ifelse(grepl("New Carrollton-bound",Delays$Incident),"New Carrollton",
                ifelse(grepl("New Carrolton-bound",Delays$Incident),"New Carrollton",
                  ifelse(grepl("Greenbelt-bound",Delays$Incident),"Greenbelt",
                    ifelse(grepl("Glenmont-bound",Delays$Incident),"Glenmont",
                      ifelse(grepl("Shady Grove-bound",Delays$Incident),"Shady Grove",
                      ifelse(grepl("Shady Grove- bound",Delays$Incident),"Shady Grove",
                        ifelse(grepl("Wiehle-Reston East-bound",Delays$Incident),"Wiehle-Reston East",
                          ifelse(grepl("Vienna-bound",Delays$Incident),"Vienna",
                            ifelse(grepl("Franconia-Springfield-bound",Delays$Incident),"Franconia-Springfield",
                              ifelse(grepl("Branch Avenue-bound",Delays$Incident),"Branch Avenue",
                              ifelse(grepl("Branch Ave-bound",Delays$Incident),"Branch Avenue",
                                ifelse(grepl("Huntington-bound",Delays$Incident),"Huntington",
                                  ifelse(grepl("Grosvenor-Strathmore-bound",Delays$Incident),"Grosvenor-Strathmore",
                                  ifelse(grepl("Grosvenor-bound",Delays$Incident),"Grosvenor-Strathmore",
                                    ifelse(grepl("Mt. Vernon Square-bound",Delays$Incident),"Mt. Vernon Square",
                                    ifelse(grepl("Mt. Vernon-bound",Delays$Incident),"Mt. Vernon Square",
                                    ifelse(grepl("Mt. Vernon Sq-bound",Delays$Incident),"Mt. Vernon Square",
                                      ifelse(grepl("Silver Spring-bound",Delays$Incident),"Silver Spring",
                                        ifelse(grepl("Fort Totten-bound",Delays$Incident),"Fort Totten",
                                        ifelse(grepl("Ft Totten-bound",Delays$Incident),"Fort Totten",
                                        ifelse(grepl("Ft. Totten-bound",Delays$Incident),"Fort Totten",
                                          ifelse(grepl("NoMa-Gallaudet-bound",Delays$Incident),"NoMa-Gallaudet",
                                            ifelse(grepl("Medical Center-bound",Delays$Incident),"Medical Center",
                                              ifelse(grepl("Friendship Heights-bound",Delays$Incident),"Friendship Height",
                                                ifelse(grepl("Van Dorn Street-bound",Delays$Incident),"Van Dorn Street",
                                                  ifelse(grepl("Judiciary Square-bound",Delays$Incident),"Judiciary Square",
                                                    ifelse(grepl("Grosvenor -bound",Delays$Incident),"Grosvenor-Strathmore",
                                                      ifelse(grepl("Branch Ave -bound",Delays$Incident),"Branch Avenue",
                                                        ifelse(grepl("Greenbelt Ave-bound",Delays$Incident),"Greenbelt Avenue",
                                                          ifelse(grepl("Van Ness-bound",Delays$Incident),"Van Ness",
                                        "N/A"))))))))))))))))))))))))))))))))


### Delay Stats ###
aggregate(Delays$Delay, by=list(Delays$TimeGroup), FUN=mean, na.rm=TRUE)
count(Delays$Delay, c('Delays$TimeGroup'))
aggregate(Delays$Delay, by=list(Delays$DayType), FUN=mean, na.rm=TRUE)
count(Delays$Delay, c('Delays$DayType'))
aggregate(Delays$Delay, by=list(Delays$Day), FUN=mean, na.rm=TRUE)
count(Delays$Delay, c('Delays$Day'))
aggregate(Delays$Delay, by=list(Delays$Yr), FUN=mean, na.rm=TRUE)
count(Delays$Delay, c('Delays$Yr'))

aggregate(Delays$Delay, by=list(Delays$Yr,Delays$Month), FUN=sum, na.rm=TRUE)
myDelayN<-count(Delays$Delay, c('Delays$Month','Delays$Yr'))
myDelay<-cast(myDelayN,Delays.Month~Delays.Yr)
colnames(myDelay)<-c("Month","Yr2013","Yr2014","Yr2015")
MonthDate<-as.data.frame(c(as.Date("2015-04-01"), 
             as.Date("2015-08-01"),
             as.Date("2015-02-01"),
             as.Date("2015-01-01"),
             as.Date("2015-07-01"),
             as.Date("2015-06-01"),
             as.Date("2015-03-01"),
             as.Date("2015-05-01"),
             as.Date("2015-11-01"),
             as.Date("2015-10-01"),
             as.Date("2015-9-01")))
myDelayT<-cbind(myDelay,MonthDate)
myDelayT<-myDelayT[c(2:5)]
colnames(myDelayT)<-c("2013","2014","2015","date")
myDelayT<- myDelayT[order(myDelayT$date),]
write.csv(myDelay,"myDelayN.csv",row.names=FALSE)
write.csv(myDelayT,"myDelayTN.csv",row.names=FALSE)

AnnualDelay<-aggregate(Delays$Delay, by=list(Delays$Yr), FUN=sum, na.rm=TRUE)
count(Delays$Delay, c('Delays$Yr'))
colnames(AnnualDelay)<-c("Year","x")
AnnualDelay$DelaySum<-round(AnnualDelay$x/60,0)
write.csv(AnnualDelay,"AnnualDelay.csv",row.names=FALSE)

DelayTime<-subset(Delays,!is.na(Delays$Delay))
count(DelayTime$Delay, c('DelayTime$Yr'))

DelayIssue<-as.data.frame(table(DelayTime$Issue,DelayTime$Yr))
Issue<-cast(DelayIssue,Var1~Var2)[c(1,3:5)]
colnames(Issue)<-c("Issue","Yr2013","Yr2014","Yr2015")
write.csv(Issue,"DelayIssue.csv",row.names=FALSE)

Schedule15<-subset(DelayTime,DelayTime$Issue=="Schedule" & DelayTime$Yr=="2015")
DelayTime$DNO<-ifelse(grepl("did not operate",DelayTime$Incident),1,0)
table(DelayTime$DNO,DelayTime$Yr)

#Delays by Line
DelaySums<-Delays[c("Silver","Red","Orange","Blue","Yellow","Green","Delay")]
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