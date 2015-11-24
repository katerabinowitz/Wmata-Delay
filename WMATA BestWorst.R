library(stringr)
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

DelayV2$Time<-substr(DelayV2$Incident,1,10)
DelayV2$Hour<-ifelse(DelayV2$Time=="\n5 57 p.m.",5,
                     ifelse(DelayV2$Time=="\n* 5:00 p.",5,
                            as.numeric(gsub(":.*$","",DelayV2$Time))))

DelayV2$Hour24<-ifelse((grepl("p",DelayV2$Time) & DelayV2$Hour==12),DelayV2$Hour,
                        ifelse(grepl("p",DelayV2$Time),DelayV2$Hour+12,DelayV2$Hour))

DelayV2$TimeGroup<-ifelse(DelayV2$DayType=="Weekday" & DelayV2$Hour24 %in% c(7,8,9),"Rush",
                          ifelse(DelayV2$DayType=="Weekday" & DelayV2$Hour24 %in% c(17,18,19),"Rush",
                                  "Off Rush"))
DelayV2<-DelayV2[c(3,5:9,11:12)]

###Delay Time###
DelayV2$ShortInc<-gsub(".*\\.m.","",DelayV2$Incident)
DelayV2$Delay<-as.numeric(str_extract(DelayV2$ShortInc,"[[:digit:]]+"))
Check<-subset(DelayV2,is.na(DelayV2$Delay) & !(grepl("was expressed",DelayV2$Incident)))

###Lines, Routes, and Stations###
DelayV2$Silver<-ifelse(grepl("Silver",DelayV2$Incident),1,0)
DelayV2$Orange<-ifelse(grepl("Orange",DelayV2$Incident),1,0)
DelayV2$Blue<-ifelse(grepl("Blue",DelayV2$Incident),1,0)
DelayV2$Red<-ifelse(grepl("Red",DelayV2$Incident),1,0)
DelayV2$Yellow<-ifelse(grepl("Yellow",DelayV2$Incident),1,0)
DelayV2$Green<-ifelse(grepl("Green",DelayV2$Incident),1,0)

DelayV2$Suspend<-ifelse(grepl("suspend",DelayV2$Incident),1,
                  ifelse(grepl("Suspend",DelayV2$Incident),1,0))
DelayV2$Closed<-ifelse(grepl("close",DelayV2$Incident),1,
                      ifelse(grepl("Close",DelayV2$Incident),1,0))
DelayV2$Offload<-ifelse(grepl("offload",DelayV2$Incident),1,
                       ifelse(grepl("Offload",DelayV2$Incident),1,0))

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
