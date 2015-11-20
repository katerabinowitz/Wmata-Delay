Delays<-read.csv("/Users/katerabinowitz/Documents/DataLensDC/WMATA-Delay/Wmata-Delay/WMATAService.csv", 
                 stringsAsFactors=FALSE, strip.white=TRUE)

Delays$Time<-substr(Delays$Incident,1,10)


Delays<-subset(Delays, Delays$Time!="Report Arc")

Delays$Silver<-ifelse(grepl("Silver",Delays$Incident),1,0)
Delays$Orange<-ifelse(grepl("Orange",Delays$Incident),1,0)
Delays$Blue<-ifelse(grepl("Blue",Delays$Incident),1,0)
Delays$Red<-ifelse(grepl("Red",Delays$Incident),1,0)
Delays$Yellow<-ifelse(grepl("Yellow",Delays$Incident),1,0)
Delays$Green<-ifelse(grepl("Green",Delays$Incident),1,0)

Delays$Suspend<-ifelse(grepl("suspend",Delays$Incident),1,
                  ifelse(grepl("Suspend",Delays$Incident),1,0))
Delays$Closed<-ifelse(grepl("close",Delays$Incident),1,
                      ifelse(grepl("Close",Delays$Incident),1,0))
Delays$Offload<-ifelse(grepl("offload",Delays$Incident),1,
                       ifelse(grepl("Offload",Delays$Incident),1,0))

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
                                         
check<-subset(Delays,Delays$Bound=="N/A")
