Delays<-read.csv("/Users/katerabinowitz/Documents/DataLensDC/WMATA-Delay/Wmata-Delay/WMATAService.csv", 
                 stringsAsFactors=FALSE, strip.white=TRUE)

Delays$Time<-substr(Delays$Incident,1,10)
Delays<-subset(Delays, Delays$Time!="Report Arc")

Delays$Line<-ifelse(grepl("Silver",Delays$Incident),"Silver",
                    ifelse(grepl("Orange",Delays$Incident),"Orange",
                           ifelse(grepl("Blue",Delays$Incident),"Blue",
                                  ifelse(grepl("Red",Delays$Incident),"Red",
                                         ifelse(grepl("Yellow",Delays$Incident),"Yellow",
                                                ifelse(grepl("Green",Delays$Incident),"Green","N/A"))))))

Delays$Bound<-ifelse(grepl("Largo Town Center-bound",Delays$Incident),"Largo",
                ifelse(grepl("New Carrollton-bound",Delays$Incident),"New Carrollton",
                  ifelse(grepl("Greenbelt-bound",Delays$Incident),"Greenbelt",
                    ifelse(grepl("Glenmont-bound",Delays$Incident),"Glenmont",
                      ifelse(grepl("Shady Grove-bound",Delays$Incident),"Shady Grove",
                        ifelse(grepl("Wiehle-Reston East-bound",Delays$Incident),"Wiehle-Reston East",
                          ifelse(grepl("Vienna-bound",Delays$Incident),"Vienna",
                            ifelse(grepl("Franconia-Springfield-bound",Delays$Incident),"Franconia-Springfield",
                              ifelse(grepl("Branch Avenue-bound",Delays$Incident),"Branch Avenue",
                                ifelse(grepl("Huntington-bound",Delays$Incident),"Huntington","N/A"))))))))))
                            
check<-subset(Delays,Delays$Bound=="N/A")
