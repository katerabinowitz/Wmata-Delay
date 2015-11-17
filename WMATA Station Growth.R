library(rgdal)
WmataHistory<-read.csv(file = "/Users/katerabinowitz/Documents/DataLensDC/WMATA-Delay/Wmata-Delay/Wmata_Growth.csv",
                       stringsAsFactors=FALSE, strip.white=TRUE)[c(2:37)]

colnames(WmataHistory)<-c("Station", "M77","M78","M79","M80","M81","M82","M84","M85","M86","M87",
                          "M88","M89","M90","M91","M92","M93","M93","M94","M95","M96","M97","M98","M99",
                          "M00","M01","M02","M03","M04","M05","M06","M07","M08","M09","M10","M11","M12","M13")


WmataMap=readOGR('http://opendata.dc.gov/datasets/ead6291a71874bf8ba332d135036fbda_58.geojson',"OGRGeoJSON")

