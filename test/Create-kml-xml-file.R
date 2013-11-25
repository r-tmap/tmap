##XML-example

data <-  read.csv(textConnection('"date","UYG.Open","UYG.High","UYG.Low","UYG.Close","UYG.Volume","UYG.Adjusted"
"2007-02-01",71.32,71.34,71.32,71.34,200,69.23
"2007-02-02",72.2,72.2,72.2,72.2,200,70.06
"2007-02-05",71.76,71.76,71.76,71.76,5100,69.63
"2007-02-06",72.85,72.85,72.85,72.85,3800,70.69
"2007-02-07",72.85,72.85,72.85,72.85,0,70.69'), as.is=TRUE)

library(XML)

xml <- xmlTree()
xml$addTag("document", close=FALSE)
for (i in 1:nrow(data)) {
  xml$addTag("row", close=FALSE)
  for (j in names(data)) {
    xml$addTag(j, data[i, j])
  }
  xml$closeTag()
}
xml$closeTag()

# view the result
cat(saveXML(xml))


##Create KML-file with Variable Data for 3-5 length-cat loops
##Set working directory
setwd("C:/NDW/uur_bestanden/Grens")

##Create csv-file for kml-file

##Load file with loops locs
gps2 <- read.csv(file = "Loops_file2e.csv", header = TRUE)
##select loops with 3 or more length categories
gps3 <- subset(gps2, lengthCat > 2)

##create XML file
library(XML)

xml <- xmlTree() ##"kml", namespaces=list("http://www.opengis.net/kml/2.2"))
xml$addTag("Document", close=FALSE)
for (i in 1:nrow(gps3)) {
  ##Init Placemark
  xml$addTag("Placemark", close=FALSE)  
  
  ##Add name tage
  xml$addTag("name", gps3[i, 1])
  
  ##Create Extended Data table tag
  xml$addTag("ExtendedData", close=FALSE)
  
  ##add Data nodes
  for(j in 2:length(gps3)){
    xml$addTag("Data", attrs = c("name" = names(gps3[j])), close=FALSE)
    xml$addTag("value", gps3[i,j])
    xml$closeTag()
  }
  
  xml$closeTag()
  
  ##Add coordinates of points
  xml$addTag("Point", close=FALSE)
  xml$addTag("coordinates", paste(gps3$locationForDisplay_longitude[i], gps3$locationForDisplay_latitude[i], sep=","))
  xml$closeTag()
  
  ##Close placemark
  xml$closeTag()
}
xml$closeTag()

##saveXML(xml)

##Store result
write.csv(saveXML(xml), file="Test2b.kml", row.names=FALSE, quote=FALSE)

##show results
cat(saveXML(xml))

