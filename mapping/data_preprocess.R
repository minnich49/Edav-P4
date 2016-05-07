library(maptools)
library(tm)
library(plyr)
library(rgdal)
library(devtools)
library(rgeos)
library(tigris)
library(UScensus2010)
library(sp)
library(viridis)

# Get census tracts and remove tracts not relevant to the analysis
# map of tracts: http://maps.nyc.gov/census/
nyc <- tracts(state = 'NY', county = c('New York'))
nyc <- nyc[which(nyc@data$NAMELSAD != "Census Tract 5"),]
nyc <- nyc[which(nyc@data$NAMELSAD != "Census Tract 1"),]
nyc <- nyc[which(nyc@data$NAMELSAD != "Census Tract 143"),]

queens <- tracts(state = 'NY', county = c('Queens'))
queens <- queens[which(queens@data$NAMELSAD != "Census Tract 331"),]
queens <- queens[which(queens@data$NAMELSAD != "Census Tract 1072.02"),]
queens <- queens[which(queens@data$NAMELSAD != "Census Tract 716"),]
queens <- queens[which(queens@data$INTPTLAT != min(queens@data$INTPTLAT)),]

bro <- tracts(state = 'NY', county = c('Kings'))
bro <- bro[which(bro@data$NAMELSAD != "Census Tract 702.03"),]
bro <- bro[which(bro@data$NAMELSAD != "Census Tract 702.02"),]
bro <- bro[which(bro@data$INTPTLAT != min(bro@data$INTPTLAT)),]

bronx <- tracts(state = 'NY', county = c('Bronx'))
bronx <- bronx[which(bronx@data$NAMELSAD != "Census Tract 1"),]

si <- tracts(state = 'NY', county = c('Richmond'))
si <- si[which(si@data$NAMELSAD != "Census Tract 9901"),]

nyc <- spRbind(nyc,queens)
nyc <- spRbind(nyc,bro)
nyc <- spRbind(nyc,bronx)
nyc <- spRbind(nyc,si)

nyc@data$NAME <- as.numeric(as.character(nyc@data$NAME ))
nyc@data <- nyc@data[which(nyc@data$AWATER==0),]

df = read.csv("data/Jan_08_2015.csv")

complaints = unique(df$Complaint.Type)

for (complaint in complaints){
  varName = gsub("[\\(\\)-/ ]","_",tolower(complaint))
  heat_hw = df[which(df$Complaint.Type == complaint),]
  heat_hw = heat_hw[,c("Latitude","Longitude","Created.Date")]
  complaintsLatLon = na.omit(heat_hw[which(grepl(".+",heat_hw$Created.Date)),])
  if (dim(complaintsLatLon)[1] == 0){next}
  
  # http://gis.stackexchange.com/questions/64513/checking-if-lng-and-lat-fall-inside-polygons-from-esri-shape-file
  # http://gis.stackexchange.com/questions/63793/how-to-overlay-a-polygon-over-spatialpointsdataframe-and-preserving-the-spdf-dat
  # http://gis.stackexchange.com/questions/141469/how-to-convert-a-spatialpolygon-to-a-saptialpolygonsdataframe-and-add-a-column-t
  # http://r-sig-geo.2731867.n2.nabble.com/point-in-polygon-or-over-help-td7583635.html
  # http://gis.stackexchange.com/questions/110117/counts-the-number-of-points-in-a-polygon-in-r

  pid <- sapply(slot(nyc, "polygons"), function(x) slot(x, "ID")) 

  nyc2.df <- data.frame(ID=1:length(nyc), row.names = pid) 

  nyc2 <- SpatialPolygonsDataFrame(nyc,nyc2.df)

  nyc2@data = nyc@data 

  complaintsLatLon = complaintsLatLon[,c("Longitude","Latitude")]
  coordinates(complaintsLatLon) <- ~ Longitude + Latitude
  proj4string(complaintsLatLon) <- CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0") 
  
  overVals <- over(complaintsLatLon,nyc2)
  binnedValues <- table(overVals$NAMELSAD) 
  binnedValues <- as.data.frame(binnedValues)
  if (length(binnedValues) == 1){next}
  
  colnames(binnedValues) = c("NAMELSAD",varName)

  nyc@data = join(binnedValues, nyc@data, by = "NAMELSAD",type = "right")
  nyc@data[is.na(nyc@data)] = 0
}

save(nyc,file="data/nyc.Rda")