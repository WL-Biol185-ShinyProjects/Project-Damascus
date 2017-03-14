library(RDSTK)
library(ggmap)

lon <- geocode(Clean_Hospital_Data$Street_Address[1:100])
Clean_Hospital_Data$Longitude <- lon[1]
Clean_Hospital_Data$Latitude <- lon[2]