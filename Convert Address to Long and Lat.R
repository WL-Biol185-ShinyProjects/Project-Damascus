library(RDSTK)
library(ggmap)

lon <- geocode(Street_Addresses$'Street Address'[2500:3337])
lon2B <-geocode(Street_Addresses$'Street Address'[3089:3337])
lon3B <-geocode(Street_Addresses$'Street Address'[2000:2499])
lon4B <-geocode(Street_Addresses$'Street Address'[1500:1999])
lon5B <-geocode(Street_Addresses$'Street Address'[1685:1999])
tablelon2B
