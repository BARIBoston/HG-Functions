##### LOOK IN GEOCODING FUNCTIONS R FILE FOR DETAILED NOTES ON HOW TO USE THE GEOCODER



library(maptools)
# geocoding example 
permits_path = "/Users/henrygomory/Downloads/buildingpermits.csv"   # downloaded from data.boston.gov

#paths for geocoder references
landParcels_path = "/Users/henrygomory/Documents/Research/BARI/Git/New-BARI/Geographical Infrastructure 2017/LandParcels.2017.csv"
landParcelsShpPath = "/Users/henrygomory/Documents/Research/BARI/Git/New-BARI/Geographical Infrastructure 2017/LandParcels.2017.shp/"
landParcelsShpName = "LandParcels.2017"

samPath = "Documents/Research/BARI/Git/New-BARI/Geographical Infrastructure 2017/sam_wGeos.csv"

roadsCSVPath  = "Documents/Research/BARI/Geographic Infrastructure/Geographical Infrastructure 2015/Roads 2015/roads_updated.csv"
roadsShpPath = "Documents/Research/BARI/Geographic Infrastructure/Geographical Infrastructure 2015/Roads 2015/"
roadsShpName = "roads_updated"

fuzzyDBPath = "/Users/henrygomory/Documents/Research/BARI/Git/New-BARI/Functions/HG-Functions//fuzzyMatchDB.csv"

# read and subset the permits
permits_all = read.csv(permits_path, stringsAsFactors=F)
permits_sub = permits_all[ sample(row.names(permits_all[ ]),1000),]




# clean the permits before geocoding
# the geocoder needs these address variables
temp = clean_address(permits_sub$ADDRESS)
permits_sub$num1 = temp[,2]
permits_sub$num2 = temp[,3]
permits_sub$street_c = temp[,4]
permits_sub$suffix_c = temp[,5]
rm(temp)
permits_sub$city_c=clean_city(permits_sub$CITY)
permits_sub$zip_c= clean_zip(permits_sub$ZIP)
temp = str_match(permits_sub$Location,"\\(([0-9-.]*), ([0-9-.]*)\\)")
permits_sub$X = as.numeric(temp[,3])
permits_sub$Y = as.numeric(temp[,2])
rm(temp)



permits_sub.shp = permits_sub[ !is.na(permits_sub$X),]
coordinates(permits_sub.shp) = ~X+Y 
proj4string(permits_sub.shp) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"






# geocode against land parcels, without geographic data
geocoded.lp.nogeo =  geocode(toGeocode = permits_sub, tgID = "PermitNumber",refName = "LandParcels",smallestGeo = "Land_Parcel_ID",fuzzyMatching = T, fuzzyMatchDBPath = fuzzyDBPath,
                             geographies = c("X","Y","TLID","Blk_ID_10","BG_ID_10","CT_ID_10","NSA_NAME","BRA_PD"),refCSVPath = landParcels_path,
                             matches = list(
                               list(c("street_c","num1","suffix_c","zip_c"),NA,NA),
                               list(c("street_c","num1","suffix_c","city_c"),NA,NA),
                               list(c("street_c","num1","suffix_c"),NA,NA),
                               list(c("street_c","suffix_c","city_c"),NA,NA),
                               list(c("street_c","suffix_c","zip_c"),NA,NA),
                               list(c("street_c","num1","city_c"),NA,NA),
                               list(c("street_c","suffix_c"),NA,NA),
                               list(c("street_c","num1"),NA,NA),
                               list(c("street_c","zip_c"),NA,NA),
                               list(c("street_c","city_c"),NA,NA),
                               list(c("street_c"),NA,NA)))

# geocode against land parcels, with geographic data
geocoded.lp.geo =  geocode(toGeocode = permits_sub, toGeocodeShp = permits_sub.shp[!duplicated( permits_sub.shp@data$PermitNumber),], tgID = "PermitNumber",refName = "LandParcels",smallestGeo = "Land_Parcel_ID",fuzzyMatching = T, fuzzyMatchDBPath = fuzzyDBPath,
                             geographies = c("X","Y","TLID","Blk_ID_10","BG_ID_10","CT_ID_10","NSA_NAME","BRA_PD"),refCSVPath = landParcels_path,refShpPath = landParcelsShpPath,refShpName = landParcelsShpName,
                             matches = list(
                               list(c("street_c","num1","suffix_c","zip_c"),NA,40),
                               list(c("street_c","num1","suffix_c","city_c"),NA,40),
                               list(c("street_c","num1","suffix_c"),NA,40),
                               list(c("street_c","suffix_c","city_c"),NA,20),
                               list(c("street_c","suffix_c","zip_c"),NA,20),
                               list(c("street_c","num1","city_c"),NA,40),
                               list(c("street_c","suffix_c"),NA,20),
                               list(c("street_c","num1"),NA,40),
                               list(c("street_c","zip_c"),NA,20),
                               list(c("street_c","city_c"),NA,20),
                               list(c("street_c"),NA,20)))


# geocode against streets, without geographic data
geocoded.roads.nogeo = geocode(toGeocode = permits_sub,tgID = "PermitNumber",refName = "Roads",smallestGeo = "TLID",
          geographies = c("BG_ID_10","CT_ID_10","NSA_NAME","BRA_PD"), fuzzyMatching = T, fuzzyMatchDBPath = fuzzyDBPath,
          refCSVPath = roadsCSVPath,
          matches = list(
            list(c("street_c","num1","suffix_c","zip_c"),NA,NA),
            list(c("street_c","num1","suffix_c","city_c"),NA,NA),
            list(c("street_c","num1","suffix_c"),NA,NA),
            list(c("street_c","suffix_c","city_c"),NA,NA),
            list(c("street_c","suffix_c","zip_c"),NA,NA),
            list(c("street_c","num1","city_c"),NA,NA),
            list(c("street_c","suffix_c"),NA,NA),
            list(c("street_c","num1"),NA,NA),
            list(c("street_c","zip_c"),NA,NA),
            list(c("street_c","city_c"),NA,NA),
            list(c("street_c"),NA,NA)))


# geocode against streets, with geographic data
geocoded.roads.geo = geocode(toGeocode = permits_sub,toGeocodeShp = permits_sub.shp[!duplicated( permits_sub.shp@data$PermitNumber),],tgID = "PermitNumber",refName = "Roads",
          smallestGeo = "TLID",geographies = c("BG_ID_10","CT_ID_10","NSA_NAME","BRA_PD"),fuzzyMatching = T, fuzzyMatchDBPath = fuzzyDBPath,
          refShpPath = roadsShpPath,
          refShpName = roadsShpName,
          refCSVPath = roadsCSVPath,
          matches = list(
            list(c("street_c","num1","suffix_c","zip_c"),NA,40),
            list(c("street_c","num1","suffix_c","city_c"),NA,40),
            list(c("street_c","num1","suffix_c"),NA,40),
            list(c("street_c","suffix_c","city_c"),NA,20),
            list(c("street_c","suffix_c","zip_c"),NA,20),
            list(c("street_c","num1","city_c"),NA,40),
            list(c("street_c","suffix_c"),NA,20),
            list(c("street_c","num1"),NA,40),
            list(c("street_c","zip_c"),NA,20),
            list(c("street_c","city_c"),NA,20),
            list(c("street_c"),NA,20)))


# geocode against sam
geocoded.s.nogeo =  geocode(toGeocode = permits_sub,tgID = "PermitNumber",fuzzyMatching = T, fuzzyMatchDBPath = fuzzyDBPath,
                             refName = "Sam",smallestGeo = "Land_Parcel_ID",expand=T,
                             geographies = c("X","Y","TLID","Blk_ID_10","BG_ID_10","CT_ID_10","NSA_NAME","BRA_PD"),
                             refCSVPath = samPath,
                            matches = list(
                              list(c("street_c","num1","suffix_c","zip_c"),NA,NA),
                              list(c("street_c","num1","suffix_c","city_c"),NA,NA),
                              list(c("street_c","num1","suffix_c"),NA,NA),
                              list(c("street_c","suffix_c","city_c"),NA,NA),
                              list(c("street_c","suffix_c","zip_c"),NA,NA),
                              list(c("street_c","num1","city_c"),NA,NA),
                              list(c("street_c","suffix_c"),NA,NA),
                              list(c("street_c","num1"),NA,NA),
                              list(c("street_c","zip_c"),NA,NA),
                              list(c("street_c","city_c"),NA,NA),
                              list(c("street_c"),NA,NA)))


# geocoding against SAM with geographic data has not yet been implemented, but it would be as simple as creating a sam shpfile

###


toGeocode = toGeocode
tgID = "PermitNumber"
refName = "LandParcels"
smallestGeo = "Land_Parcel_ID"
geographies = c("X","Y","TLID","Blk_ID_10","BG_ID_10","CT_ID_10","NSA_NAME","BRA_PD")
refCSVPath = landParcels_path
weirdRange=F
fullRange=F
oddsAndEvens=T
buffer=0
maxGeoDistance = Inf
maxNumDistance = 10
expand = T
xy = F
refShpPath = ""
refShpName = ""
fuzzyMatching = T
fuzzyMatchDBPath ="/Users/henrygomory/Documents/Research/BARI/Git/New-BARI/Functions/fuzzyMatchDB.csv"
batchSize = 3000
planarCRS = "+init=epsg:32619 +units=m"
matches = list(
  list(c("street_c","num1","suffix_c","zip_c"),10,NA), # first NA is num, second is geo
  list(c("street_c","num1","suffix_c","city_c"),10,NA),
  list(c("street_c","num1","suffix_c"),10,NA),
  list(c("street_c","suffix_c","city_c"),NA,40),
  list(c("street_c","suffix_c","zip_c"),NA,40),
  list(c("street_c","num1","city_c"),NA,40),
  list(c("street_c","suffix_c"),NA,NA),
  list(c("street_c","num1"),NA,NA),
  list(c("street_c","zip_c"),NA,NA),
  list(c("street_c","city_c"),NA,NA),
  list(c("street_c"),NA,NA))



#

refShpPath = landParcelsShpPath
refShpName = landParcelsShpName
refCSVPath = landParcels_path

toGeocodeShp = toGeocode.shp

