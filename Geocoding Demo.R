library(maptools)
# geocoding example 
permits_path = "/Users/henrygomory/Downloads/buildingpermits.csv"   # downloaded from data.boston.gov

#paths for geocoder references
landParcels_path = "/Users/henrygomory/Documents/Research/BARI/Git/New-BARI/Geographical Infrastructure 2017/LandParcels.2017.csv"
landParcelsShpPath = "/Users/henrygomory/Documents/Research/BARI/Git/New-BARI/Geographical Infrastructure 2017/LandParcels.2017.shp/"
landParcelsShpName = "LandParcels.2017"

roadsCSVPath  = "Documents/Research/BARI/Geographic Infrastructure/Geographical Infrastructure 2015/Roads 2015/roads_updated.csv"
roadsShpPath = "Documents/Research/BARI/Geographic Infrastructure/Geographical Infrastructure 2015/Roads 2015/"
roadsShpName = "roads_updated"


permits_all = read.csv(permits_path, stringsAsFactors=F)
permits_sub = permits_all[ sample(row.names(permits_all[ ]),1000),]




# first must clean
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

sum(!duplicated(permits_sub$PermitNumber))


toGeocode = permits_sub[ !duplicated(permits_sub$PermitNumber),]

# geocode against land parcels, without geographic data
geocoded.lp.nogeo =  geocode(toGeocode = toGeocode,tgID = "PermitNumber",
                             refName = "LandParcels",smallestGeo = "Land_Parcel_ID",expand=T,
          geographies = c("X","Y","TLID","Blk_ID_10","BG_ID_10","CT_ID_10","NSA_NAME","BRA_PD"),
          refCSVPath = landParcels_path)
table(geocoded.lp.nogeo$matchType)

# the geocode prints the number of "full geocodes" - meaning it found a "smallestGeo" - from each type of match
# the functions returns a dataset with a row for every row in the geocoded dataset, whether or not matches were found, and the following fields:
# first is the ID specified in the field tgID
# next is a column for each geography, starting with "smallestGeo"
# next is matchType, which is an important variable summarizing the match
#    Unique means that one and only one item in the reference file matched the record being geocoded, this is the best type of match
#    Number means that one item in the reference file was found, but by finding the reference item that had the closest number, even though it did not match
#           Number can also appear even when maxNumDistance = 0, meaning that it must have been a perfect match on number
#           This seems strange, because one would assume, then, that it would be a "Unique" match. The fact that it is in number implies that there were multiple items
#           in the reference file that matched to our record being geocoded, even on number. Because of that it was not a unique match, but when we checked for matches 
#           that had number differences <= maxNumDistance (0) it did match. IMO these matches can be trusted, but it might be worth looking into why we have multiple 
#           items in the reference file with the same addresses
#    Distance is the geographic equivalent of number. It means that although no unique reference item was found based on the matches, we found the closest one that matched
#           that was less than maxGeoDistance. 
#    Matches but none unique means that some matches were found, but we couldn't narrow it to a single item in the reference file. There may still be extensive geographic
#           information for these records, just no definitive "smallestGeo"
#    No matches simply means that in all of the types of matches attempted, none of them had any matches - this suggests that the record is either junky or not in Boston
# matchVars: the list of variables on which the match that found the item was made. It is empty for  "No matches", and for "Matches but none unique" it is the first match
#    that had any success (typically the most complex). For "Number", the match never has num1 as part of it, that is because the number criteria is attempted based on the 
#    matches that do not match perfectly on number, if that makes sense. In those cases, you know that number was also matched on, in a fuzzy way
# matchGeoDist: the distance in meters of the match, if matchType is "Distance", NA for all other matchTypes
# matchGeoDist: the difference between numbers of match, if matchType is "Number", NA for all other matchTypes
# numMatches: the number of matches in the match that found the reference item, only exists for Unique, Number, Distance matchType (not for "Matches but non unique")
# next is a column for each type of match, with the number of matches found in that match, if it is NA, that is because an earlier match was completely successful, and so the match was not attempted



toGeocode = merge(toGeocode,geocoded.lp.nogeo[,c("PermitNumber","Land_Parcel_ID","CT_ID_10")],by="PermitNumber",all.x=T)

# add fuzzy match to toGeocode
toGeocode = fuzzymatchNames(toGeocode,reference = landParcels_path,referenceType = "path",fuzzyMatch = Inf)

geocoded.lp.nogeo2 = geocode(toGeocode = toGeocode[ is.na(toGeocode$Land_Parcel_ID),],tgID = "PermitNumber",
                            refName = "LandParcels",smallestGeo = "Land_Parcel_ID",expand=T,
                            geographies = c("X","Y","TLID","Blk_ID_10","BG_ID_10","CT_ID_10","NSA_NAME","BRA_PD"),
                            refCSVPath = landParcels_path)
toGeocode = merge_and_move(toGeocode,geocoded.lp.nogeo2,byx="PermitNumber",byy="PermitNumber",allx=T,ally=T,varList = c("Land_Parcel_ID","CT_ID_10"))


# allow less exact number matches
# max num distance allows matches within a certain value to count as exact matches, so 10 Maple St. can match to 20 Maple St.
# fullRange determines whether in the reference file 20-30 Maple St. expands to 20, 22, 24 etc. or 20, 21, 22, etc. 
#     The effect of this is less important when max num distance is enabled because that allows odd to evens matches
#     This should be changed so that maxNumDistance only allows odd to odd or even to even matches (woudld be simple, just change the find closest num function)
# oddsAndEvens is very minor and just determines whether a set of numbers in the toGeocode file like 2-5 is changed to 2-4. By default it is
# the major change here is the maxNumDistance
# it can be useful to allow a high maxNumDistance but then change the geographies to only go for TLID and larger, and then drop the Land_Parcel_ID because it is inexact
# buffer is an alternative. a buffer = 10 tries all matches within 10 numbers of the object being geocoded and then treats them all as matches
#      buffer is more conservative than maxNumDistance because maxNumDistance chooses the one that is closest and number and treats it as a perfect match
#      buffer, instead, only takes geographies if all of the matches share that geography
#      this is more clear if you see how it is implemented in the code
geocoded.lp.nogeo3 = geocode(toGeocode = toGeocode[ is.na(toGeocode$Land_Parcel_ID),],tgID = "PermitNumber",maxNumDistance = 10, fullRange = T,oddsAndEvens = F,
                             refName = "LandParcels",smallestGeo = "Land_Parcel_ID",expand=T,
                             geographies = c("X","Y","TLID","Blk_ID_10","BG_ID_10","CT_ID_10","NSA_NAME","BRA_PD"),
                             refCSVPath = landParcels_path)
# moving over only CT_ID_10
toGeocode = merge_and_move(toGeocode,geocoded.lp.nogeo3,byx="PermitNumber",byy="PermitNumber",allx=T,ally=T,varList = c("CT_ID_10"))



toGeocode.shp = toGeocode[ !is.na(toGeocode$X),]
coordinates(toGeocode.shp) = ~X+Y 
proj4string(toGeocode.shp) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"


#geocode against land parcels, with geographic data
geocoded.lp.geo = geocode(toGeocode = toGeocode.shp[ toGeocode.shp@data$PermitNumber %in% toGeocode$PermitNumber[ is.na(toGeocode$Land_Parcel_ID)],],
                          tgID = "PermitNumber",refName = "LandParcels",smallestGeo = "Land_Parcel_ID",
          geographies = c("X","Y","TLID","Blk_ID_10","BG_ID_10","CT_ID_10","NSA_NAME","BRA_PD"),xy=T,maxGeoDistance=40,
          refShpPath = landParcelsShpPath,
          refShpName = landParcelsShpName,
          refCSVPath = landParcels_path)
toGeocode = merge_and_move(toGeocode,geocoded.lp.geo,byx="PermitNumber",byy="PermitNumber",allx=T,ally=T,varList = c("Land_Parcel_ID","CT_ID_10"))

sum(!is.na(toGeocode$Land_Parcel_ID))/nrow(toGeocode)
sum(!is.na(toGeocode$CT_ID_10))/nrow(toGeocode)




# geocode against streets, without geographic data
geocoded.roads.nogeo = geocode(toGeocode = toGeocode,tgID = "PermitNumber",refName = "Roads",smallestGeo = "TLID",
          geographies = c("BG_ID_10","CT_ID_10","NSA_NAME","BRA_PD"), 
          refCSVPath = roadsCSVPath)


# geocode against streets, with geographic data
geocoded.roads.geo = geocode(toGeocode = toGeocode.shp,tgID = "PermitNumber",refName = "Roads",
          smallestGeo = "TLID",geographies = c("BG_ID_10","CT_ID_10","NSA_NAME","BRA_PD"),xy=T,
          refShpPath = roadsShpPath,
          refShpName = roadsShpName,
          refCSVPath = roadsCSVPath)









