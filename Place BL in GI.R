# geocoding business licenses

# read in and get paths needed for geocoding
bl= read.csv("Downloads/BL - June.csv",stringsAsFactors=F)
landParcelsPath = "/Users/henrygomory/Documents/Research/BARI/Git/New-BARI/Geographical Infrastructure 2017/LandParcels.2017.csv"
roadsPath  = "Documents/Research/BARI/Geographic Infrastructure/Geographical Infrastructure 2015/Roads 2015/roads_updated.csv"


# clean business license file, needs "Cleaning functions.R"
# this would be improved by incorporating litpostal, but I don't think it would make a large difference on the end product, though I could be wrong
bl$street_c = clean_streetName(bl$STNAME)
temp = clean_num(bl$STNO)
bl$num1 = temp[,2] 
bl$num2 = temp[,3]
bl$suffix_c = clean_suffix(bl$SUFFIX)
bl$zip_c = clean_zip(bl$ZIP)
bl$city_c = clean_city(bl$CITY)

# create a uniqueID, needed for matching geocode file to the original file 
bl$uniqueGeocodeID = c(1:nrow(bl))

# take a sample, so we don't have to geocode all of them, while in this testing phase
bl_sample = bl[sample(row.names(bl),1000),]

# make subsamples of the data
# citywide licenses, these we won't be able to place
bl_sample_cw = bl_sample[ bl_sample$street_c == "CITYWIDE",]
# empty ones we won't be able to place either
bl_sample_e = bl_sample[ bl_sample$street_c == "",]
# it might also be useful to take out ones that go to common landmarks like Boston Common, the airport, etc. as some of those areas  will not geocode, but we know where they are

# reduce the sample to those that are no citywide or empty
bl_sample = bl_sample[! bl_sample$uniqueGeocodeID %in% bl_sample_cw$uniqueGeocodeID & 
                        ! bl_sample$uniqueGeocodeID %in% bl_sample_e$uniqueGeocodeID , ]

# this checks the street names against names in the land parcels file
# for each street name that doesnt match, we see the closest match, then can choose whether to change the street name in the business license file to that one
bl_sample = fuzzymatchNames(bl_sample,landParcelsPath,referenceType = "path")

# this geocodes against the land parcels file
geocode_lp = geocode(toGeocode = bl_sample,tgID = "uniqueGeocodeID",refName = "LandParcels",smallestGeo = "Land_Parcel_ID",maxNumDistance = 10,
                         geographies = c("X","Y","TLID","Blk_ID_10","BG_ID_10","CT_ID_10","NSA_NAME","BRA_PD"), refCSVPath = landParcelsPath)

# this geocodes against the roads file. it does fuzzy matching again, but this time against the roads file
geocode_r = geocode(toGeocode = bl_sample,tgID = "uniqueGeocodeID",refName = "Roads",smallestGeo = "TLID",maxNumDistance = 10,fuzzyMatching = T,
                     geographies = c("BG_ID_10","CT_ID_10","NSA_NAME","BRA_PD"), refCSVPath = roadsPath)

# with the geocoding done, now we can incorporate the geographic data
# this adds all of the geographic indicators, including Land_Parcel_ID, X, and Y, but only for perfect matches
bl_sample_geo = merge(bl_sample,geocode_lp[ geocode_lp$matchType=="Unique" |  (geocode_lp$matchType == "Number" &  geocode_lp$matchNumDist==0) ,
                                            c("uniqueGeocodeID","Land_Parcel_ID","X","Y","TLID","Blk_ID_10","BG_ID_10","CT_ID_10","NSA_NAME","BRA_PD")],by="uniqueGeocodeID",all.x=T)
# this adds less precise geographic indicators, based on the roads geocode
bl_sample_geo = merge_and_move(bl_sample_geo,geocode_r,byx="uniqueGeocodeID",byy="uniqueGeocodeID",allx=T,ally=T,varList =c("TLID","BG_ID_10","CT_ID_10","NSA_NAME","BRA_PD"))
# this adds less precise geographic indicators, based on the land parcels geocdoe, but for imperfect matches
bl_sample_geo = merge_and_move(bl_sample_geo,geocode_lp[! (geocode_lp$matchType=="Unique" |  (geocode_lp$matchType == "Number" &  geocode_lp$matchNumDist==0)),],
                               byx="uniqueGeocodeID",byy="uniqueGeocodeID",allx=T,ally=T,varList =c("TLID","Blk_ID_10","BG_ID_10","CT_ID_10","NSA_NAME","BRA_PD"))

# this checks how good the geocode was. in two trials i got 77.5% and 78.6% for Land_Parcel_ID, and 96.5% and 97.0% for TLID
sum(!is.na(bl_sample_geo$Land_Parcel_ID))/nrow(bl_sample_geo)
sum(!is.na(bl_sample_geo$TLID))/nrow(bl_sample_geo)





