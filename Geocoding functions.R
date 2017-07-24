library(rgeos)
library(dplyr)
library(maptools)
library(rgdal)
library(stringdist)
#--------------------------------------#
#         Geocoder                     #
#--------------------------------------#
emptyDF = function(varNames){
  emptyDF = data.frame()
  for (var in varNames) {emptyDF[,var]=logical(0)}
  return(emptyDF)
}

getReferenceShpFile = function(refShpPath,refShpName,planarCRS,refName) {
    # if geographic data is included it pulls the geographic reference data
    referenceShp = capture.output(readOGR(refShpPath,refShpName,stringsAsFactors=F))
    referenceShp = spTransform(referenceShp,CRS(planarCRS))
    # this will have to be changed if the name changes
    if (refName == "LandParcels") {referenceShp@data$Land_Parcel_ID = referenceShp@data$Ln_P_ID}    
    return(referenceShp)
}

prepareFileToGeocode = function(toGeocode,fuzzyMatching,reference,fuzzyMatchDBPath) {
  for (var in c("num1","num2","street_c","suffix_c","unit_c","zip_c","city_c")) {
    if (!var %in% names(toGeocode)) {
      print(paste(var, "not found in toGeocode; set to NA"))
      toGeocode[,var]=NA
    }
  }
  
  toGeocode$num1 = clean_num(toGeocode$num1)[,2]
  toGeocode$num2 = clean_num(toGeocode$num2)[,2]
  toGeocode$street_c = clean_streetName(toGeocode$street_c)
  toGeocode$unit_c = (clean_unit(unit = toGeocode$unit_c, num = toGeocode$num1))
  toGeocode$suffix_c = clean_suffix(toGeocode$suffix_c)
  toGeocode$zip_c = clean_zip(toGeocode$zip_c)
  toGeocode$city_c = clean_city(toGeocode$city_c)
  
  if (fuzzyMatching) {
    toGeocode = fuzzymatchNames(df = toGeocode,reference = reference,referenceType="df",fuzzyMatch = Inf,fuzzyMatchDBPath=fuzzyMatchDBPath)
  }
  return(toGeocode)
}

getRawMatches = function(toGeocode_sub,reference,match,smallestGeo,geographies,maxNumDifference,tgID) {
  rawMatches = merge(
    toGeocode_sub[complete.cases(toGeocode_sub[,match]), c(match,tgID)],
    reference[complete.cases(reference[,match]), c(match,smallestGeo,geographies)],
    by=match
  )
  
  # make numDiff field, which is NA if num is not in the match
  if ( "num1" %in% match) {
    rawMatches = rename(rawMatches, num1.x = num1)
    rawMatches$num1.y = rawMatches$num1.x
    if (nrow(rawMatches)>0) {
      rawMatches$numDiff = 0
    } else {
      rawMatches$numDiff=logical(0)
    }
  }
  # match not on num, to get imperfect num matches
  # if maxNumDifference is not NA, then these matches are allowed
  if (!is.na(maxNumDifference) & "num1" %in% match & length(match) != 1) {
    toGeocode_sub_notMatched = toGeocode_sub[ !toGeocode_sub[[tgID]] %in% rawMatches[,tgID],]
    match_noNum = setdiff(match,"num1")
    rawMatches_noNum = merge(
      toGeocode_sub_notMatched[complete.cases(toGeocode_sub_notMatched[,match_noNum]), c(match,tgID)],
      reference[complete.cases(reference[,match_noNum]), c(match,smallestGeo,geographies)],
      by=match_noNum
    )
    
    # find the closest match
    rawMatches_noNum$numDiff  = abs(as.numeric(rawMatches_noNum$num1.x)-as.numeric(rawMatches_noNum$num1.y))
    # change something here if we want it to only get matches that are both even or both odd
    rawMatches_noNum = rawMatches_noNum[ !is.na(rawMatches_noNum$numDiff) & 
                                                       rawMatches_noNum$numDiff <= maxNumDifference,]
    rawMatches_noNum = rawMatches_noNum[ order(rawMatches_noNum$numDiff),]
    rawMatches_noNum = rawMatches_noNum[ !duplicated(rawMatches_noNum[,tgID]),]
    
    # add the num matches back in 
    rawMatches = rbind(rawMatches, rawMatches_noNum)
  }
  return(rawMatches)
}


getUnique = function(df,idVar,targetVar) {
  uniqueVals = by(data = df,INDICES = df[[idVar]],FUN = function(x,targetVar){return(oneOrNone(x[[targetVar]]))},targetVar=targetVar)
  uniqueVals = data.frame(ph1 = names(uniqueVals),ph2=as.vector(uniqueVals),stringsAsFactors=F)
  names(uniqueVals) = c(idVar,targetVar)
  return(uniqueVals)
}

getUniqueMatches = function(rawMatches,tgID,smallestGeo,reference,geographies) {
  uniqueMatches = getUnique(rawMatches,tgID,smallestGeo)
  uniqueMatches = uniqueMatches[ !is.na(uniqueMatches[[smallestGeo]]),]
  # get geographies
  uniqueMatches = merge(uniqueMatches, reference[!duplicated(reference[[smallestGeo]]),c(smallestGeo,geographies)],by=smallestGeo,all.x=T)
  if (nrow(uniqueMatches)>0) {
    uniqueMatches$fm_type = "Unique"
    uniqueMatches$fm_geoDist = NA
  } else {
    uniqueMatches$fm_type = character(0)
    uniqueMatches$fm_geoDist = logical(0)
  }
  return(uniqueMatches)
}
# gets geographic matches
getGeoMatches = function(remainingRawMatches,tgID,smallestGeo, geographies,referenceShp,toGeocodeShp,maxGeoDistance,reference) {
  # removing matches that don't have geographic data
  toAggregate = remainingRawMatches[ remainingRawMatches[[smallestGeo]] %in% referenceShp@data[[smallestGeo]] & 
                                       remainingRawMatches[[tgID]] %in% toGeocode.shp@data[[tgID]],]
  if (nrow(toAggregate)>0) {
    geoMatches = by(toAggregate,INDICES = toAggregate[[tgID]],findClosestGeo,referenceShp = referenceShp, toGeocodeShp=toGeocodeShp,smallestGeo=smallestGeo,tgID=tgID)     
    geoMatches = data.frame(ph1 = names(geoMatches),ph2 = unlist(lapply(geoMatches,'[[',1)),ph3 =unlist(lapply(geoMatches,'[[',2)),stringsAsFactors=F)
    names(geoMatches) = c(tgID,smallestGeo,"fm_geoDist")
    geoMatches = geoMatches[ as.numeric(geoMatches$fm_geoDist)<maxGeoDistance,]
    geoMatches = merge(geoMatches,reference[!duplicated(reference[[smallestGeo]]),c(smallestGeo,geographies)],by=smallestGeo,all.x=T)
    if (nrow(geoMatches)>0) {
      geoMatches$fm_type = "Geo"
    } else {
      geoMatches$fm_type = character(0)
    }
    
    return(geoMatches)
  } else {
    return(emptyDF(c(tgID,smallestGeo,geographies,"fm_type","fm_geoDist")))
  }
}

getNonUniqueMatches = function(remainingRawMatches,tgID,geographies,smallestGeo) {
  if (nrow(remainingRawMatches)>0) {
    nonUniqueMatches = data.frame(unique(remainingRawMatches[[tgID]]))
    names(nonUniqueMatches)=tgID
    nonUniqueMatches[,smallestGeo]=NA
    for (geo in geographies) {
      nonUniqueMatches = merge(nonUniqueMatches,getUnique(remainingRawMatches,tgID,geo),by=tgID,all.x=T)
    }
    nonUniqueMatches$fm_type = NA
    nonUniqueMatches$fm_geoDist = NA
    return(nonUniqueMatches)
  } else {
    return(emptyDF(c(tgID,smallestGeo,geographies,"fm_type","fm_geoDist")))
  }
}

addMetaVars = function(allMatches,match,tgID,smallestGeo,rawMatches) {
  if ("num1" %in% match) {
    # get numDiff
    minNumDiff = aggregate(rawMatches[["numDiff"]], by = list(rawMatches[[tgID]]),FUN = min,na.rm=T)
    names(minNumDiff)=c(tgID,"fm_numDiff")
    allMatches = merge(allMatches,minNumDiff,by=tgID,all.x=T)
    # fm_numDiff should be NA if there was not a final match (fm)
    allMatches$fm_numDiff[is.na(allMatches[[smallestGeo]])]=NA
  } else {
    allMatches$fm_numDiff = NA
  }
  
  allMatches$fm_vars = ifelse(is.na(allMatches[[smallestGeo]]),NA,paste(match,collapse="; "))
  
  numMatches = by(rawMatches,INDICES = rawMatches[[tgID]],function(df,x){length(unique(df[[x]][!is.na(df[[x]])]))},x=smallestGeo) 
  numMatches = data.frame(ph1 = names(numMatches),ph2 = as.vector(numMatches),stringsAsFactors=F)
  names(numMatches) = c(tgID,paste(match,collapse="; "))
  allMatches = merge(allMatches,numMatches,by=tgID,all=T)
  # fm_numMatches only exists for final match (fm)
  allMatches$fm_numMatches = ifelse(is.na(allMatches[[smallestGeo]]),NA,allMatches[[paste(match,collapse="; ")]])
  return(allMatches)
}

initialCheck = function(xy, refShpPath, refShpName, toGeocodeShp,toGeocode,refName,tgID){
 
  if (refName != "LandParcels" & refName != "Roads" & refName != "Sam") {
    print("Invalid reference name provided - EXITING")
    return(0)
  }
  if (!tgID %in% names(toGeocode)) {
    print("tgID not in toGeocode data frame - EXITING")
    return(0)
  }
  if (xy) {
    if ( (is.na(refShpPath) | is.na(refShpName))) {
      print("Geographic mode indicated but no spatial reference provided - EXITING")
      return(0)
    }
    if (is.na(toGeocodeShp)) {
      print("Geographic mode indicated but no spatial to geocode file provided - EXITING")
      return(0)
    }
    if ( !"data" %in% slotNames(toGeocodeShp)) {
      print("toGeocodeShp has no slot data, likely wrong type of file - EXITING")
      return(0)
    }
    if ( "data" %in% slotNames(toGeocodeShp)) {
      if (!tgID %in% toGeocodeShp@data) {
        print("tgID not in toGeocodeShp - EXITING")
        return(0)
      }
    }
  }

    return(1)
}

secondaryCheck = function(smallestGeo,referenceShp,toGeocodeShp,geographies,reference,tgID,matches,toGeocode,xy) {
  if (xy) {
    if (! smallestGeo %in% names(referenceShp@data)) {
      print("Reference shapefile missing smallestGeo - EXITING")
      return(0)
    }
  }
  # check that toGeocode and reference each have the right variables
  if (sum(c(geographies,smallestGeo) %in% names(reference)) < length(c(geographies,smallestGeo))) {
    print("Reference file missing some geographies - EXITING")

    return(0)
  }
  
  if (sum(unique(unlist(lapply(matches,'[[',1))) %in% names(reference)) < length(unique(unlist(lapply(matches,'[[',1))))) {
    print("Reference file missing some matching fields - EXITING")
    return(0)
  }
  if (sum(unique(unlist(lapply(matches,'[[',1))) %in% names(toGeocode)) < length(unique(unlist(lapply(matches,'[[',1))))) {
    print("toGeocode file missing some matching fields - EXITING")
    return(0)
  }
  return(1)
}
# toGeocode: the data frame or spatial object to be geocoded
#   if xy is true, then it must be a spatial object, with a projection
#   in either case it needs to have the following columns: num1, num2, street_c, suffix_c, zip_c, city_c
#   any of them can be NA (for example city_c is almost always entirely NA, and num2 is almost entirely NA)
#   those columns can be created using the cleaning functions 
# tgID: unique ID for the data frame, must be unique!
# refName: the database to match to, can either be "Roads","Sam", or "LandParcels"
#   The reference file pointed to must have all of the geographies in the geographies argument
# smallestGeo: the smallest geography after which you deem an object geocoded
#   99% of the time this should be TLID for roads and Land_Parcel_ID for LandParcels
#   I actually can't really think of a time when you would want something different, so maybe it should just be coded in that way
# geographies: a vector of column names of geographies I want to find out about through the geocoding
#   if xy is true, then these will just be merged on from the closest match
#   if xy is false, then if there is a perfect match these will be merged on in the same way
#   but if xy is false and there are multiple matches with no perfect one, these will be added if all of the matches agree for their value
#   for example if there are 10 matches but all are within the same tract, then that tract will be added      
# weirdRange: used when expanding the addresses in the reference file, determines whether 25-3 should be interpreted as 3-25
#   typically I say no, since I think it opens us up to too many potential errors
# fullRange: used when expanding the addresses in the reference file, determines whether 1-5 shoudl be expanded to 1, 3, 5 or 1, 2, 3, 4, 5
#   this is especially important when geocoding to roads
#   i think the best way to use this is to first geocode with it off, then geocode the remains with it on
#   if you have xy it doesn't matter so much, because it will match based on street and then find the clsest
# oddsAndEvens: used when expanding the addresses in the reference file, determines whether 2-5 should be change to 2-4, or 2-3 to 2-2
# buffer: used when expanding addresses in the reference file, if it is 2, 3-7 will expand to 1, 3, 5, 7, 9, allowing for close matches
#   this should be used like fullRange, as a secondary match
# xy: a boolean saying whether the toGeocode has geographic data that can be used, if true then a shapefile for hte reference file will be pulled
#   and a different algorithm is used for matching
# matches: be default it does all possible matches, but here you can specify which pieces of the address you want to match based on
#   it is a list of matches, and they are done in that order, so the more strict matches should be placed first, since they are better matches

# description of process
# the geocoder first pulls a reference dataset, based on refName, that dataset has expanded out its addresses from
#   1-5 Maple St. to 3 rows: 1, 3, 5 Maple St., so that all possible matches are made
# it then merges based on the list of matches. if the default list of matches is specified, first it merges based on street_c, num1, suffix_c, and zip_c, 
#   then it tries with city instead of zip, etc. 
# based on the matches, it figures out the geographies, and it adds them to the data frame to be returned
#   the algorithm for getting geographies is as simple as just copying them over if it is a perfect match
#   or it can find if one geography that is common to all matches if there are multiple matches
#   or it can use geographic data - this is all explained in more detail below
# any that are matched such that we know their "smallest geo" are then excluded from future matches
# after all matches have been attempted, the dataset is returned



geocode <- function(toGeocode,tgID,refName,smallestGeo,geographies=c(),refCSVPath,
                    toGeocodeShp = NA, weirdRange=F,fullRange=F,oddsAndEvens=T,buffer=0, expand = T,
                    refShpPath = NA, refShpName = NA, fuzzyMatching = F,fuzzyMatchDBPath =NA, batchSize = 3000,planarCRS = "+init=epsg:32619 +units=m",
                    matches = list(
                      list(c("street_c","num1","suffix_c","zip_c"),NA,NA), # first NA is num, second is geo
                      list(c("street_c","num1","suffix_c","city_c"),NA,NA),
                      list(c("street_c","num1","suffix_c"),NA,NA),
                      list(c("street_c","suffix_c","city_c"),NA,NA),
                      list(c("street_c","suffix_c","zip_c"),NA,NA),
                      list(c("street_c","num1","city_c"),NA,NA),
                      list(c("street_c","suffix_c"),NA,NA),
                      list(c("street_c","num1"),NA,NA),
                      list(c("street_c","zip_c"),NA,NA),
                      list(c("street_c","city_c"),NA,NA),
                      list(c("street_c"),NA,NA))) {
  
  # Part of a series of little messages, since geocoding can take a while
  print("Starting geocoder")
  
  # doing some manipulations of the arguments
  xy = (sum(!is.na(lapply(matches,'[[',3)))>0)
  if (length(geographies)==0 | (length(geographies == 1) & smallestGeo %in% geographies)) {
    geographies = c()
  } else {
    geographies = setdiff(geographies,smallestGeo)
  }

  # doing an initial check of the arguments
  if (!initialCheck(xy = xy,refShpPath = refShpPath,refShpName = refShpName,toGeocodeShp = toGeocodeShp,toGeocode = toGeocode,refName = refName,tgID = tgID)){return(NA)}

  # this returns the data frame that will be geocoded again
  reference = getReferenceFile(refName = refName,weirdRange=weirdRange,buffer=buffer,fullRange=fullRange,refCSVPath = refCSVPath,oddsAndEvens=oddsAndEvens)
  
  # if xy, get reference shp and make sure toGeocodeShp is in correct projection
  if (xy) {
      referenceShp = getReferenceShpFile(refShpPath = refShpPath,refShpName = refShpName,planarCRS = planarCRS,refName =refName)
      # projection is changed to planar in order to do gDistance
      toGeocodeShp = spTransform(toGeocodeShp,CRS(planarCRS))
  } else {
    referenceShp = NA
    toGeocodeShp = NA
  }
  print("Reference loaded")

  toGeocode = prepareFileToGeocode(toGeocode = toGeocode,fuzzyMatching = fuzzyMatching,reference = reference,fuzzyMatchDBPath = fuzzyMatchDBPath)

  if(!secondaryCheck(smallestGeo = smallestGeo,referenceShp = referenceShp,toGeocodeShp = toGeocodeShp,geographies = geographies,reference = reference,tgID = tgID,matches = matches,toGeocode = toGeocode,xy=xy)) {return(NA)}
  
  print("Starting geocoding")
  # iterating in batches of 3000
  base = 1
  while (base < nrow(toGeocode)) {
    
    # take subset, change base and toRow
    toRow = ifelse((base+batchSize-1)>nrow(toGeocode),nrow(toGeocode),(base+batchSize-1))
    toGeocode_sub = toGeocode[c(base:toRow),]
    base = toRow+1
    
    # expand toGeocode if that option is enabled
    if (expand) {
      toGeocode_sub = expandAddresses(toGeocode_sub,tgID)
    }
 
    
    #prepares the dataframe that will hold the geocode information
    # it has: a unique ID
    #         a row for each geography, including smallest geography
    #         the classifying vars: fm_numDiff, fm_geoDist, fm_type, fm_vars, fm_numMatches
    #         a var for each match type
    full_geocode_sub = data.frame(unique(toGeocode_sub[[tgID]]))
    names(full_geocode_sub ) = tgID
    for (var in c(smallestGeo,geographies,"fm_type","fm_geoDist","fm_numDiff","fm_vars","fm_numMatches",
                  unlist(lapply(matches,FUN = function(x){paste(x[[1]],collapse="; ")})))) {full_geocode_sub[,var]=NA}

    # iterating through each match
    for (matchVars in matches) {
      match = matchVars[[1]]
      maxNumDifference = matchVars[[2]]
      maxGeoDistance = matchVars[[3]]
      
      # get all of the potential matches, that will then be reduced down to real data
      # uniqueMatches, geoMatches, nonUnique matches are non-overlapping and each follow the same variable format, so they are then rbinded
      rawMatches = getRawMatches(toGeocode = toGeocode_sub,reference = reference,match = match,smallestGeo = smallestGeo,geographies = geographies,maxNumDifference = maxNumDifference,tgID = tgID)
      
      if (nrow(rawMatches)>0) {
        # first processing of raw matches, gets unique matches
        uniqueMatches = getUniqueMatches(rawMatches = rawMatches,tgID = tgID,smallestGeo = smallestGeo,reference = reference,geographies = geographies)
       
        # removes unique matches, gets geo matches
        remainingRawMatches = rawMatches[ !rawMatches[[tgID]] %in% uniqueMatches[[tgID]],]
        if (!is.na(maxGeoDistance)) {
          geoMatches = getGeoMatches(remainingRawMatches = remainingRawMatches,tgID = tgID,smallestGeo = smallestGeo,geographies =  geographies,referenceShp = referenceShp,toGeocodeShp = toGeocodeShp,maxGeoDistance = maxGeoDistance,reference = reference)
          remainingRawMatches = remainingRawMatches[ !remainingRawMatches[[tgID]] %in% geoMatches[[tgID]],]
        } else {
          geoMatches = emptyDF(c(tgID,smallestGeo,geographies,"fm_type","fm_geoDist"))
        }
        
        # removes geo matches, gets non unique matches
        nonUniqueMatches = getNonUniqueMatches(remainingRawMatches = remainingRawMatches,tgID = tgID,geographies = geographies,smallestGeo = smallestGeo)
        
        # create allMatches, add the meta variables
        allMatches = rbind(uniqueMatches,geoMatches,nonUniqueMatches)
        allMatches = addMetaVars(allMatches = allMatches,match = match,tgID = tgID,smallestGeo = smallestGeo,rawMatches = rawMatches)
      
        
        # add data into the full_geocode file
        full_geocode_sub = merge_and_move(full_geocode_sub, allMatches,byx=tgID,allx=T,varList = c(smallestGeo,geographies,paste(match,collapse="; "), "fm_type","fm_geoDist","fm_numDiff","fm_vars","fm_numMatches"))
              
        # remove matches that we have smallestGeo for from toGeocode file
        toGeocode_sub = toGeocode_sub[ ! toGeocode_sub[[tgID]] %in% allMatches[[tgID]][!is.na(allMatches[[smallestGeo]])],]
      }
      
      # if nothing was merged on, the number of matches is made 0
      full_geocode_sub[[paste(match,collapse="; ")]][is.na(full_geocode_sub[[paste(match,collapse="; ")]])]=0
    }
    
    if (!exists("full_geocode")) {
      full_geocode = full_geocode_sub
    } else {
      full_geocode = rbind(full_geocode,full_geocode_sub)
    }
    print(paste(toRow,"/",nrow(toGeocode)," processed. ",sum(!is.na(full_geocode[[smallestGeo]])),"/",toRow," fully geocoded.",sep=""))
  }
  #return the geocoded file and the original file
  full_geocode$fm_type = ifelse(!is.na(full_geocode$fm_type),full_geocode$fm_type,
                                ifelse(rowSums(full_geocode[,unlist(lapply(matches,FUN = function(x){paste(x[[1]],collapse="; ")}))])>0,"Non-Unique matches","No matches"))
  print("Finished geocode: ")
  print(table(full_geocode$fm_vars,full_geocode$fm_type,useNA = "always"))
  
  #rearranges the vars before returning
  full_geocode[,c( c(tgID,smallestGeo,geographies,"fm_type","fm_geoDist","fm_numDiff","fm_vars","fm_numMatches") ,
                   setdiff(names(full_geocode), c(tgID,smallestGeo,geographies,"fm_type","fm_geoDist","fm_numDiff","fm_vars","fm_numMatches")))]
  
  return(full_geocode)
}

# a function used in the aggregation step of geographic matches, finds the closest polygon in the refernce file
findClosestGeo = function(df,referenceShp,toGeocodeShp,smallestGeo,tgID) {
    referenceShp_sub  = referenceShp[ referenceShp@data[[smallestGeo]] %in% df[[smallestGeo]],]
    toGeocodeShp_sub = toGeocodeShp[ toGeocodeShp@data[[tgID]] %in% df[[tgID]],]
    minDist = Inf
    theRow = NA
    for (i in c(1:nrow(referenceShp_sub@data))) {
      dist = gDistance(toGeocodeShp_sub,referenceShp_sub[i,])
      if (dist < minDist) {
        theRow = i
        minDist = dist
      }
    }
    return(list(referenceShp_sub@data[theRow,smallestGeo],minDist)) 
}


# a function used in the aggregation step of geographic matches, finds the closest polygon in the refernce file
findClosestNum = function(df,smallestGeo,tgID) {
  df= df[order(as.numeric(df$num1.x)),]
  temp = abs(as.numeric(df$num1.x) - as.numeric(df$num1.y))
  return(list(df[which.min(temp),smallestGeo],min(temp),length(unique(df[,smallestGeo]))))
}
          
fuzzymatchNames = function(df, reference,referenceType="df",fuzzyMatch=Inf,fuzzyMatchDBPath="") {
  # get reference df
  if (referenceType == "path") {
    reference = read.csv(reference,stringsAsFactors=F)    
  } 
  if ("street_1" %in% names(reference)) {
    reference$street_c = reference$street_1
  }
  # get fuzzy match DB
  if (fuzzyMatchDBPath !="") {
    fmdb= read.csv(fuzzyMatchDBPath,stringsAsFactors=F)
    print("Fuzzy matches being recorded")
  } else {
    print("Fuzzy matches not being recorded")
    fmdb = data.frame(street1=NA,street2=NA,decision=NA)
  }
  uniqueStreet = unique(df$street_c[! df$street_c %in% reference$street_c])
  for (i in c(1:length(uniqueStreet))) {
    temp = stringdist(uniqueStreet[i],reference$street_c)/nchar(uniqueStreet[i])
    if (min(temp,na.rm = T) < fuzzyMatch) {
      # check if comparison already exists
      dbCheck = tryCatch({
        grepl(uniqueStreet[i],fmdb$street1)*grepl(reference$street_c[which.min(temp)],fmdb$street2)
      },error = function(e){
        rep(0,length(fmdb$street1))
      })  
      if (sum(dbCheck)>0) {
        # there should only be one, but just in case I add the [1]
        decision = fmdb$decision[dbCheck==1][1]
      } else {
        decision = 2-menu(c("Accept", "Reject","End fuzzy matching"), title=paste(c(i,"/",length(uniqueStreet),": ",uniqueStreet[i],"--->",reference$street_c[which.min(temp)]),collapse=""))
        if (decision == -1) {
          print("Ending fuzzy matching")
          break
        }
        fmdb[nrow(fmdb)+1,]=c(uniqueStreet[i],reference$street_c[which.min(temp)],decision)
      }
      if (as.numeric(decision)==1) {
        df[!is.na(df$street_c) & df$street_c == uniqueStreet[i],"street_c"] = reference$street_c[which.min(temp)]
        if (sum(dbCheck)>0){print(paste(c(uniqueStreet[i],"-----YES---->",reference$street_c[which.min(temp)]),collapse=""))}
      } else {
        if (sum(dbCheck)>0){print(paste(c(uniqueStreet[i],"-----NO------",reference$street_c[which.min(temp)]),collapse=""))}
      }
    }
    if (i%%50) {
      write.csv(fmdb,fuzzyMatchDBPath,row.names=F)      
    }
  }
  if (fuzzyMatchDBPath !="") {
  }
  return(df)
}


# returnFullReference and expandAddresses are used to call the reference dataframe
getReferenceFile = function(refName,weirdRange=F,buffer=0,fullRange=F,oddsAndEvens=F, refCSVPath) {
  
  # path to roads file or landparcels file; roads file needs to have NSA, BRA_PD, etc. attached - if it doesn't first merge it on from a BG file
  referenceOriginal = read.csv(refCSVPath,stringsAsFactors=F)
  
  if (refName == "LandParcels") {  
    reference_raw = "placeholder"
    
    #loops through the two sets of address variables in a land parcels to get all possible places
    for (num in c(1:2)) {
      parcels_sub = referenceOriginal[,c("Land_Parcel_ID",paste(c("minNum_","maxNum_","street_","suffix_","TLID_"),num,sep="" ),"zip","X","Y","Blk_ID_10","BG_ID_10","CT_ID_10","NSA_NAME","BRA_PD")]
      names(parcels_sub)[2:7]<-c("num1","num2","street_c","suffix_c","TLID","zip_c")
      if (!is.data.frame(reference_raw)) {
        reference_raw =  parcels_sub
      } else {
        reference_raw = rbind(reference_raw,parcels_sub)
      }
      reference_raw = reference_raw[!is.na(reference_raw$num1),]
    }
    # cleans here, in case any changes to the cleaning has been made since constructing the land parcels file, for example i just added something to take out apostrophes in street names
    reference_raw$zip_c = clean_zip(reference_raw$zip_c)
    reference_raw$street_c = clean_streetName(reference_raw$street_c)
    reference_raw$suffix_c = clean_suffix(reference_raw$suffix_c)
    reference_raw$city_c=NA
    
    #gives a generic name to the ID
    reference_raw$ReferenceID = reference_raw$Land_Parcel_ID
    
  } else if (refName=="Roads") {
    
    # cleans the roads file, the land parcels were already cleaned from when it was created
    temp = separate_suffix(referenceOriginal$FULLNAME)
    referenceOriginal$street_c = temp[,2]
    referenceOriginal$suffix_c = temp[,3]
    # separates the left and right side of the street into different rows
    roads_r = rename(referenceOriginal[, c("TLID","RFROMADD","RTOADD","street_c","suffix_c","ZIPR","BG_ID_10","CT_ID_10","NSA_NAME","BRA_PD")],num1 = RFROMADD,num2 = RTOADD,zip = ZIPR)
    roads_r$side = "R"
    roads_l = rename(referenceOriginal[, c("TLID","LFROMADD","LTOADD","street_c","suffix_c","ZIPL","BG_ID_10","CT_ID_10","NSA_NAME","BRA_PD")],num1 = LFROMADD,num2 = LTOADD,zip = ZIPL)
    roads_l$side = "L"
    reference_raw = rbind(roads_r,roads_l)
    # cleans the rest (i'm not sure if there's any reason  cleaned the street and suffix first above)
    reference_raw$zip_c = clean_zip(reference_raw$zip)
    reference_raw$num1 = clean_num(reference_raw$num1)[,2]
    reference_raw$num2 = clean_num(reference_raw$num2)[,2]
    reference_raw$city_c = NA
    # its referenceID is the TLID + a letter specifying the side
    reference_raw$ReferenceID = paste(reference_raw$TLID,reference_raw$side,sep="")
    reference_raw = reference_raw[ ,c("ReferenceID", "num1","num2","street_c","suffix_c","zip_c","city_c","TLID","BG_ID_10","CT_ID_10","NSA_NAME","BRA_PD")]
    reference_raw = reference_raw[!is.na(reference_raw$num1),]
  } else if (refName == "Sam") {
    # reup the cleaning
    reference_raw = referenceOriginal
    reference_raw$street_c = clean_streetName(reference_raw$street_c)
    reference_raw$suffix_c = clean_suffix(reference_raw$suffix_c)
    reference_raw$zip_c = clean_zip(reference_raw$zip_c)
    reference_raw$city_c = clean_city(reference_raw$city_c)
    reference_raw$unit_c = (clean_unit(reference_raw$unit_c,reference_raw$num1))
    reference_raw$ReferenceID = reference_raw$SAM_ADDRESS_ID
  } else {
    print("REFERENCE NAME NOT FOUND")
  }
  
  # after this point reference_raw needs to be in cleaned form
  # this then expands addresses like 1-5 Maple St. to 3 rows: 1, 3, 5 Maple St.
  reference = expandAddresses(reference_raw,weirdRange=weirdRange,buffer=buffer,fullRange=fullRange,oddsAndEvens=oddsAndEvens)
  
  return( reference)
}

# expands addresses like 1-5 Maple St. to 3 rows: 1, 3, 5 Maple St.
# used in other places besides the geocoder (in creating the geographical infrastructure for example, actually might be the only other place)
expandAddresses = function(reference_raw,ReferenceID = "ReferenceID",weirdRange=F,buffer=0,fullRange=F,oddsAndEvens=T) {
  #this expands out addresses that cover multiple numbers into multiple rows
  #if weird range is true it allows ranges where the second number is smaller than the first
  #the problem with this is it interprets 112-3 as 3-112, when it should probably be 112-113!
  
  
  # have to do this stupid thing because later in the group_by you can't pass a variable
  reference_raw$ReferenceID = reference_raw[,ReferenceID]
  
  # some of these number manipulations could be made more efficient, but this feels like clear steps
  # 1. makes them numeric, makes num2 = num1 if num2 is missing, so that adding the buffer will work correctly
  reference_raw$num1 = as.numeric(reference_raw$num1)
  reference_raw$num2 = ifelse(is.na(reference_raw$num2),reference_raw$num1,as.numeric(reference_raw$num2))
  # 2. if weirdRange is enabled, it makes num1 the minimum, num2 the maximum, otherwise it makes num2 = num1 if it is less than it
  if (weirdRange) {
    num2 = reference_raw$num2
    num1 = reference_raw$num1   
    reference_raw$num1 = pmin(num1,num2)
    reference_raw$num2 = pmax(num1,num2)
  } else {
    reference_raw$num2 = ifelse(reference_raw$num2 < reference_raw$num1, reference_raw$num1, reference_raw$num2)
  }
  
  # 3. oddsAndEvens determines whether both numbers in the address must be the same sign, whether 2-5 should be changed to 2-4
  if (oddsAndEvens) {
    reference_raw$num2 = ifelse((reference_raw$num2-reference_raw$num1)%%2 == 0, reference_raw$num2, reference_raw$num2-1)
  }
  
  
  # 4. with the numbers set, it incorporates the buffer
  # right now, if the we allow numbers with the buffer to go down to 0, which might be dangerous
  reference_raw$num1 = ifelse((reference_raw$num1 - buffer)>=0,reference_raw$num1 - buffer,
                              ifelse(reference_raw$num1%%2==1,1,0))

  reference_raw$num2 = reference_raw$num2 + buffer
  
  reference_raw$num_addr = 1
  #calculates the number of addresses that will be created out of each row
  if (fullRange) {
    reference_raw$num_addr = ifelse((reference_raw$num2-reference_raw$num1+1)<1,1,
                                (reference_raw$num2-reference_raw$num1+1))
  } else {
    reference_raw$num_addr = ifelse((ceiling((reference_raw$num2-reference_raw$num1)/2)+1)<1,1,
                                (ceiling((reference_raw$num2-reference_raw$num1)/2)+1))      
  }

  reference_raw$num_addr[ is.na(reference_raw$num_addr)]=1
  # expands the rows with multiple numbers into multiple rows then binds them back in
  reference_raw = reference_raw[rep(row.names(reference_raw),reference_raw$num_addr),]
  if (fullRange) {
    reference_raw = reference_raw %>%
      group_by(ReferenceID) %>%
      mutate(addOn = ((row_number())-1)) %>%
      ungroup()    
  } else {
    reference_raw = reference_raw %>%
      group_by(ReferenceID) %>%
      mutate(addOn = ((row_number()*2)-2)) %>%
      ungroup()  
  }
  
  reference_raw$num1 = as.character(ifelse((reference_raw$num1 + reference_raw$addOn)<=reference_raw$num2,
                                           reference_raw$num1 + reference_raw$addOn,
                                           reference_raw$num2))
  reference_raw$num2 <- NULL
  reference_raw$addOn <- NULL

  return(reference_raw)
}

oneOrNone <- function(x) {
  uniqueVals = unique(x)
  return(ifelse(
    length(uniqueVals) == 1,
    uniqueVals,
    NA
  )
  )
}
# another helper function
merge_and_move <- function(dataset1,dataset2,byx,varList,byy=NA,allx=T,ally=F) {
  if (is.na(byy)) {byy=byx}
  merge = merge(dataset1, dataset2[,c(byy,varList)],by.x=byx,by.y=byy,all.x=allx,all.y=ally )
  for (var in varList) {
    varx = paste(var,".x",sep="")
    vary = paste(var,".y",sep="")
    merge[,var]=ifelse(!is.na(merge[,varx]),merge[,varx],merge[,vary])
    merge[,varx] <- NULL
    merge[,vary] <- NULL
  }
  return(merge)
}
