library(rgeos)
library(dplyr)
library(maptools)
library(rgdal)
library(stringdist)
#--------------------------------------#
#         Geocoder                     #
#--------------------------------------#

# toGeocode: the data frame or spatial object to be geocoded
#   if xy is true, then it must be a spatial object, with a projection
#   in either case it needs to have the following columns: num1, num2, street_c, suffix_c, zip_c, city_c
#   any of them can be NA (for example city_c is almost always entirely NA, and num2 is almost entirely NA)
#   those columns can be created using the cleaning functions 
# tgID: unique ID for the data frame, must be unique!
# refName: the database to match to, can either be "Roads" or "LandParcels"
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
                    weirdRange=F,fullRange=F,oddsAndEvens=T,buffer=0,maxGeoDistance = Inf,maxNumDistance =0, expand = T,
                    xy = F,refShpPath = "", refShpName = "", fuzzyMatching = F,fuzzyMatchDBPath ="", batchSize = 3000,
                    matches = list(
                      c("street_c","num1","suffix_c","zip_c"),
                      c("street_c","num1","suffix_c","city_c"),
                      c("street_c","num1","suffix_c"),
                      c("street_c","suffix_c","city_c"),
                      c("street_c","suffix_c","zip_c"),
                      c("street_c","num1","city_c"),
                      c("street_c","suffix_c"),
                      c("street_c","num1"),
                      c("street_c","zip_c"),
                      c("street_c","city_c"),
                      c("street_c"))) {
  
  # Part of a series of little messages, since geocoding can take a while
  print("Geocoding")
  
  
  #a helper function used later in the geocode
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
  #--------------------------------#  
  #      LOADING REFERENCE FILE    #
  #--------------------------------#
  
  # this returns the data frame that will be geocoded again
  reference = returnFullReference(refName,weirdRange=weirdRange,buffer=buffer,fullRange=fullRange,refCSVPath,oddsAndEvens=oddsAndEvens)
  # adds smallest geo as the literal name, because later it has to be written out somewhere, and a variable couldn't be passed
  # this is done in a few places for other variables and is kind of annoying and stupid and could probably be fixed by using a different function for aggregation
  reference$smallestGeo = reference[[smallestGeo]]
  

  # if geographic data is included it pulls the geographic reference data
  if (xy) {
    toGeocodeShp = toGeocode
    # projection is changed to planar in order to do gDistance
    toGeocodeShp = spTransform(toGeocodeShp,CRS("+init=epsg:32619 +units=m"))
    toGeocode = toGeocode@data
    referenceShp = readOGR(refShpPath,refShpName,stringsAsFactors=F)
    referenceShp = spTransform(referenceShp,CRS("+init=epsg:32619 +units=m"))
    # this will have to be changed if the name changes
    if (refName == "LandParcels") {referenceShp@data$Land_Parcel_ID = referenceShp@data$Ln_P_ID}
  }
  print("Loaded reference file")

  if (fuzzyMatching) {
    toGeocode = fuzzymatchNames(toGeocode,reference = reference,referenceType="df",fuzzyMatchDBPath=fuzzyMatchDBPath)
  }
  #--------------------------------#  
  #      GEOCODING                 #
  #--------------------------------#
  
  # iterating in batches of 3000
  base = 1
  while (base < nrow(toGeocode)) {
    toRow = ifelse((base+batchSize-1)>nrow(toGeocode),nrow(toGeocode),(base+batchSize-1))
    toGeocode_sub = toGeocode[c(base:toRow),]
    
    if (expand) {
      toGeocode_sub = expandAddresses(toGeocode_sub,tgID)
    }
    base = toRow+1
    
    
    #prepares the dataframe that will hold the geocode information, theres got to be a better way to set up a blank df with a column name held in a variable
    full_geocode = data.frame(ph = unique(toGeocode_sub[[tgID]]))
    full_geocode[,tgID] = full_geocode$ph
    full_geocode$ph = NULL
    for (geo in c(smallestGeo,geographies,"matchNumDist","matchGeoDist","matchType","matchVars","numMatches")) {full_geocode[,geo]=NA}
    for (match in matches) {full_geocode[, paste(match,collapse=".")] = NA}
    
    
    # iterating through each match
    for (match in matches) {
      #merges based on the type of match, between the data.frames, even if there is geographic data
      geocode_raw = merge(
        toGeocode_sub[complete.cases(toGeocode_sub[,match]), c(unique(c(match,"num1")),tgID)],
        reference[complete.cases(reference[,match]), c(unique(c(match,"num1")),smallestGeo,"smallestGeo",geographies)],
        by=match
      )
      
      #if there have been matches
      if (nrow(geocode_raw)!=0) {
        
        #if there is xy data, find closest match based on gDistance
        if (xy) {
          
          # aggregates by the id in the data frame being geocoded, returning the smallest geo ID, the distance, and the number of matches it chose out of
          # it's not a bad idea to implement a maximum distance, and if so, it should be done here, erasing any matches over a certain distance
          # that way, matches could be attempted later, since it will be removed from future matches after this point
          toAgg = geocode_raw[ geocode_raw[,smallestGeo] %in% referenceShp@data[,smallestGeo],]
          agg = by(toAgg,INDICES = toAgg[,tgID],findClosestGeo,referenceShp = referenceShp, toGeocodeShp=toGeocodeShp,smallestGeo=smallestGeo,tgID=tgID)     
          geocode_f = data.frame(matrix(unlist(agg), nrow=nrow(agg), byrow=T),stringsAsFactors=F)
          names(geocode_f) = c(smallestGeo,"matchGeoDist","numMatches")
          geocode_f[tgID] = names(agg)
          geocode_f[ geocode_f$matchGeoDist>maxGeoDistance,c("matchGeoDist",smallestGeo)] = c(NA,NA)
          if (length(setdiff(geocode_raw[[tgID]],geocode_f[[tgID]]))>0) {
            temp =  setdiff(geocode_raw[[tgID]],geocode_f[[tgID]])
            cantAggTGIDs =  data.frame(matchGeoDist = rep(NA,length(temp)),numMatches = rep(NA,length(temp)))
            cantAggTGIDs[,smallestGeo]=NA
            cantAggTGIDs[,tgID]=temp
            geocode_f = rbind(geocode_f, 
                              cantAggTGIDs)
            }
          # merges on geographies
          geocode_f = merge(geocode_f, reference[!duplicated(reference$smallestGeo),c(smallestGeo,geographies)],by=smallestGeo,all.x=T)
          geocode_f$matchType = ifelse(!is.na(geocode_f[[smallestGeo]]),
                                       ifelse(geocode_f$numMatches==1,"Unique","Distance"),NA)
          geocode_f$matchNumDist = NA
        } else {
          #if there is no xy data, for each geography check if all matches are in agreement 
          geocode_raw$tgID_ = geocode_raw[,tgID]
          
          
          
          
          # vvvv This has to be the least efficient way in the world to do this , but it's only 3 lines
          geocode_f = data.frame(ph=NA)
          geocode_f[,tgID]= NA
          geocode_f$ph = NULL
          # otherwise, try to salvage information, for each geography
          for (geo in c(smallestGeo,geographies)) {
            #probably could fix this next line, only was necessary because of string vs. not string variable names in function calls
            # this is what i mentioned earlier about strings in aggregate, kind of stupid and annoying
            if (sum(!is.na(geocode_raw[[geo]])>0)) {
                geocode_raw$geo_ = geocode_raw[,geo]
            #aggregates all matches to see if there is a unique value for that geography (oneOrNone function)
              aggregate_geo = aggregate(geo_~tgID_,geocode_raw,FUN=oneOrNone)
              aggregate_geo[,geo] = aggregate_geo$geo_
              geocode_f = merge(geocode_f,aggregate_geo[,c("tgID_",geo)],by.x=tgID,by.y="tgID_",all=T)
            } else {
              geocode_f[,geo]=NA
            }
          }
          geocode_f = geocode_f[!is.na(geocode_f[,tgID]),]
          geocode_f$matchType = ifelse(!is.na(geocode_f[,smallestGeo]), "Unique",NA)
          geocode_f$matchNumDist = NA
          aggregate_geo = aggregate(smallestGeo~tgID_,geocode_raw,FUN=function(x){length(unique(x))})
          geocode_f$numMatches = aggregate_geo$smallestGeo
          # matchGeoDist is NA, because it was not a geographic match
          if (!"num1" %in% match) {
            geocode_raw_noSmallest = geocode_raw[ !geocode_raw[,tgID] %in% geocode_f[ !is.na(geocode_f[,smallestGeo]),tgID] & 
                                                    !is.na(geocode_raw$num1.x) & !is.na(geocode_raw$num1.y),]
            agg_num = by(geocode_raw_noSmallest,INDICES = geocode_raw_noSmallest[,tgID],findClosestNum,smallestGeo=smallestGeo,tgID=tgID)     
            geocode_f2 = data.frame(matrix(unlist(agg_num), nrow=nrow(agg_num), byrow=T),stringsAsFactors=F)
            if (nrow(geocode_f2)>0) {
            names(geocode_f2) = c(smallestGeo,"matchNumDist","numMatches")
            geocode_f2[tgID] = names(agg_num)
            geocode_f2[ geocode_f2$matchNumDist>maxNumDistance,c("matchNumDist",smallestGeo)] = c(NA,NA)
            
            
            # merges on geographies
            geocode_f2 = merge(geocode_f2, reference[!duplicated(reference$smallestGeo),c(smallestGeo,geographies)],by=smallestGeo,all.x=T)
            geocode_f2$matchType = ifelse(!is.na(geocode_f2[[smallestGeo]]),
                                         ifelse(geocode_f2$numMatches==1,"Unique","Number"),NA)
            geocode_f = merge_and_move(geocode_f,geocode_f2,byx = tgID,byy=tgID,allx=T,ally=T,varList = c(geographies,smallestGeo,"matchType","matchNumDist","numMatches"))
            }
          }
          geocode_f$matchGeoDist = NA 
          
          
          # could also do this based on numMatches vv
        }
        # at this point, geocode_f should have the geographies, and numMatches, matchType, matchNumDist, matchGeoDist
        
        # outputs the type of match that was attempted and how many full and partial geographies we have from that match
        # full are determined if smallestGeo was found, and partial are if any geographic variables were determined
        # for geographic matches, there are only full matches
        print(paste(paste(paste(match,collapse="; "),": Fully geocoded:",sep=""),length(which(!is.na(geocode_f[,smallestGeo]))),sep=" "))
        
        #if we got some full geographies, remove them from the toGeocode_sub file
        if (sum(!is.na(geocode_f[,smallestGeo]))>0) {
          temp= data.frame(ph=geocode_f[!is.na(geocode_f[,smallestGeo]),tgID],geocoded=1)
          temp[,tgID] = temp$ph
          temp$ph = NULL
          toGeocode_sub = merge(toGeocode_sub,
                            temp,
                            by = tgID,
                            all.x=T)
          toGeocode_sub = toGeocode_sub[is.na(toGeocode_sub$geocoded),]
          toGeocode_sub$geocoded <- NULL
        }
        
        # add any geographies we got to the final geocode data file, even partial ones
        # geocode_f holds information for every case that matched at all, even if it matched to a ton of them and even if they were all above the minimum distance
        # we want to bring int hat metadata, but have it get overwritten later by a better geocode, 
        # but also bring in information about geographies from a non-geographic geocode that would not necessarily be overwritten
        # make a variable holding the match
        geocode_f$matchVars = paste(match,collapse="; ")
        
        geocode_f[,paste(match,collapse=".")] = as.numeric(geocode_f$numMatches)
        geocode_f$numMatches[ is.na(geocode_f[,smallestGeo])]=NA
        # move over all geographic data and the number of matches, for all matches
        # matchType and matchGeoDist, matchNumDist should only exist for the perfect matches, matchVars should exist for all matches, so it will be the vars of the first time there was at least one match
        full_geocode = merge_and_move(full_geocode, geocode_f,byx=tgID,byy=tgID,allx=T,ally=T,varList = c(geographies,smallestGeo,paste(match,collapse="."),"matchGeoDist","matchNumDist","matchType","matchVars","numMatches"))
       
                                                 
      }
      else {
        
        # if there were no matches in the original merge it states that here
        print(paste(paste(match,collapse="; "), ": Fully geocoded: 0",sep=" "))
      }
      # if it is still to be geocoded and the number of matches is missing, then the numMatches = 0
      full_geocode[full_geocode[,tgID] %in% toGeocode_sub[,tgID] & is.na(full_geocode[,paste(match,collapse=".")]),paste(match,collapse=".")] =0
      
    }
    
    # at the end of all matches, outputs the final totals
    print("")
    print(paste("ALTOGETHER:",length(which(!is.na(full_geocode[,smallestGeo]))),sep=" "))
  
    full_geocode$matchType[is.na(full_geocode$matchType) & rowSums(full_geocode[,sapply(matches,paste,collapse=".")],na.rm=T)!= 0] = "Matches but none unique"
    full_geocode$matchType[is.na(full_geocode$matchType) ] = "No matches"
    
    if (!exists("full_geocode_FINAL")) {
      full_geocode_FINAL = full_geocode
    } else {
      full_geocode_FINAL = rbind(full_geocode_FINAL,full_geocode)
    }
  }
  #return the geocoded file and the original file
  return(full_geocode_FINAL[,c( c(tgID,smallestGeo,geographies,"matchType","matchVars","matchGeoDist","matchNumDist","numMatches") ,
                           setdiff(names(full_geocode_FINAL), c(tgID,smallestGeo,geographies,"matchType","matchVars","matchGeoDist","matchNumDist","numMatches")))])
}

# a function used in the aggregation step of geographic matches, finds the closest polygon in the refernce file
findClosestGeo = function(df,referenceShp,toGeocodeShp,smallestGeo,tgID) {
    referenceShp_sub  = referenceShp[ referenceShp@data[,smallestGeo] %in% df[,smallestGeo],]
    toGeocodeShp_sub = toGeocodeShp[ toGeocodeShp@data[,tgID] %in% df[,tgID],]
    minDist = Inf
    theRow = NA
    for (i in c(1:nrow(referenceShp_sub@data))) {
      dist = gDistance(toGeocodeShp_sub,referenceShp_sub[i,])
      if (dist < minDist) {
        theRow = i
        minDist = dist
      }
    }
    return(list(referenceShp_sub@data[theRow,smallestGeo],minDist,length(unique(referenceShp_sub@data[,smallestGeo])))) 
}


# a function used in the aggregation step of geographic matches, finds the closest polygon in the refernce file
findClosestNum = function(df,smallestGeo,tgID) {
  df= df[order(as.numeric(df$num1.x)),]
  temp = abs(as.numeric(df$num1.x) - as.numeric(df$num1.y))
  return(list(df[which.min(temp),smallestGeo],min(temp),length(unique(df[,smallestGeo]))))
}
          
fuzzymatchNames = function(df, reference,referenceType,fuzzyMatch=Inf,fuzzyMatchDBPath="") {
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

  uniqueStreet = unique(df$street_c)
  for (i in c(1:length(uniqueStreet))) {
    if (!uniqueStreet[i] %in% reference$street_c) {
      temp = stringdist(uniqueStreet[i],reference$street_c)/nchar(uniqueStreet[i])
      if (min(temp,na.rm = T) < fuzzyMatch) {
        # check if comparison already exists
        dbCheck = grepl(uniqueStreet[i],fmdb$street1)*grepl(reference$street_c[which.min(temp)],fmdb$street2)
        if (sum(dbCheck)>0) {
          # there should only be one, but just in case I add the [1]
          decision = fmdb$decision[dbCheck==1][1]
        } else {
          decision = 2-menu(c("Accept", "Reject"), title=paste(c(uniqueStreet[i],"--->",reference$street_c[which.min(temp)]),collapse=""))
          fmdb[nrow(fmdb)+1,]=c(uniqueStreet[i],reference$street_c[which.min(temp)],decision)
        }
        if (decision) {
          df[!is.na(df$street_c) & df$street_c == uniqueStreet[i],"street_c"] = reference$street_c[which.min(temp)]
          print(paste(c(uniqueStreet[i],"-----YES---->",reference$street_c[which.min(temp)]),collapse=""))
        } else {
          print(paste(c(uniqueStreet[i],"-----NO------",reference$street_c[which.min(temp)]),collapse=""))
        }
      }
    }
  }
  if (fuzzyMatchDBPath !="") {
    write.csv(fmdb,fuzzyMatchDBPath,row.names=F)
  }
  return(df)
}


# returnFullReference and expandAddresses are used to call the reference dataframe
returnFullReference = function(refName,weirdRange=F,buffer=0,fullRange=F,oddsAndEvens=F, refCSVPath) {
  
  # path to roads file or landparcels file; roads file needs to have NSA, BRA_PD, etc. attached - if it doesn't first merge it on from a BG file
  referenceOriginal = read.csv(refCSVPath,stringsAsFactors=F)
  
  if (refName == "LandParcels") {  
    reference_raw = "placeholder"
    
    #loops through the two sets of address variables in a land parcels to get all possible places
    for (num in c(1:2)) {
      parcels_sub = referenceOriginal[,c("Land_Parcel_ID",paste(c("minNum_","maxNum_","street_","suffix_","TLID_"),num,sep="" ),"zip","X","Y","Blk_ID_10","BG_ID_10","CT_ID_10","NSA_NAME","BRA_PD")]
      names(parcels_sub)[2:7]<-c("num1","num2","street_c","suffix_c","TLID","zip_c")
      parcels_sub$zip_c = clean_zip(parcels_sub$zip_c)
      parcels_sub$city_c=NA
      if (!is.data.frame(reference_raw)) {
        reference_raw =  parcels_sub
      } else {
        reference_raw = rbind(reference_raw,parcels_sub)
      }
      reference_raw = reference_raw[!is.na(reference_raw$num1),]
    }
    
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

