# simple regex trims the string, found online
trim <- function (x) gsub("^\\s+|\\s+$", "", x)  

# finds the mode of a vector (how does r not have this built in)
Mode <- function(x,na.rm=T) {
  if (na.rm) {
    x = x[!is.na(x)]
  }
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


# this merges on a dataset that has variables that are already present in the first one, then adds in those variables 
# where they were missing in the first one and cleans up the variable names
# this is useful for when you have to add data for a bunch of variables from a bunch of sources, for example geographic data from multiple geocodes
# in practice i stopped using this, in order to be more transparent
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



# these two functions combine several vectors of string variables, while not including any that were blank or NA
# actually quite useful for recombining pieces of addresses into a full address, because if the suffix is missing you don't want an NA or an extra space stuck in the middle
# i may have found this online
mass_combine <- function(df,X) {
  result = rep(NA,length(df[,1]))
  for (i in 1:length(df[1,])) {
    result = combine_NA(result,df[,i],X)
  }  
  return(result)
}

# this just combines two vectors of string variables 
combine_NA <- function(a,b,X) {
  a <- lapply(a,as.character)
  a= trim(a)
  a[a==""] <- NA
  b <- lapply(b,as.character)
  b = trim(b)
  b[b==""]<-NA
  c = paste(a,b,sep=X)
  c[is.na(b)] <- a[is.na(b)]
  c[is.na(a)] <- b[is.na(a)]
  c = trim(c)
  return(c)
}




## creates a Z score, with percentage limits allow you to cap extreme values 
ZScore <- function(x,pLimits=c(NA,NA)) {
  if (!is.na(pLimits[1]) & !is.na(pLimits[2])) {
    df = data.frame(x = x, ID = c(1:length(x)))
    pMin = pLimits[1]
    pMax = pLimits[2]
    orderedX = x[!is.na(x)][order(x[!is.na(x)])]
    df$newX = df$x
    df[!is.na(df$x) & df$x< orderedX[floor(pMin*length(orderedX)/100)+1],"newX"]=orderedX[floor(pMin*length(orderedX)/100)+1]
    df[!is.na(df$x) & df$x> orderedX[floor(pMax*length(orderedX)/100)],"newX"]=orderedX[floor(pMax*length(orderedX)/100)]
  }
  else {
    df = data.frame(newX = x, ID = c(1:length(x)))
  }
  x_real = df$newX[!is.na(df$newX)]
  mean_calc<-sum(as.numeric(x_real))/length(x_real)
  stddev_calc<-(sum((as.numeric(x_real)-mean_calc)^2)/(length(x_real)-1))^.5 
  df$x_zscore = (as.numeric(df$newX)-mean_calc)/stddev_calc
  return(df[order(df$ID),"x_zscore"])
}

# gives the number of missing and percent nonmissing for our common geographic indicators
check_geo <- function(x) {
  for (columnName in c("X","Y","Land_Parcel_ID","GIS_ID","Property_ID","parcel_num","TLID","Blk_ID_10","BG_ID_10","CT_ID_10","BRA_PD","NSA_NAME")) {
    if (columnName %in% names(x)) {
      print(paste(columnName,"-", sum(is.na(x[,columnName]) | x[,columnName] == 0) ) )
      print(paste("    ",(1-(sum(is.na(x[,columnName]) | x[,columnName] == 0)/nrow(x)))))
    } else {
      print(paste(columnName, " not found",sep=""))
    }
  }
}

placeInGI = function(df,IDConnectorPath,landParcelsPath,landParcelsShpName="",landParcelsShpPath="",roadsPath="",roadsShpPath="",roadsShpName="") {
  geoVars = c("Land_Parcel_ID","X","Y","TLID","Blk_ID_10","BG_ID_10","CT_ID_10","NSA_NAME","BRA_PD")
  for (var in geoVars) { df[,var] = NA}
  df$geoType = NA
  
  df = standardizeGeoNames(df)
  
  IDconnector = read.csv(IDConnectorPath,stringsAsFactors=F)
  landParcels = read.csv(landParcelsPath,stringsAsFactors=F)
  landParcels$TLID = landParcels$TLID_1
  IDconnector.geo = merge(IDconnector, landParcels,by="Land_Parcel_ID",all.x=T)
  
  if ("Property_ID" %in% names(df)) {
    df = merge_and_move(df,
                        IDconnector.geo[!duplicated(IDconnector.geo$Property_ID),],
                        byx = "Property_ID",byy="Property_ID",allx=T,
                        varList=geoVars)
    df$geoType[is.na(df$geoType)&!is.na(df$Land_Parcel_ID)] = "Property_ID"
  }
  else {print("No Property_ID")}
  
  if ("parcel_num" %in% names(df)) {
    df = merge_and_move(df,
                        IDconnector.geo[!duplicated(IDconnector.geo$parcel_num),],
                        byx = "parcel_num",byy="parcel_num",allx=T,
                        varList=geoVars)  
    df$geoType[is.na(df$geoType)&!is.na(df$Land_Parcel_ID)] = "parcel_num"
    print(table(df$geoType))
  } else {print("No parcel_num")}
  
  if ("GIS_ID" %in% names(df)) {
    df = merge_and_move(df,
                        IDconnector.geo[!duplicated(IDconnector.geo$GIS_ID),],
                        byx = "GIS_ID",byy="GIS_ID",allx=T,
                        varList=geoVars)
    df$geoType[is.na(df$geoType)&!is.na(df$Land_Parcel_ID)] = "GIS_ID"
  }  else {print("No GIS_ID")}
  
  if (sum(c("num1","num2","street_c","suffix_c","zip_c","city_c")%in%names(df))==6) {
    df$uniqueGeocodeID = row.names(df)
    df.geocode = df[ is.na(df$Land_Parcel_ID),]
    
    
    # geocode against land parcels, without geographic data
    geocoded = geocode(toGeocode = df.geocode,tgID = "uniqueGeocodeID",refName = "LandParcels",smallestGeo = "Land_Parcel_ID",
                       geographies = c("X","Y","TLID","Blk_ID_10","BG_ID_10","CT_ID_10","NSA_NAME","BRA_PD"), refCSVPath = landParcelsPath)
    df = merge_and_move(df,
                        geocoded,
                        byx = "uniqueGeocodeID",byy="uniqueGeocodeID",allx=T,ally=F,
                        varList=geoVars)
    
    if (sum(c("lat","lng") %in% names(df))==2 & landParcelsShpPath!="" & landParcelsShpName!="") {
      df.geocode.shp = df[ !is.na(df$lat) & is.na(df$Land_Parcel_ID),]
      coordinates(df.geocode.shp) = ~lng+lat 
      proj4string(df.geocode.shp) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
      geocoded = geocode(toGeocode = df.geocode.shp,tgID = "uniqueGeocodeID",refName = "LandParcels",smallestGeo = "Land_Parcel_ID",
              geographies = c("X","Y","TLID","Blk_ID_10","BG_ID_10","CT_ID_10","NSA_NAME","BRA_PD"),
              xy=T, refCSVPath = landParcelsPath,refShpPath = landParcelsShpPath,refShpName = landParcelsShpName)
      df = merge_and_move(df,
                          geocoded,
                          byx = "uniqueGeocodeID",byy="uniqueGeocodeID",allx=T,ally=F
                          varList=geoVars)
    } else { print("Lat/Lng vars or land parcels shapefile path not found")}
    
    if (roadsPath!="") {
      
      geocoded = geocode(toGeocode =df[ is.na(df$L)],tgID = "uniqueGeocodeID",refName = "Roads",smallestGeo = "TLID",
                         geographies = c("BG_ID_10","CT_ID_10","NSA_NAME","BRA_PD"), refCSVPath = roadsPath)
      df = merge_and_move(df,
                          geocoded,
                          byx = "uniqueGeocodeID",byy="uniqueGeocodeID",allx=T,ally=F
                          varList=c("TLID","BG_ID_10","CT_ID_10","NSA_NAME","BRA_PD"))
      
      if (sum(c("lat","lng") %in% names(df))==2 & roadsShpPath!="" & roadsShpName!="") {
        df.geocode.shp = df[ !is.na(df$lat) & is.na(df$Land_Parcel_ID),]
        coordinates(df.geocode.shp) = ~lng+lat 
        proj4string(df.geocode.shp) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
        geocoded = geocode(toGeocode = df.geocode.shp,tgID = "uniqueGeocodeID",refName = "Roads",smallestGeo = "TLID",
                geographies = c("BG_ID_10","CT_ID_10","NSA_NAME","BRA_PD"),
                xy=T, refCSVPath = roadsPath,refShpPath = roadsShpPath,refShpName = roadsShpName)
        df = merge_and_move(df,
                            geocoded,
                            byx = "uniqueGeocodeID",byy="uniqueGeocodeID",allx=T,ally=F
                            varList=c("TLID","BG_ID_10","CT_ID_10","NSA_NAME","BRA_PD"))
      }  else { print("Lat/Lng vars or roads shapefile path not found")}
    } else { print("Roads path not found")}
  
    df$geoType[is.na(df$geoType)&!is.na(df$Land_Parcel_ID)] = "Geocode"
    df$uniqueGeocodeID = NULL
  } else { print("Street address vars not found")}
  
  table(df$geoType)
  return(df)
  
}

