library(stringr)
library(dplyr)

###### HG 6/29 
# A huge amount of this should be replaced by libPostal :L



#--------------------------------------#
#         Cleaning Functions           #
#--------------------------------------#
trim <- function (x) gsub("^\\s+|\\s+$", "", x)  

# this is dumb inefficient code, but it runs quickly enough and seems silly to go back and improve it now - HG 6/2017
# this fixes the name and some common formatting inconsistencies in geographic IDs
standardizeGeoNames <- function(df) {
  
  # property id
  if (!"Property_ID" %in% names(df)) {
    if ("propertyid" %in% names(df)) {df = rename(df, Property_ID = propertyid)}
    if ("property_id" %in% names(df)) {df = rename(df, Property_ID = property_id)} 
    if ("Property_id" %in% names(df)) {df = rename(df, Property_ID = Property_id)} 
    if ("PROPID" %in% names(df)) {df = rename(df, Property_ID = PROPID)}     
  }
  
  # parcel num
  if (!"parcel_num" %in% names(df)) {
    if ("PID" %in% names(df)) {df = rename(df,parcel_num = PID)}
    if ("ParcelNum" %in% names(df)) {df = rename(df,parcel_num = ParcelNum)}
    if ("parcelnum" %in% names(df)) {df = rename(df,parcel_num = parcelnum)}
    if ("Parcel_Num" %in% names(df)) {df = rename(df,parcel_num = Parcel_Num)}
    if ("Parcel_ID" %in% names(df)) {df = rename(df,parcel_num = Parcel_ID)} 
  }
  
  # x
  if (!"X" %in% names(df)) {
    if ("x" %in% names(df)) {df = rename(df,X = x)}
  }
  
  # y
  if (!"Y" %in% names(df)) {
    if ("y" %in% names(df)) {df = rename(df,Y = y)}
  }
  
  # block
  if (!"Blk_ID_10" %in% names(df)) {
    if ("Blk_ID" %in% names(df)) {df = rename(df,Blk_ID_10 = Blk_ID)}
    if ("BLK_ID_10" %in% names(df)) {df = rename(df,Blk_ID_10 = BLK_ID_10)}
    if ("BLK_ID" %in% names(df)) {df = rename(df,Blk_ID_10 = BLK_ID)}
  }
  
  # block group
  if (!"BG_ID_10" %in% names(df)) {
    if ("BG_ID" %in% names(df)) {df = rename(df,BG_ID_10 = BG_ID)}
  }
  
  #tract
  if (!"CT_ID_10" %in% names(df)) {
    if ("CT_ID" %in% names(df)) {df = rename(df,CT_ID_10 = CT_ID)}    
  }
  
  # cleaning values of GIS_ID and parcel_num
  if ("GIS_ID" %in% names(df)) { df$GIS_ID = as.character(as.numeric(gsub("_","",df$GIS_ID))) }
  if ("parcel_num" %in% names(df)) {  df$parcel_num = as.character(as.numeric(gsub("_","",df$parcel_num))) }
  
  return(df)
}


# cleans a vector of number strings of the format: c("3","5", "3-5","3 5","3A","3-5A",etc.)
clean_num <- function(number) {
  
  #initial cleaning
  number = trim(toupper(as.character(number)))
  
  #this is a temporary fix because numbers like 0.21414 were becoming 0 and 21414
  #i should fix the regex but this is simpler for now
  number[number>0 & number<1]<-NA
  
  #separating numbers 
  # one pattern matches a single number, the other matches two numbers, I could and should combine them but I was dumb when i first wrote it
  pattern1 = "([0-9]+)[^0-9]+([0-9]+)"
  match1=str_match(number,pattern1)
  pattern2 = "([0-9]+)"
  match2=str_match(number,pattern2)
  num1 = rep(NA, length(number))
  num2 = rep(NA,length(number))
  # god this is alot of code that would be much simpler with a better regex
  if (ncol(match1)>1) {
    num1=as.integer(trim(match1[,2]))
  }
  if (ncol(match2)>1) {
    num1[is.na(num1)]=as.integer(trim(match2[is.na(num1),2]))
  }
  num1[is.na(num1)]=as.integer(number[is.na(num1)])
  if (ncol(match1)>2) {
    num2=as.integer(trim(match1[,3]))
  }
  
  # added this because otherwise we get 57-1, 57-2, etc. showing up as 1-57
  # there is a larger problem of numbers being written in as 153-5, meaning 153-155, that should be addressed
  num2[!is.na(num2) & !is.na(num1) & num2<num1 ] = NA
  
  # this returns a matrix! so you have to specify [,2] and [,3] to get num1 and num2 
  return(matrix(data = c(number,num1,num2),ncol=3))
}

# cleans and standardizes a vector of streetnames, returns a vector
# again, a lot of the regexes could be rewritten and combined to be much more efficient
clean_streetName <- function(streetName) {
  streetName <- trim(toupper(streetName))
  streetName <- gsub("&#039","",streetName)
  streetName <- gsub("'","",streetName)
  
  streetName=gsub("^E ","EAST ",streetName)
  streetName=gsub(" E "," EAST ",streetName)
  streetName=gsub("^E\\. ","EAST ",streetName)
  streetName=gsub(" E\\. "," EAST ",streetName)
  
  streetName=gsub("^S ","SOUTH ",streetName)
  streetName=gsub(" S "," SOUTH ",streetName)
  streetName=gsub("^S\\. ","SOUTH ",streetName)
  streetName=gsub(" S\\. "," SOUTH ",streetName)
  
  streetName=gsub("^W ","WEST ",streetName)
  streetName=gsub(" W "," WEST ",streetName)
  streetName=gsub("^W\\. ","WEST ",streetName)
  streetName=gsub(" W\\. "," WEST ",streetName)
  
  streetName=gsub("^N ","NORTH ",streetName)
  streetName=gsub(" N "," NORTH ",streetName)
  streetName=gsub("^N\\. ","NORTH ",streetName)
  streetName=gsub(" N\\. "," NORTH ",streetName)
  
  streetName=gsub("^MT ","MOUNT ",streetName)
  streetName=gsub(" MT "," MOUNT ",streetName)
  streetName=gsub("^MT\\. ","MOUNT ",streetName)
  streetName=gsub(" MT\\. "," MOUNT ",streetName)
  
  streetName=gsub("^ST\\. ","SAINT ",streetName)
  streetName=gsub(" ST\\. "," SAINT ",streetName)
  streetName=gsub("^ST ","SAINT ",streetName)
  streetName=gsub(" ST "," SAINT ",streetName)
  
  streetName=gsub("^PK ","PARK ",streetName)
  streetName=gsub(" PK "," PARK ",streetName)
  
  streetName=gsub("^1(ST)? ","FIRST ",streetName)  
  streetName=gsub("^1(ST)?$","FIRST",streetName)  
  streetName=gsub(" 1(ST)? "," FIRST ",streetName)
  streetName=gsub(" 1(ST)?$"," FIRST",streetName)  
  
  streetName=gsub("^2(ND)? ","SECOND ",streetName)  
  streetName=gsub(" 2(ND)? "," SECOND ",streetName)  
  streetName=gsub("^2(ND)?$","SECOND",streetName)  
  streetName=gsub(" 2(ND)?$"," SECOND",streetName)
  
  streetName=gsub("^3(RD)? ","THIRD ",streetName)  
  streetName=gsub(" 3(RD)? "," THIRD ",streetName)  
  streetName=gsub("^3(RD)?$","THIRD",streetName)  
  streetName=gsub(" 3(RD)?$"," THIRD",streetName) 
  
  streetName=gsub("^4(TH)? ","FOURTH ",streetName)  
  streetName=gsub(" 4(TH)? "," FOURTH ",streetName)   
  streetName=gsub("^4(TH)?$","FOURTH",streetName)  
  streetName=gsub(" 4(TH)?$"," FOURTH",streetName) 
  
  streetName=gsub("^5(TH)? ","FIFTH ",streetName)  
  streetName=gsub(" 5(TH)? "," FIFTH ",streetName)  
  streetName=gsub("^5(TH)?$","FIFTH",streetName)  
  streetName=gsub(" 5(TH)?$"," FIFTH",streetName) 
  
  streetName=gsub("^6(TH)? ","SIXTH ",streetName)  
  streetName=gsub(" 6(TH)? "," SIXTH ",streetName)  
  streetName=gsub("^6(TH)?$","SIXTH",streetName)  
  streetName=gsub(" 6(TH)?$"," SIXTH",streetName)  
  
  streetName=gsub("^7(TH)? ","SEVENTH ",streetName)  
  streetName=gsub(" 7(TH)? "," SEVENTH ",streetName)  
  streetName=gsub("^7(TH)?$","SEVENTH",streetName)  
  streetName=gsub(" 7(TH)?$"," SEVENTH",streetName)  
  
  streetName=gsub("^8(TH)? ","EIGHTH ",streetName)  
  streetName=gsub(" 8(TH)? "," EIGHTH ",streetName)  
  streetName=gsub("^8(TH)?$","EIGHTH",streetName)  
  streetName=gsub(" 8(TH)?$"," EIGHTH",streetName)  
  
  streetName=gsub("^9(TH)? ","NINTH ",streetName)  
  streetName=gsub(" 9(TH)? "," NINTH ",streetName)  
  streetName=gsub("^9(TH)?$","NINTH",streetName)  
  streetName=gsub(" 9(TH)?$"," NINTH",streetName)  
  
  streetName=gsub("^13(TH)? ","THIRTEENTH ",streetName)  
  streetName=gsub(" 13(TH)? "," THIRTEENTH ",streetName)  
  streetName=gsub("^13(TH)?$","THIRTEENTH",streetName)  
  streetName=gsub(" 13(TH)?$"," THIRTEENTH",streetName)  
  
  streetName=gsub("^16(TH)? ","SIXTEENTH ",streetName)  
  streetName=gsub(" 16(TH)? "," SIXTEENTH ",streetName)
  streetName=gsub("^16(TH)?$","SIXTEENTH",streetName)  
  streetName=gsub(" 16(TH)?$"," SIXTEENTH",streetName)  
  
  #boston specific
  streetName[streetName=="MEDCALF"] = "METCALF"
  streetName[streetName=="JULIET"] = "JULIETTE"
  streetName[streetName=="MLK"] = "MARTIN LUTHER KING"
  streetName[streetName=="MONSIGNOR DENNIS F O'CALLAGHAN"|streetName=="O'CALLAGHAN"|streetName=="MSGR. DENNIS F. O'CALLAGHAN"|streetName=="MSGR DENNIS F. O'CALLAGHAN"] = "MSGR O'CALLAGHAN"
  streetName[streetName=="DE SOTO"] = "DESOTO"
  streetName[streetName=="HENRY STERLING"] = "STERLING"
  streetName[streetName=="PARK VALE"] = "PARKVALE"
  streetName[streetName=="MASS"] = "MASSACHUSETTS"
  streetName[streetName=="BARTLET"] = "BARTLETT"
  streetName[streetName=="CHESBOROUGH"] = "CHESBROUGH"
  streetName[streetName=="SELWIN"] = "SELWYN"
  streetName[streetName=="MARIBOSA"] = "MARIPOSA"
  streetName[streetName=="MCCORMICK"] = "MCCORMACK"
  streetName[streetName=="JULIET"] = "JULIETTE"
  streetName[streetName=="WILLOOWWOOD"] = "WILLOWWOOD"
  streetName[streetName=="ADAM"] = "ADAM"
  streetName[streetName=="MASCHSTS"] = "MASSACHUSETTS"
  streetName[streetName=="COMNWLTH"] = "COMMONWEALTH"
  streetName[streetName=="WM F MCCLELLAN"] = "WILLIAM F MCCLELLAN"
  streetName[streetName=="ROSEBERY"] = "ROSEBERRY"
  streetName[streetName=="MARTIN LUTHER KING JR"] = "MARTIN LUTHER KING"
  streetName[streetName=="CASTLE ROCK"] = "CASTLEROCK"
  streetName[streetName=="MSGR P J LYDON"] = "MSGR PATRICK J LYDON"
  streetName[streetName=="CASTLE ROCK"] = "CASTLEROCK"
  streetName[streetName=="O CONNELL"] = "OCONNELL"
  
  return(streetName)
}

# like the streetname function, cleans and standardizes a vector of suffixes and returns the cleaned vector
clean_suffix <- function(suffix) {
  suffix <- trim(toupper(suffix))
  
  suffix[suffix=="ST S"]<- "ST SOUTH"
  suffix[suffix=="ST EXN"|suffix=="ST EXD"]<- "ST EXTENSION"
  suffix[suffix=="ST W"] <-"ST WEST"
  suffix[suffix=="ST E"] <-"ST EAST"
  suffix[suffix=="ST N"] <-"ST NORTH"
  
  suffix[suffix=="PLAZA"]<- "PLZ"
  suffix[suffix=="PZ"]<- "PLZ"
  suffix[suffix=="ROWE"|suffix=="RO"] <- "ROW"
  suffix[suffix=="ROAD"] <- "RD"
  suffix[suffix=="AV"|suffix=="AVE."] <- "AVE"
  suffix[suffix=="TER"|suffix=="TE"|suffix=="TERRACE"] <- "TERR"
  suffix[suffix=="COURT"] <- "CT"
  suffix[suffix=="PARKWAY"] <- "PKWY"
  suffix[suffix=="PLACE"] <- "PL"
  suffix[suffix=="AVENUE"] <- "AVE"
  suffix[suffix=="STREET"|suffix=="STRET"] <- "ST"
  suffix[suffix=="WY"] <- "WAY"
  suffix[suffix=="LA"] <- "LN"
  suffix[suffix=="CI"] <- "CIR"
  suffix[suffix=="HW"|suffix=="HW."] <- "HWY" 
  suffix[suffix=="PARK"] <- "PK" 
  suffix[suffix=="CIRCUIT"] <- "CIRT"
  suffix[suffix=="CC"] <- "CIRT"
  suffix[suffix=="BL"|suffix=="BOULEVARD"] <- "BLVD"
  suffix[suffix=="PKWY"] <- "PW"
  suffix[suffix==""] <- NA
  return(suffix)
}

# cleans and standardizes a vector of city names - most of these are for cities/neighborhoods within Boston proper
clean_city <- function(city) {
  city = toupper(trim(city))
  city[city== "NULL"] <- NA
  city[city == "ALSTON"] <- "ALLSTON"
  city[city == "BACKBAY"] <- "BACK BAY"
  city[city == "BEACON HIL"] <- "BEACON HILL"
  city[!is.na(match(city,c("BORSOTN", "BOSTON WEST END", "BOSTON-DORCHESTER", "BOSTON02113", "BOSTON02129", "BOTON")))] <- "BOSTON"
  city[!is.na(match(city,c("BRGHTON","BRIGHTJTON","BRIGHJTON","BRIGTON","BRIGRTON","BTIGHTON","BR")))] <- "BRIGHTON"
  city[!is.na(match(city,c("CHRLESTOWN", "CHARLES TOWN","CHARLESTOEN","CHARLESTONW","CHARLESTOWEN","CHARLESTWON",
                           "CHARLSETOWN","CH")))] <- "CHARLESTOWN"
  city[!is.na(match(city,c("CHESNUT HILL")))] <- "CHESTNUT HILL"
  city[city=="LEATHER DISTR."] <- "CHINATOWN"
  city[!is.na(match(city,c("DORCHESER","DORCHESETER","DORCHESTERT", "DOCHESTER","DOR","DORC","DORC HESTER",
                           "DORCEHSTER","DORCESTER","DORCH","DORCHERSTER","DORCHESTE","DORCHESTER MA","DORCHSTERT",
                           "DORCHESTON","DORCHESTOR/BOSTON","DORCHESWTER","DORCHETSER","DORCHSETER","DORCHSTER",
                           "DORHESTER","DORSHESTER","DO")))] <- "DORCHESTER"
  city[!is.na(match(city,c("E BOSTON","E. BOSTON","E.BOSTON","EAST BOST5ON","EAST BOTON","E BSOTN","EAST BSOTN",
                           "EASTBOSTON","EB")))] <- "EAST BOSTON"
  city[!is.na(match(city,c("FENMORE","FENS/KENMORE","FENWAY","KENMORE")))] <- "FENWAY/KENMORE"
  city[!is.na(match(city,c("HYDE  PARK","HYDE PAEK","HYDE PARD","HYDE PARK 02136","HYDEPARK","HYDEPPARK" ,"HYE PARK",
                           "HYPE PARK","HP")))] <- "HYDE PARK"
  city[!is.na(match(city,c("J P","J.P","JAMCIA PLAIN","JAMAIC PLAIN","JAMAICA","JAMAICA BLAIN","JAMAICA PLAINI","JAMAICA PLAN",
                           "JAMAICAPLAIN","JAMAICIA PLAIN","JAMIACA PLAIN","JAMICA PLAIN","JP","JAMACIA PLAIN")))] <- "JAMAICA PLAIN"
  city[!is.na(match(city,c("MATAPABN","MATAPAN","MATTAAPAN","MATTAPN","MATTTAPAN","MT")))] <- "MATTAPAN"
  city[!is.na(match(city,c("N END","NO. END","NORTHEND")))] <- "NORTH END"
  city[!is.na(match(city,c("ROLINDALE","ROSINDALE","ROSLINDANLE","ROSLINADLE","ROSLINDAEL","ROSLINDALE ST","ROSLNDALE",
                           "ROSLIDANLE","RS")))] <- "ROSLINDALE"
  city[!is.na(match(city,c("ROXBURY CROSSING","ROX","ROXBURY02119","ROXBURY02120","TOXBURY")))] <- "ROXBURY"
  city[!is.na(match(city,c("S  BOSTON","S BOSTON","S. BOSTON","SO. BOSTON","SO.BOSTON","SOTH BOSTON","SOUHT BOSTON","SOUTH BOSOTN",
                           "SOUTH BOSTN","SOUTH BOSTOM","SOUTH BPSTOM","SOUTHBOSTON","BOUTH BOSTON","SB")))] <- "SOUTH BOSTON"
  city[!is.na(match(city,c("S END","SE","SO. END","SOUTHEND")))] <- "SOUTH END"
  city[!is.na(match(city,c("W ROXBURY","W  ROXBURY","W. ROX","W. ROXBURY","W.ROXBURY","WEST  ROXBURY","WEST ROXB URY",
                           "WEST ROXBRUY","WEST ROXBRY","WEST RXOBURY","WESTROXBURY","WR")))] <- "WEST ROXBURY"
  city[city == "" | city == "NA" | city == "N/A"] <- NA
  return(city)
}

# cleans and returns a vector of zips, again, crappy regex!
clean_zip <- function(zip) {
  zip = gsub(",","",gsub(",","",as.character(trim(zip))))
  zip[zip=="NULL"|zip=="NA"|zip=="0"]<-NA
  pattern1 = "^([0-9]{4})$"
  match1 = str_match(zip,pattern1)[,2]
  
  pattern2 = "^([0-9]{5})$"
  match2 = str_match(zip,pattern2)[,2]
  
  pattern3 = "^([0-9]{4})-([0-9]{0,4})$"
  match3 = str_match(zip,pattern3)[,2]
  
  pattern4 = "^([0-9]{5})-([0-9]{0,4})$"
  match4 = str_match(zip,pattern4)[,2]
  
  cleaned_zip = zip
  cleaned_zip[!is.na(match1)]=paste("0",match1[!is.na(match1)],sep="")
  cleaned_zip[!is.na(match2)]= match2[!is.na(match2)]
  cleaned_zip[!is.na(match3)]=paste("0",match3[!is.na(match3)],sep="")
  cleaned_zip[!is.na(match4)]= match4[!is.na(match4)]
  
  cleaned_zip = gsub("_","",cleaned_zip)
  return(cleaned_zip)
}


#separates and cleans a street and suffix (used inside the function clean_address, not necessarily useful on its own)
separate_suffix <- function(street) {
  street = trim(toupper(street))
  
  # this is a slightly better regex!
  # if any suffixes are added here, also remember to add them to the suffix cleaner to make sure they are in a standard form, if necessary (e.g. if you add RDE as a misspelling of road, make sure it gets corrected to RD in clean_suffix)
  pattern = "^(.*) (ST N|ST E|ST W|ST CON|ST EXD|ST EXN|ST S|STRET|ST|AVE|PL|AV|CIR|CI|LA|LN|DR|RD|WAY|BLVD|CT|TE|TER|TERR|ROAD|SQ|HWY|WY|PKWY|PW|COURT|PARKWAY|STREET|PLACE|AVENUE|AVE|HW|PK|PARK|CIRCUIT|CIRT|BL|RO|CC|ROW|ROWE|PLAZA|PLZ|PZ)[.]?$|(.*)$"
  match = str_match(street,pattern)
  street_name = trim(match[,2])
  street_name[is.na(street_name)] = match[is.na(street_name),1]
  street_suffix = trim(match[,3])
  street_suffix = clean_suffix(street_suffix)
  street_name = clean_streetName(street_name)
  
  return(matrix(data=c(street,street_name,street_suffix),ncol=3))
}

# separates and cleans an address vector that holds street number, street name, street suffix, possibly a PO box
# calls the other cleaning functions
clean_address <- function(address ){
  
  #removing a weird character that was causing problems, making all characters capital and trimming
  address_raw <- as.character(gsub("\xae","R",address))
  
  address_raw <- toupper(trim(address_raw))
  
  #boston specific
  address_raw[address_raw=="1 SCHROEDER PLZ"|address_raw=="SCHROEDER PLZ"]="1201 TREMONT ST"
  
  #pull out a NSEW SUFFIX if it is there
  nsew_match = str_match(address_raw,"(.*) (E|EAST|W|WEST|S|SOUTH|N|NORTH)$")
  directional = nsew_match[,3]
  directional[directional=="E"] = "EAST"
  directional[directional=="W"] = "WEST"
  directional[directional=="S"] = "SOUTH"
  directional[directional=="N"] = "NORTH"
  address_raw = ifelse(!is.na(nsew_match[,2]),nsew_match[,2],address_raw)
  
  #splitting number and street
  pattern1 = "^([0-9]+[A-Z]?[A-Z]? ?- ?[0-9]*[A-Z]?[A-Z]?|[0-9]+[A-Z]?[A-Z]?) (.*)$"
  match = str_match(address_raw,pattern1)
  number = trim(match[,2])
  street = ifelse(is.na(trim(match[,3])),address_raw,trim(match[,3]))
  
  #for those that don't match the pattern, the original text is put in (I think this is when there was no street number)
  street[is.na(street)]=match[is.na(street),1]
  
  #getting suffix
  separated_suffix = separate_suffix(street)
  street_name = separated_suffix[,2]
  #if no suffix, get the whole
  street_name[is.na(separated_suffix[,2]) | (separated_suffix[,2] == "")] = separated_suffix[is.na(separated_suffix[,2]) | (separated_suffix[,2] == ""),1]
  street_suffix = separated_suffix[,3]
  
  #cleaning number
  cleaned_num = clean_num(number)
  num1 = cleaned_num[,2]
  num2 = cleaned_num[,3]
  
  
  #getting PO boxes
  pattern4 = "([P][O]|[P][.][O][.]) BOX +([0-9]+)"
  match=str_match(address_raw,pattern4)
  po_num = match[,3]
  
  
  returnMatrix = matrix(data = c(address,num1,num2,street_name,street_suffix,po_num,directional),ncol = 7)
  colnames(returnMatrix) <- c("address raw","num1","num2","street name","street suffix","PO num","directional")
  return(returnMatrix)
}

#from a clean state, reduces different names of cities within Boston to common grouping (e.g. Back Bay -> Back Bay/Allston)
reduce_cities <-function(cities) {
  cities = toupper(trim(cities))
  cities[!is.na(match(cities,c("ALLSTON","BRIGHTON")))] <- "ALLSTON/BRIGHTON"
  cities[!is.na(match(cities,c("NORTH DORCHESTER","SOUTH DORCHESTER")))] <- "DORCHESTER"
  cities[!is.na(match(cities,c("WEST ROXBURY","ROXBURY","EAST ROXBURY")))] <- "ROXBURY"
  cities[!is.na(match(cities,c("BACK BAY/BEACON HILL","CENTRAL","SOUTH END","FENWAY/KENMORE")))] <- "BOSTON"
  return(cities)
}

#checks if a zip code was accidentally put into the city variable
# i almost never use this, but at some point it was useful
city_zip <- function(city) {
  city = as.character(city)
  match1 = str_match(city,"^([0-9]{4})$")[,2]
  match2 = str_match(city,"^([0-9]{5})$")[,2]
  match3 = str_match(city,"^([0-9]{4})-([0-9]{4})$")[,2]
  match4 = str_match(city,"^([0-9]{5})-([0-9]{4})$")[,2]
  city_zip = NA
  city_zip[!is.na(match1)]=paste("0",match1[!is.na(match1)],sep="")
  city_zip[!is.na(match2)]= match2[!is.na(match2)]
  city_zip[!is.na(match3)]=paste("0",match3[!is.na(match3)],sep="")
  city_zip[!is.na(match4)]= match4[!is.na(match4)]
  
  return(city_zip)
  #return(cleaned_zip)
  
}



