
#--------------------------------------#
#       Non-BARI Functions             #
#--------------------------------------#

# the projection i've used for lat long coordinates 
latLongProj = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
# planar projection for 
planarProj = "+init=epsg:32619"

# IDK how useful this will be in the future but I used it to make some plots
ggPlotBlankTheme__ =  theme(axis.line=element_blank(),
                            axis.text.x=element_blank(),
                            axis.text.y=element_blank(),
                            axis.ticks=element_blank(),
                            axis.title.x=element_blank(),
                            axis.title.y=element_blank(),
                            legend.position="none",
                            panel.background=element_blank(),
                            panel.border=element_blank(),
                            panel.grid.major=element_blank(),
                            panel.grid.minor=element_blank(),
                            plot.background=element_blank())

# MAXIMIZING MODELS FUNCTIONS

# from my thesis work and a stat class
# finds the best model by comparing stepwise AIC or BIC, possible for multiple types of models
# there are 3 functions, they probably could be easily consolidated into one
# i realized after writing this that there's a canned procedure to do this, but this allows you to modify it as you see fit and might have some extra features
# THE FUNCTIONS
# findStepwiseModels_fast doesn't iterate through all of the variables before choosing the best one, it immediately adds a variable if it improves the AIC
#      it also doesn't allow for multiple outcomes or some of the extra features in findStepwiseModels 
# findStepwiseModels does iterate through all of the possible variables before choosing the one that improves the model the most
# findStepwiseModels_back starts with all of the variables in the model and works backward, like findStepwiseModels it iterates through all possible
#      variables before choosing which one to remove
# VARIABLES:
# dataset: data frame
# outcome: string name of variable
# outcomes: vector of string names of variables
# primaryVars: vector of variable names to be tested, called primary because in some of the functions you can specify a set of secondary vars to test after
# secondaryVars: vector of variable names to be tested after none of the primary vars improve the model, or all primary vars are added
# AICBIC: string equal to either "AIC" or "BIC", the statistic to test for model fit
# modelType: string, "lm", "glm.nb" or "glm.binomial"
# startingVars: vector of variable names to be automatically included
# comments are in findStepwiseModels, other functions are derivative

findStepwiseModels = function(dataset, outcomes, primaryVars, AICBIC,modelType="lm", startingVars=c(),secondaryVars = c() ) {
  
  # algoModels will be the output, a list of the best model for each outcome, in a format that can then be plugged into a model as a string
  algoModels = vector(mode="list", length=length(outcomes))
  names(algoModels) = outcomes
  
  for (outcome in outcomes) {
    # bestBIC will hold the current best AIC or BIC, after each iteration through available variables, we will see if the bestBIC
    # is better than the oldBIC, and if not the model will be maximized, if it is better, then the associated variable will be adeed
    bestBIC = Inf
    varsInModel = c(startingVars)
    # variable "is" decides whether the whole process gets run through a second time with secondary vars
    if (length(secondaryVars)!=0) { is = 2}
    else {is = 1}
    for (i in c(1:is)) {
      # this will become true when the bestBIC is not an improvement
      modelMaximized = F
      # on the first run we maximize with the primary vars, then if they are there we maximize with the secondary vars
      if (i == 1) {vars = primaryVars}
      else if (i == 2) {vars = secondaryVars}
      while (!modelMaximized) {
        oldBIC = bestBIC
        # bestVar will get set at the same time as bestBIC, and it will reflect the var that is associated with that bestBIC
        bestVar = NA
        # iterates through each possible var before choosing one
        for (var in vars) {
          # skips a var if it has already been added
          if (!var %in% varsInModel) {
            # fits a new model with the var included
            if (modelType == "lm") {
              model = lm(paste(outcome,paste(c(varsInModel,var),collapse = "+"),sep="~"),
                         data =dataset)
            } else if (modelType == "glm.nb") {
              model = glm.nb(paste(outcome,paste(c(varsInModel,var),collapse = "+"),sep="~"),
                             data =dataset)            
            }else if (modelType == "glm.binomial") {
              model = glm(paste(outcome,paste(c(varsInModel,var),collapse = "+"),sep="~"),
                          data =dataset,family=binomial(link='logit'))            
            }
            
            # sets the newBIC
            if (AICBIC == "BIC") {
              newBIC = BIC(model)
            } else {
              newBIC = AIC(model)
            }
            
            # compares the newBIC to the bestBIC and if it's an improvement, sets the bestBIC and bestVar
            if (newBIC < bestBIC) {
              bestBIC = newBIC
              bestVar = var
            }
          }
        }
        
        # after all vars have been tested, it checks if there is an improvement, and if not ends the loop, and if so adds the associated var
        if (oldBIC == bestBIC) {
          modelMaximized = T
        } else {
          print(paste("New var: ",bestVar,sep=""))
          varsInModel = c(varsInModel,bestVar)
        }    
      }   
    }
    # for each outcome, writes the best model
    algoModels[[outcome]] = paste(varsInModel,collapse=" + ")
  }
  
  return(algoModels)
}  




findStepwiseModels_fast = function(dataset, outcome, primaryVars, AICBIC,modelType="lm", startingVars=c() ) {
  
  varsInModel = c(startingVars)
  bestBIC = Inf
  oldBIC = bestBIC
  modelMaxed=F
  
  while (!modelMaxed) {
    for (var in primaryVars) {
      if (!var %in% varsInModel) {
        if (modelType == "lm") {
          model = lm(paste(outcome,paste(c(varsInModel,var),collapse = "+"),sep="~"),
                     data =dataset)
        } else if (modelType == "glm.nb") {
          model = glm.nb(paste(outcome,paste(c(varsInModel,var),collapse = "+"),sep="~"),
                         data =dataset)            
        }else if (modelType == "glm.binomial") {
          model = glm(paste(outcome,paste(c(varsInModel,var),collapse = "+"),sep="~"),
                      data =dataset,family=binomial(link='logit'))            
        }
        if (AICBIC == "BIC") {
          newBIC = BIC(model)
        } else {
          newBIC = AIC(model)
        }
        if (newBIC < bestBIC) {
          bestBIC = newBIC
          print(paste("New var: ",var,sep=""))
          varsInModel = c(varsInModel,var)
        }
      }
    }
    if (oldBIC == bestBIC) {
      modelMaxed=T
    }
  }
  return(paste(varsInModel,collapse=" + "))
} 



findStepwiseModelsBACK = function(dataset, outcomes, vars, AICBIC, modelType="lm"  ) {
  
  algoModels = vector(mode="list", length=length(outcomes))
  names(algoModels) = outcomes
  for (outcome in outcomes) {
    bestBIC = Inf
    varsInModel = c(vars)
    modelMaximized = F
    while (!modelMaximized) {
      oldBIC = bestBIC
      dropVar = NA
      for (var in c("DUMMY",varsInModel)) {
        if (modelType == "lm") {
          model = lm(paste(outcome,paste(setdiff(c(varsInModel,1),var),collapse = "+"),sep="~"),
                     data =dataset)
        } else if (modelType == "glm.nb") {
          model = glm.nb(paste(outcome,paste(setdiff(c(varsInModel,1),var),collapse = "+"),sep="~"),
                         data =dataset)            
        } else if (modelType == "glm.binomial") {
          model = glm(paste(outcome,paste(setdiff(c(varsInModel,1),var),collapse = "+"),sep="~"),
                      data =dataset,family=binomial(link='logit'))  
        }
        if (AICBIC == "BIC") {
          newBIC = BIC(model)
        } else {
          newBIC = AIC(model)
        } 
          if (newBIC < bestBIC) {
            bestBIC = newBIC
            dropVar = var
          }
        }
        if (oldBIC == bestBIC) {
          modelMaximized = T
        } else {
          varsInModel = setdiff(varsInModel,dropVar)
        }    
      }   
      algoModels[[outcome]] = paste(varsInModel,collapse=" + ")
    }
    return(algoModels)
  }  


# TRACT BOUNDARY CONVERSION

# from my thesis work
# this converts tract data that uses 2010 boundaries to 2000 boundaries for Boston
# although it could be used for other areas covered by the same crosswalk file
# crosswalk file came from Alex Ciomek, and originally from https://s4.ad.brown.edu/Projects/Diversity/Researcher/LTBDDload/DataList.aspx
# there is also Stata code you can use on the website to do this. I'm not positive why I decided to write it myself
# i'm not sure if the stata code allows the "scalingMin"

convertTractData = function(oldData,tractID,scalingMin = .3, city = "Boston",
                            crosswalkPath = "/Users/henrygomory/Documents/School/Thesis/BNS/Data files/crosswalk_2000_2010.csv",
                            bostonTracts2000Path = "/Users/henrygomory/Documents/School/Thesis/BNS/Data files/BNS 2010 for Analysis.csv",
                            chicagoTracts2000Path = "/Users/henrygomory/Documents/School/Thesis/BNS/Data files/Chicago/chicago_tracts_2000.csv") {
  
  crosswalk0010 = read.csv(crosswalkPath,stringsAsFactors=F)
  
  # reduces the crosswalk file to just those in Boston or chicago, watch out for the column names on the files that are read in
  # almost definitely a better way to do this by using the ID names in the oldData 
  # probably just crosswalk_lim = crosswalk0010[ crosswalk0010$trtid10 %in% oldData[,tractID],]
  # in this new way, rows would be dropped, even if they have a 2000 id, for which we don't have the 2010 data, 
  # but i don't think there's any reason that it would be useful to have those rows anyway
  if (city =="Boston") {
    bostonTracts2000 = read.csv(bostonTracts2000Path,stringsAsFactors=F)
    crosswalk_lim = crosswalk0010[ crosswalk0010$trtid00 %in% c(bostonTracts2000$CT_ID,25025010102),]
  }
  else if (city == "Chicago") {
    chicagoTracts2000 = read.csv(chicagoTracts2000Path,stringsAsFactors=F)$x
    crosswalk_lim = crosswalk0010[ crosswalk0010$trtid00 %in% chicagoTracts2000,]
  } else {
    return("NO CITY")
  }
  
  # this will hold the transformed data
  newData = data.frame(matrix(nrow = length(unique(crosswalk_lim$trtid00)),ncol = ncol(oldData)))
  names(newData) = names(oldData)
  oldData = oldData[!is.na(oldData[,tractID]),]
  newData$realCT_ID = unique(crosswalk_lim$trtid00) 
  
  # last_ct00 is the name of the 2000 ct_id from the last row, to see whether it is finished
  # each row in the crosswalk file is a pair of 2000 and 2010 ids, and a value of how much of the 2000 geography is covered by the 2010 geography
  # when we get to a new 2000 id, then we know the last one is finished, and if we got only 60% of it covered by a 2010 geography (in the case we are missing data, perhaps)
  # then we scale it up
  last_ct00 = NA
  temp = rep(0,ncol(oldData))
  scaling = rep(0,ncol(oldData))
  
  # iterates through each row of the crosswalk_lim
  # there is a way to do this more simply, using matrix multiplication, but i ran into some errors when programming it and this way didn't run slowly at all
  # plus this allowed for checks to be done that might have been more complicated if doing matrix algebra
  for (i in c(1:nrow(crosswalk_lim))) {
    
    this_ct00 = crosswalk_lim$trtid00[i]
    
    # this means that the last ct is done, and must be written to the new file
    if (this_ct00 != last_ct00 & i > 1) {
      
      # scaling is the percent of the 2000 geography that was accounted for. typically this is 99.9%, but in some cases we only got a portion
      # and then it must be scaled up. however, the scalingMin value creates a threshold, for example 30% or more of the tract must be covered
      newData[ newData$realCT_ID == last_ct00,-ncol(newData)] = ifelse(scaling > scalingMin,temp/scaling,NA)
      temp = rep(0,ncol(oldData))
      scaling = 0
    }
    
    # if there are rows in the 2010 data that correspond to the 2010 id in this row of the crosswalk, then that 2010 data will be added in
    if (sum(oldData[,tractID]==crosswalk_lim$trtid10[i],na.rm=T) == 1) {
      # this adds together any values from past rows in the crosswalk (meaning other 2010 geographies) to the new one, scaling by the 
      # weight column in the crosswalk, which tells the percentage of the 2000 geography that is covered by the 2010 geography
      # i'm not sure why i do a rowSums(cbind()) but it seemed the easiest way to work, couldn't I just do a pairwise sum? I can't remember
      temp = rowSums(cbind(temp,
                           as.numeric(oldData[ oldData[,tractID]==crosswalk_lim$trtid10[i],]) *  as.numeric(crosswalk_lim$weight[i])),
                     na.rm=T)
      scaling = scaling + as.numeric(crosswalk_lim$weight[i])*!is.na(oldData[ oldData[,tractID]==crosswalk_lim$trtid10[i],])
    }
    last_ct00 = this_ct00
  }
  
  # adds data for the last one - i think i forgot to add the ifelse for scaling > scalingMin to this one
  newData[ newData$realCT_ID == last_ct00,-ncol(newData)] = temp/scaling
  
  # orders the column for realCT_ID to be first
  newData = newData[,c("realCT_ID",setdiff(names(newData),"realCT_ID"))]
  newData[,tractID] = NULL
  newData = newData[!is.na(newData$realCT_ID),]
  return(newData)
}

  
  