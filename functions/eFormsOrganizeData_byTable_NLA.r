# eFormsOrganizeData_byTable_NLA.r
# Purpose: For each type of data, organize into data frames
# First figure out the type of data by sample type
#
# Created 6/5/2019 by Karen Blocksom - HAVE NOT STARTED IT YET
###############################################################

eFormsOrganize_byTable <- function(rawData){
  # Extract visit info
  visitinfo <- as.data.frame(rawData[1:7],stringsAsFactors=F)

  # Extract sample type from 8th element in each file
  sampletype <- names(rawData)[8]

  # # PHAB sample types are special
  sampletype <- ifelse(substring(sampletype,1,4) %in% c('PHAB'),'PHAB',sampletype)

  # Create data frame of parsed data to start with, making them all character variables 
  parsedData <- as.data.frame(rawData[8])
  parsedData[,names(parsedData)] <- lapply(parsedData[,names(parsedData)], as.character)
  
  # run parsed data through organizing function, based on sample type 
  switch(sampletype,
    ASSESSMENT = {rr <- organizeAssessment(parsedData)},
    INDEX_SAMPLE = {rr <- organizeIndex(parsedData)},
    PROFILE_CALIBRATION = {rr <- organizeCalibration(parsedData)},
    LITTORAL_SAMPLE = {rr <- organizeLittoral(parsedData)},
    PHAB = {rr <- organizePhab(parsedData)},
    VERIFICATION = {rr = organizeVerification(parsedData)},
    PROFILE_DATA = {rr = organizeProfile(parsedData)}
  )
  
  ss <- list(cbind(visitinfo, rr))
    # Add new object to list with sample type name
  ss[["SAMPLE_TYPE"]] <- sampletype
  return(ss)
}

#############################################################################################################
# This begins the section which organizes the parsed data by sample type

organizeVerification <- function(parsedIn){
# Simply melt these data and clean up parameter names
  aa <- mutate(parsedIn, SAMPLE_TYPE='VERIF') %>%
    melt(id.vars=c('SAMPLE_TYPE'), variable.name='PARAMETER', value.name='RESULT') %>%
    mutate(PARAMETER=gsub('VERIFICATION\\.', '', PARAMETER)) %>%
    select(SAMPLE_TYPE, PARAMETER, RESULT) 
    
  return(aa)
}

organizeIndex <- function(parsedIn){
  # Simply melt these data by SAMPLE_TYPE and clean up parameter names
  aa <- mutate(parsedIn, SAMPLE_TYPE='INDEX_SAMPLE') %>%
    melt(id.vars=c('SAMPLE_TYPE'), value.name='RESULT') %>%
    filter(str_detect(variable,'REVIEW',negate=TRUE)) %>%
    mutate(SAMPLE_TYPE=substring(as.character(variable),14,17), 
           PARAMETER=substring(as.character(variable),19,nchar(as.character(variable)))) %>%
    select(SAMPLE_TYPE, PARAMETER, RESULT)
  
  return(aa)
}

organizeLittoral <- function(parsedIn){
  # Simply melt these data by SAMPLE_TYPE and clean up parameter names
  aa <- subset(parsedIn, select = str_starts(names(parsedIn),'LITTORAL_SAMPLE\\.BENT')) %>%
    mutate(SAMPLE_TYPE='BENT') %>%
    melt(id.vars=c('SAMPLE_TYPE'), value.name='RESULT') %>%
    mutate(PARAMETER=str_replace(variable,'LITTORAL\\_SAMPLE\\.BENT\\_',''),STATION='ALL') %>%
    select(SAMPLE_TYPE, STATION, PARAMETER, RESULT)
  
  bb <- subset(parsedIn, select= str_starts(names(parsedIn),'LITTORAL_SAMPLE\\.[:alpha:]\\_SUBBENT')) %>%
    mutate(SAMPLE_TYPE='SUBBENT') %>%
    melt(id.vars=c('SAMPLE_TYPE'),value.name='RESULT') %>%
    mutate(STATION=substring(variable,17,17),PARAMETER=str_replace(variable,'LITTORAL\\_SAMPLE\\.[:alpha:]\\_SUBBENT\\_','')) %>%
    select(SAMPLE_TYPE,STATION,PARAMETER,RESULT)
  
  cc <- rbind(aa,bb)
  
  return(cc)
}

organizeAssessment <- function(parsedIn){
  
  # Simply melt data and clean up parameter names
  aa <- mutate(parsedIn, SAMPLE_TYPE='ASSESS') %>%
    melt(id.vars=c('SAMPLE_TYPE'), variable.name='PARAMETER', value.name='RESULT') %>%
    mutate(PARAMETER=gsub('ASSESSMENT\\.', '', PARAMETER)) %>%
    select(SAMPLE_TYPE, PARAMETER, RESULT)
  
  return(aa)
  
}

organizeProfile <- function(parsedIn){
 # NEED TO FIND PARAMETERS THAT START WITH CHARACTER VS. NUMBER
  aa <- mutate(parsedIn, SAMPLE_TYPE='PROF') %>%
    subset(select=str_detect(names(parsedIn), 'F1')==FALSE) %>%
    melt(id.vars=c('SAMPLE_TYPE'), value.name='RESULT') %>%
    mutate(variable = str_replace(variable, "PROFILE\\_DATA\\.",""), LINE=str_extract(variable,'[:digit:]+\\_')) %>%
    mutate(LINE=str_replace(LINE,"\\_",""),PARAMETER=str_replace(variable,"[:digit:]+\\_",'')) %>%
    select(SAMPLE_TYPE,LINE,PARAMETER,RESULT)
  
  bb <- subset(parsedIn, select=str_detect(names(parsedIn),'F1')) 
  
  if(ncol(bb) > 0){
    bb <- mutate(bb, SAMPLE_TYPE='PROF') %>%
      melt(id.vars=c('SAMPLE_TYPE'),value.name='COMMENT') %>%
      mutate(variable = str_replace(variable, "PROFILE\\_DATA\\.",""), LINE=str_extract(variable,'[:digit:]+\\_')) %>%
      mutate(LINE=str_replace(LINE,"\\_","")) %>%
      select(SAMPLE_TYPE,LINE,COMMENT)
   
    cc <- merge(aa,bb,by=c('SAMPLE_TYPE','LINE'),all=TRUE) 
  }else{
    cc <- aa
  }
  
  return(cc)
  
}

organizeCalibration <- function(parsedIn){
  # Simply melt data and clean up parameter names
  aa <- mutate(parsedIn, SAMPLE_TYPE='CALIB') %>%
    melt(id.vars=c('SAMPLE_TYPE'), variable.name='PARAMETER', value.name='RESULT') %>%
    mutate(PARAMETER=gsub('PROFILE\\_CALIBRATION\\.', '', PARAMETER),LINE='0') %>%
    select(SAMPLE_TYPE, LINE, PARAMETER, RESULT)
  
  return(aa)  
  
}

organizePhab <- function(parsedIn){
  
  aa <- subset(parsedIn, select=str_detect(names(parsedIn),'COMMENT')==FALSE) %>%
    mutate(SAMPLE_TYPE='PHAB') %>%
    melt(id.vars=c('SAMPLE_TYPE'),value.name='RESULT') %>%
    mutate(STATION=substring(variable,6,6),PARAMETER=str_replace(variable,'PHAB\\_[:alpha:]\\.','')) %>%
    select(SAMPLE_TYPE,STATION,PARAMETER,RESULT)
    
  bb <- subset(parsedIn, select=str_detect(names(parsedIn),'COMMENT')) 
  
  if(ncol(bb) > 0){
    bb <- mutate(bb, SAMPLE_TYPE='PHAB') %>%
      melt(id.vars=c('SAMPLE_TYPE'),value.name='COMMENT') %>%
      mutate(STATION=substring(variable,6,6),PARAMETER=str_replace(variable,'PHAB\\_[:alpha:]\\.','')) %>%
      mutate(PARAMETER=str_replace(PARAMETER,'\\_COMMENT','')) %>%
      select(SAMPLE_TYPE,STATION,PARAMETER,COMMENT)
    
    cc <- merge(aa,bb,by=c('SAMPLE_TYPE','STATION','PARAMETER'),all=T)
  }else{
    cc <- aa
  }
    
  return(cc)
  
}