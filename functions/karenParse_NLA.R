
karenOrganizationShiny <- function(pathlist,filelist){
  finalOut <- list() #vector("list", length(filelist)) # preallocate list length to speed processing
  
  for(i in 1:length(filelist)){
    # fileName <- paste(path,filelist[i],sep='/')
    fileName <- filelist[i]
    # filePath <- filelist[i]
    filePath <- pathlist[i]
    
    print(fileName)
    
    # This step parses data and then organizes data in each file
    if(str_detect(fileName,'SHIPPING|TRACKING',negate=TRUE)){
      fileName <- gsub("[[:alnum:]]+[[:punct:]][[:alpha:]]+[[:punct:]][[:alnum:]]+[[:punct:]][[:alnum:]][[:punct:]]", "", fileName)
      fileName <- gsub('.json*', '', fileName)
      fileName <- gsub('.*/', '', fileName)
      
      rr <- eFormsParseJSON(filePath)
      tt <- eFormsOrganize_byTable(rr)
      
      finalOut[[fileName]] <- tt
      
    # finalOut[[fileName %>% 
    #             str_replace("[:alnum:]+\\_[:alpha:]+\\-[:alnum:]+\\_[:alnum:]\\_|[:alnum:]+\\_[:alnum:]+\\_[:alnum:]+\\_[:alnum:]+\\_",'') %>%
    #             str_replace('.json*|.JSON*','') %>% 
    #             str_replace('.*/','') ]] <- eFormsParseJSON(filePath) %>%
    #   eFormsOrganize_byTable()  
    }
  }
  
    return(finalOut)
}


karenWriteShiny <- function(filelist, finalList){
  # Create the first part of the filename for writing to a .csv file, based on visit info and sample type
  subName.out <- str_extract(filelist[1],"[:alnum:]+\\_[:alpha:]+\\-[:alnum:]+\\_[:alnum:]\\_|[:alnum:]+\\_[:alnum:]+\\_[:alnum:]+\\_[:alnum:]+\\_")
  print(subName.out)

    specialCases <- names(finalList)[substring(names(finalList),1,4)=='PHAB']

    
    for(i in 1:length(specialCases)){
      names(finalList)[names(finalList)==specialCases[i]] <- 'PHAB'
    }
    
    others <- finalList[!(names(finalList)=='PHAB')]
    
    phab_all <- finalList[names(finalList)=='PHAB']
    phab_all <- map_df(phab_all, 'PHAB')
    # phab_all <- finalList[names(finalList)=='PHAB'] %>%
    #   map_df('PHAB')
    
    phab <- list(PHAB=phab_all)
    meta <- list(Metadata = metadata)
    
    return(c(map(others,1),phab,meta))

}


