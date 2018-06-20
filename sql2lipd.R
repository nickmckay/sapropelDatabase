#turn sql structure into LiPD files.
library(here)
library(tidyverse)
library(reshape2)
library(magrittr)
library(lipdR)
load(here("allTables.RData"))

#assign individual data.frames
area <- all$AREA
core <- all$CORE
measurement <- all$MEASUREMENT #effectively columns
mea_core <- all$MEA_CORE #appears empty
mea_sample <- all$MEA_SAMPLE #data within each column
paper <- all$PAPER #pub information
project <- all$PROJECT #collection meta
region <- all$REGION #Region names...
sapropel <- all$SAPROPEL #Sapropel identification by depth.. Should be worked into measurement tables?

#change the name of the age column...
measurement$Name[which(measurement$Name=="Cal yrs")] <- "age"

# #load in name converter
# nc <- read_csv(here("sisal2lipd_names.csv")) %>% 
#   filter(!is.na(lipd))
# 
# nc.chron <- read_csv(here("sisal2lipd_names_chron.csv")) %>% 
#   filter(!is.na(lipd))
# 
# nc.chron.lam <- read_csv(here("sisal2lipd_names_chron_lam.csv")) %>% 
#   filter(!is.na(lipd))
# #assign in data.frames


#Cores will correspond to lipd files.
for(s in 1:nrow(core)){
  #GEO
  geo <- list()
  geo$siteName <- core$Name[s]
  geo$latitude <- core$Latitude[s]
  geo$longitude <- core$Longitude[s]
  geo$elevation <- core$Depth[s]
  geo$region <- core$RegionName[s]
  geo$project <- core$ProjectName[s]
  
  
  #restrict to this core
  thisObs <- filter(mea_sample,CoreID == core$CoreID[s])
  
  if(nrow(thisObs)==0){
    next 
  }
  
  #PUBS 
  #loop through associated publications
  which.pubs <- which(paper$PaperID %in% thisObs$PaperID)
  
  pub <- vector(mode = "list",length = length(which.pubs))
  for(p in which.pubs){
    
    wp <- which(p %in% which.pubs)
    
    #PUB
    pub[[wp]]$author <- str_replace_all(paper$Citation[p],pattern = ",",replacement = " and ") %>% 
      str_replace_all(pattern = "  ",replacement = " ") %>%  
      str_replace_all(pattern = '"',replacement = "") 
    
    try(pub[[wp]]$year <- lubridate::year(paper$Year[p]))
    pub[[wp]]$title <- paper$Title[p]
    pub[[wp]]$journal <- paper$Journal[p]
    pub[[wp]]$issue <- paper$Issues[p]
    pub[[wp]]$volume <- paper$Volume[p]
    pub[[wp]]$pages <- paper$Pages[p]
    
    pub[[wp]]$DOI <- str_remove(paper$Doi[p],pattern ="^(.+?)(?=10.)")
    
    if(wp==1){
      firstAuthor <-  str_remove(paper$Author[p],pattern ="^(.+)(?= )") %>% 
        str_remove(pattern = " ")
    }
    #Connect publication to observations
    
  }
  
  ##BASE
  dataSetName <-  stringr::str_remove_all(stringr::str_c(geo$siteName,firstAuthor,pub[[1]]$year,sep = "."),pattern = " ") %>% 
    str_remove_all("/")
  
  ##PaleoData
  
  pmt <- vector(mode = "list",length = 1)
  
  thisObs <- mutate(thisObs, DepthMid = (DepthStart+DepthEnd)/2)
  #try to build table
  mt <- dcast(thisObs, DepthMid ~ MeasurementID,value.var = "Value",fun.aggregate = mean)
  dsC <- dcast(thisObs, DepthMid ~ MeasurementID,value.var = "DepthStart",fun.aggregate = mean)
  deC <- dcast(thisObs, DepthMid ~ MeasurementID,value.var = "DepthEnd",fun.aggregate = mean)
  
  if(ncol(dsC)>2){
  mt$DepthStart <- rowMeans(dsC[,-1],na.rm=T)
  mt$DepthEnd <- rowMeans(deC[,-1],na.rm=T)
  }else{
    mt$DepthStart <- dsC[,2]
    mt$DepthEnd <- deC[,2]
  }


  #get Id table
  mids <- unique(thisObs$MeasurementID)
  this.meas <- filter(measurement,MeasurementID %in% mids)  
  
  #assign in depthTop
  tc <- list()
  tc$variableName <- "depthTop"
  tc$units <- "mbsf"
  tc$description <- "top depth"
  tc$variableType <- "measured"
  tc$proxyObservationType <- "depth"

  #add in the data
  tc$values <- as.matrix(mt$DepthStart)
  
  if(!all(is.na(tc$values))){      #plop into the measuermentTable
    pmt[[1]][[tc$variableName]] <- tc
  }
  
  #assign in depthMid
  tc <- list()
  tc$variableName <- "depth"
  tc$units <- "mbsf"
  tc$description <- "middle depth, the average of top and bottom depth"
  tc$variableType <- "measured"
  tc$proxyObservationType <- "depth"
  
  #add in the data
  tc$values <- as.matrix(mt$DepthMid)
  if(!all(is.na(tc$values))){      #plop into the measuermentTable
    pmt[[1]][[tc$variableName]] <- tc
  }
  
  
  #assign in depthBottom
  tc <- list()
  tc$variableName <- "depthBottom"
  tc$units <- "mbsf"
  tc$description <- "bottom depth"
  tc$variableType <- "measured"
  tc$proxyObservationType <- "depth"
  
  #add in the data
  tc$values <- as.matrix(mt$DepthEnd)
  if(!all(is.na(tc$values))){      #plop into the measuermentTable
    pmt[[1]][[tc$variableName]] <- tc
  }
  
  
  #create a sapropel label column
  tc <- list()
  tc$variableName <- "sapropelName"
  tc$units <- "unitless"
  tc$description <- "Identification of known Sapropel unit"
  tc$variableType <- "inferred"
  tc$inferredVariableType <- "stratigraphy"
  
  thisSap <- filter(sapropel, CoreID == core$CoreID[s])
  #find depths that correspond
  sapDepths <- which(thisSap$DepthStart <= pmt[[1]]$depthTop$values & thisSap$DepthEnd >= pmt[[1]]$depthBottom$values)
  sap <- matrix(NA, nrow = length(pmt[[1]]$depthBottom$values))
  sap[sapDepths] <- thisSap$SapropelName
  
  #add in the data
  tc$values <- sap
  if(!all(is.na(tc$values))){      #plop into the measuermentTable
    pmt[[1]][[tc$variableName]] <- tc
  }
  
#loop through columns
  for(col in 1:nrow(this.meas)){
    tc <- list()#create an empty list
    #COLUMN META
    tc$variableName <- this.meas$Name[col] %>% 
      str_replace_all(pattern = "[?]",replacement = "d") %>% 
      str_replace_all(pattern = " ",replacement = "_")
      
    if(tc$variableName=="age"){
      print(paste(dataSetName,"has age"))
    }
    
    tc$units <- this.meas$Units[col]
    tc$description <- this.meas$Description[col]
    tc$variableType <- "measured"
    if(this.meas$Class[col]!=""){
      tc$proxyObservationType <- this.meas$Class[col]
    }
    if(this.meas$SubClass[col]!=""){
    tc$proxyObservationTypeDetail <- this.meas$SubClass[col]
    }
    if(this.meas$Material[col]!=""){
    tc$measurementMaterial <- this.meas$Material[col]
    }
    if(this.meas$Method[col]!=""){
    tc$measurementMethod <- this.meas$Method[col]
    }
    
    #add in the data
    tc$values <- as.matrix(mt[as.character(this.meas$MeasurementID[col])])
    
    
    if(!all(is.na(tc$values))){      #plop into the measuermentTable
      pmt[[1]][[tc$variableName]] <- tc
    }
    
  }
  
  
  #repeat for uncertainty...
  der <- dcast(thisObs, DepthMid ~ MeasurementID,value.var = "DeltaError",fun.aggregate = mean) %>% 
    left_join(mt,der,by = "DepthMid")
  
  
  #loop through columns
  for(col in 1:nrow(this.meas)){
    tc <- list()#create an empty list
    #COLUMN META
    tc$variableName <- str_c(this.meas$Name[col],"_Uncertainty") %>% 
      str_replace_all(pattern = "[?]",replacement = "d") %>% 
      str_replace_all(pattern = " ",replacement = "_")
    
    
    tc$units <- "unitless"
    tc$description <- str_c("Uncertainty on ",this.meas$Name[col])
    tc$variableType <- "sampleMetadata"
    
    #add in the data
    tc$values <- as.matrix(der[str_c(as.character(this.meas$MeasurementID[col]),".y")])
    
    
    if(!all(is.na(tc$values) | tc$values == 0)){      #plop into the measuermentTable
      pmt[[1]][[tc$variableName]] <- tc
    }
    
  }
  
  #repeat for notes...
  notes <- dcast(thisObs, DepthMid ~ MeasurementID,value.var = "Notes",fun.aggregate = mean) %>% left_join(mt,notes,by = "DepthMid")

  
  #loop through columns
  for(col in 1:nrow(this.meas)){
    tc <- list()#create an empty list
    #COLUMN META
    tc$variableName <- str_c(this.meas$Name[col],"_Notes") %>% 
      str_replace_all(pattern = "[?]",replacement = "d") %>% 
      str_replace_all(pattern = " ",replacement = "_")
    tc$units <- "unitless"
    tc$description <- str_c("Notes for ",this.meas$Name[col])
    tc$variableType <- "sampleMetadata"
    
    #add in the data
    tc$values <- as.matrix(notes[str_c(as.character(this.meas$MeasurementID[col]),".y")])
    
    
    if(!all(is.na(tc$values) | tc$values == 0)){      #plop into the measuermentTable
      pmt[[1]][[tc$variableName]] <- tc
    }
    
  }
  
  pmt[[1]]$missingValue <- "NA"
  
  ##end paleo data
  
  
  
  
  
  










  #build into a lipd file
  L <- list()
  L$dataSetName <- dataSetName
  L$archiveType <- "marine sediment"
  L$createdBy <- "lipdR"
  L$lipdVersion <- 1.3
  L$geo <- geo
  L$pub <- pub
  L$paleoData <- vector(mode = "list",length = 1)
  L$paleoData[[1]]$measurementTable <- pmt


  writeLipd(L,path = here("lipds"),ignore.warnings = TRUE)
  
}
