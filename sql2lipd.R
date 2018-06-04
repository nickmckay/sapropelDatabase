#turn sql structure into LiPD files.
library(here)
library(tidyverse)
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
    if(is.null)
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
  dataSetName <-  stringr::str_remove_all(stringr::str_c(geo$siteName,firstAuthor,pub[[p]]$year,sep = "."),pattern = " ")
  
  ##PaleoData
  
  pmt <- vector(mode = "list",length = 1)
  
  #try to build table
  mt <- dcast(thisObs, DepthStart ~ MeasurementID,value.var = "Value")
  de <- dcast(thisObs, DepthEnd ~ MeasurementID,value.var = "Value")
  
  mt$depthEnd <- de$depthEnd
  
  #get Id table
  mids <- unique(thisObs$MeasurementID)
  this.meas <- filter(measurement,MeasurementID == mids)  
  
#loop through columns
  for(col in 1:nrow(this.meas)){
    tc <- list()#create an empty list
    #COLUMN META
    tc$variableName <- this.meas$Name[col]
    tc$units <- this.meas$Units[col]
    tc$description <- this.meas$Description[col]
    tc$variableType <- "measured"
    tc$proxyObservationType <- this.meas$Class[col]
    tc$proxyObservationTypeDetail <- this.meas$SubClass[col]
    tc$measurementMaterial <- this.meas$Material[col]
    tc$measurementMethod <- this.meas$Method[col]
    
    #add in the data
    tc$values <- as.matrix(mt[as.character(this.meas$MeasurementID[col])])
    
    
    if(!all(is.na(tc$values))){      #plop into the measuermentTable
      pmt[[1]][[tc$variableName]] <- tc
    }
    
  }
  
  
  #repeat for uncertainty...
  der <- dcast(thisObs, DepthEnd ~ MeasurementID,value.var = "_DeltaError")
  der$depthEnd <- de$depthEnd
  
  #loop through columns
  for(col in 1:nrow(this.meas)){
    tc <- list()#create an empty list
    #COLUMN META
    tc$variableName <- str_c(this.meas$Name[col],"Uncertainty")
    tc$units <- "unitless"
    tc$description <- str_c("Uncertainty on ",this.meas$Name[col])
    tc$variableType <- "sampleMetadata"
    
    #add in the data
    tc$values <- as.matrix(der[as.character(this.meas$MeasurementID[col])])
    
    
    if(!all(is.na(tc$values) | tc$values == 0)){      #plop into the measuermentTable
      pmt[[1]][[tc$variableName]] <- tc
    }
    
  }
  
  #repeat for notes...
  notes <- dcast(thisObs, DepthEnd ~ MeasurementID,value.var = "_Notes")
  notes$depthEnd <- de$depthEnd
  
  #loop through columns
  for(col in 1:nrow(this.meas)){
    tc <- list()#create an empty list
    #COLUMN META
    tc$variableName <- str_c(this.meas$Name[col],"Notes")
    tc$units <- "unitless"
    tc$description <- str_c("Notes for ",this.meas$Name[col])
    tc$variableType <- "sampleMetadata"
    
    #add in the data
    tc$values <- as.matrix(notes[as.character(this.meas$MeasurementID[col])])
    
    
    if(!all(is.na(tc$values) | tc$values == 0)){      #plop into the measuermentTable
      pmt[[1]][[tc$variableName]] <- tc
    }
    
  }
  
  ##end paleo data
  
  
  
  
  
  
}






###SISAL EXAMPLE BELOW HERE dont run it and expect it to work.

#load in name converter
nc <- read_csv(here("sisal2lipd_names.csv")) %>% 
  filter(!is.na(lipd))

nc.chron <- read_csv(here("sisal2lipd_names_chron.csv")) %>% 
  filter(!is.na(lipd))

nc.chron.lam <- read_csv(here("sisal2lipd_names_chron_lam.csv")) %>% 
  filter(!is.na(lipd))
#assign in data.frames

#loop through sites
for(s in 19:nrow(sites)){
  

  #PALEODATA
  pmt <- vector(mode = "list",length = nrow(this.ent))
  for(e in 1:nrow(this.ent)){#create a measurement table for every entry
    pmt[[e]]$tableName <- this.ent$entity_name[e]
    pmt[[e]]$SISALEntityID <- this.ent$entity_id[e]
    pmt[[e]]$hasPublication <- this.elr$thisRefId[e]
    
    #find samples that belong to this entity
    this.samp <- filter(sample,entity_id == this.ent$entity_id[e])
    
    #create a complete data.frame for this entity
    adf <- left_join(this.samp,d18O,by="sample_id") %>% 
      left_join(d13C,by="sample_id") %>% 
      left_join(origDates,by="sample_id") %>% 
      left_join(all$gap,by="sample_id") %>% 
      left_join(all$hiatus,by="sample_id")
    
    
    for(n in 1:nrow(nc)){#create a column for each row
      
      tc <- list()#create an empty list
      #COLUMN META
      tc$variableName <- nc$lipd[n]
      tc$units <- nc$units[n]
      tc$description <- nc$description[n]
      tc$variableType <- nc$variableType[n]
      if(!is.na(nc$proxyObservationType[n])){
        tc$proxyObservationType <- nc$proxyObservationType[n]
      }
      if(!is.na(nc$inferredVariableType[n])){
        tc$inferredVariableType <- nc$inferredVariableType[n]
      }
      
      #add in the data
      tc$values <- as.matrix(adf[nc$sisal[n]])
      
      if(!all(is.na(tc$values))){      #plop into the measuermentTable
        pmt[[e]][[tc$variableName]] <- tc
      }
    }#end column loop
    
  }#end measurementTable loop
  
  #CHRONDATA
  cmt <- vector(mode = "list",length = nrow(this.ent))
  hasChron <- c()
  
  for(e in 1:nrow(this.ent)){#create a measurement table for every entry
    cmt[[e]]$tableName <- this.ent$entity_name[e]
    cmt[[e]]$SISALEntityID <- this.ent$entity_id[e]
    cmt[[e]]$hasPublication <- this.elr$thisRefId[e]
    
    #find dates that belong to this entity
    this.dates <- filter(dating,entity_id == this.ent$entity_id[e])
    
    #find dates that belong to this entity
    this.datingLamina <- filter(datingLamina,entity_id == this.ent$entity_id[e])
    hasChron[e] <- TRUE
    
    
    if(nrow(this.dates)>0){#tie points
      for(n in 1:nrow(nc.chron)){#create a column for each row
        
        tc <- list()#create an empty list
        #COLUMN META
        tc$variableName <- nc.chron$lipd[n]
        tc$units <- nc.chron$units[n]
        tc$description <- nc.chron$description[n]
        tc$variableType <- nc.chron$variableType[n]
        if(!is.na(nc.chron$proxyObservationType[n])){
          tc$proxyObservationType <- nc.chron$proxyObservationType[n]
        }
        if(!is.na(nc$inferredVariableType[n])){
          tc$inferredVariableType <- nc.chron$inferredVariableType[n]
        }
        
        #add in the data
        col.name <- nc.chron$sisal[n]
        if(grepl("[0-9]",substr(col.name,1,1))){#starts with a number
          #append an X
          col.name <- str_c("X",col.name)
        }
        tc$values <- as.matrix(this.dates[col.name])
        tc$values <- str_remove_all(tc$values,'"')
        tc$values <- str_remove_all(tc$values,"'")
        tc$values <- str_remove_all(tc$values,",")
        
        if(!all(is.na(tc$values))){ #plop into the measuermentTable
          cmt[[e]][[tc$variableName]] <- tc
          tableRows <- length( tc$values)
        }
      }#end column loop
      
      #add an agetype column
      tc <- list()#create an empty list
      #COLUMN META
      tc$variableName <- "ageType"
      tc$units <- "unitless"
      tc$description <- "broad class of age control"
      tc$variableType <- "sampleMetadata"
      
      #add in the data
      tc$values <- matrix("U/Th",nrow =tableRows)
      cmt[[e]]$ageType <- tc
      
    }#end tiepoints
    if(nrow(this.datingLamina)>0){#lamina
      c.lam <- list()
      for(n in 1:nrow(nc.chron.lam)){#create a column for each row
        tc <- list()#create an empty list
        #COLUMN META
        tc$variableName <- nc.chron.lam$lipd[n]
        tc$units <- nc.chron.lam$units[n]
        tc$description <- nc.chron.lam$description[n]
        tc$variableType <- nc.chron.lam$variableType[n]
        if(!is.na(nc.chron.lam$proxyObservationType[n])){
          tc$proxyObservationType <- nc.chron.lam$proxyObservationType[n]
        }
        if(!is.na(nc.chron.lam$inferredVariableType[n])){
          tc$inferredVariableType <- nc.chron.lam$inferredVariableType[n]
        }
        
        #add in the data
        col.name <- nc.chron.lam$sisal[n]
        if(grepl("[0-9]",substr(col.name,1,1))){#starts with a number
          #append an X
          col.name <- str_c("X",col.name)
        }
        tc$values <- as.matrix(this.datingLamina[col.name])
        tc$values <- str_remove_all(tc$values,'"')
        tc$values <- str_remove_all(tc$values,"'")
        tc$values <- str_remove_all(tc$values,",")
        
        if(!all(is.na(tc$values))){ #plop into the measuermentTable
          cmt[[e]][[tc$variableName]] <- tc
          tableRows <- length( tc$values)
        }
      }#end column loop
      
      #add an agetype column
      tc <- list()#create an empty list
      #COLUMN META
      tc$variableName <- "ageType"
      tc$units <- "unitless"
      tc$description <- "broad class of age control"
      tc$variableType <- "sampleMetadata"
      
      #add in the data
      tc$values <- matrix("layerCount",nrow =tableRows)
      c.lam$ageType <- tc
      
    }#end lamina
    
    #combine as needed
    if(nrow(this.dates)>0 & nrow(this.datingLamina)>0){
      tieLength <- length(cmt[[e]]$age$values)
      lamLength <- length(c.lam$age$values)
      tieNames <- c(nc.chron$lipd,"ageType")
      lamNames <- c(nc.chron.lam$lipd,"ageType")
      
      
      for(n in 1:length(tieNames)){#loop through tiepoints and append...
        this.col <- cmt[[e]][[tieNames[n]]]
        if(any(this.col$variableName %in% lamNames)){#append lams
          print(paste("appending",this.col$variableName))
          this.col$values <- c(this.col$values,c.lam[[this.col$variableName]]$values)
        }else{#append NAs
          this.col$values <- c(this.col$values,matrix(NA,nrow=lamLength))
        }
        cmt[[e]][[tieNames[n]]] <- this.col
      }
      for(n in 1:length(lamNames)){#loop through tiepoints and append...
        this.col <- c.lam[[lamNames[n]]]
        if(any(this.col$variableName %in% tieNames)){#append lams
          #do nothing
        }else{#prepend NAs
          this.col$values <- c(matrix(NA,nrow=tieLength),this.col$values)
          cmt[[e]][[lamNames[n]]] <- this.col
        }
      }
    }else if(nrow(this.datingLamina)>0){
      cmt[[e]] <- c.lam
    }
    
    #
    if(nrow(this.dates)==0 & nrow(this.datingLamina)==0){
      hasChron[e] <- FALSE
    }   
  }#end measurementTable loop
  
  
  #build into a lipd file
  L <- list()
  L$dataSetName <- dataSetName
  L$lipdVersion <- 1.3
  L$geo <- geo
  L$pub <- pub
  L$paleoData <- vector(mode = "list",length = 1)
  L$paleoData[[1]]$measurementTable <- pmt
  if(any(hasChron)){
    L$chronData <- vector(mode = "list",length = 1)
    L$chronData[[1]]$measurementTable <- cmt
  }
  
  writeLipd(L,path = here("lipds"),ignore.warnings = TRUE)
  
}
