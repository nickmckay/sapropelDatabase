#call libraries
library(tidyverse)
library(devtools)
install_github("nickmckay/lipd-utilities",subdir = "R")
install_github("nickmckay/geoChronR")
library(geoChronR)
library(lipdR)
library(here)

#load in all the LiPD files
D <- readLipd(here("lipds"))

#create a timeseries object
TS <- extractTs(D) 

#lump sapropel names
TS <- lumpTsMetaVariables(TS, sc = "sapropelName")

#get only those that have age 
TS.age <- TS[!sapply(sapply(TS,"[[","age"),is.null)]

###EXAMPLE 1, produce plots of all d18O data, and sapropel identification

#test
test <- sapply(TS,"[[","sapropelName")
which(!sapply(test,is.null))

#pull out a list of unique variableNames
allVariableNames <- unique(sapply(TS, "[[", "paleoData_variableName"))

#filter the TS to only include d18O
TS.d18O <- filterTs(TS, "paleoData_variableName == d18O")

#figure out which have sapropelName fields
test <- sapply(TS.d18O,"[[","sapropelName")
hasSap <- which(!sapply(test,is.null))

TS.d18O.hasSap <- TS.d18O[hasSap]

library(ggplot2)


for(i in 1:length(TS.d18O.hasSap)){
tts <- TS.d18O.hasSap[[i]]

to.plot <- data.frame(depth = tts$depth,value = tts$paleoData_values,sap = tts$sapropelName) %>% 
  filter(!is.na(value))

if(is.factor(to.plot$value)){
  to.plot$value <- as.numeric(to.plot$value)
}


sapOnly <- filter(to.plot,sap == "S1")
sapStart <- min(sapOnly$depth)
sapEnd <- max(sapOnly$depth)



plot <- ggplot(to.plot)+
  geom_rect(aes(xmin = sapStart,xmax = sapEnd,ymin = min(to.plot$value),ymax = max(to.plot$value)), fill = "green", alpha = 0.01)+
  geom_line(aes(x = depth,y = value)) +
  ylab(tts$paleoData_variableName) +
  ggtitle(tts$dataSetName)

ggsave(plot, filename = here("plots",str_c(tts$dataSetName,".pdf")))


}

###END EXAMPLE 1####



###EXAMPLE 2### create proportional, within sapropel depth scale
test <- sapply(TS,"[[","sapropelName")
hasSap <- which(!sapply(test,is.null))

for(i in 1:length(hasSap)){
tts <- TS[[hasSap[i]]]#get one entry

#find the min and max sapropel depth
sdf <- data.frame(depth = tts$depth, sap = tts$sapropelName) %>% 
  filter(sap == "S1")

#create normalized depth column
sdf <- mutate(sdf, normalizedDepth = (depth-min(depth)) / (max(depth)-min(depth)))

#put back into TS, 
tts$normalizedDepth <- matrix(NA, nrow = length(tts$depth))
tts$normalizedDepth[which(tts$sapropelName=="S1")] = sdf$normalizedDepth

#assign back in
TS[[hasSap[i]]] <- tts
}

###EXAMPLE 3### bin TOC by AGE
binvec <- seq(0,10,0.1) #set up bins, 0 to 10000 yr BP with 100 year steps...

#pull out TOC
TS.TOC <- filterTs(TS.age,"paleoData_variableName == TOC") 
  
binned.TS = binTs(TS.TOC,binvec = binvec,timeVar = "age")
binYear <- binned.TS[[1]]$time
tocMat <- matrix(NA,ncol = length(binned.TS),nrow = length(binYear))
for(i in 1:ncol(tocMat)){
  if(!all(is.nan(binned.TS[[i]]$matrix))){
  tocMat[,i] <- binned.TS[[i]]$matrix
  }
}
#zscore
tocMatScaled <- scale(tocMat,scale = T,center = F)

#plot it.
ggplot()+geom_line(aes(x= binYear, y =rowMeans(tocMat,na.rm = T) , colour = "Raw TOC"))+
  geom_line(aes(x= binYear, y =rowMeans(tocMatScaled,na.rm = T), colour = "Scaled TOC" ))+
  xlab("age (ka)")+ylab("TOC")

#pull out coordinates for mapping
lat <- sapply(TS.TOC,"[[","geo_latitude")
lon <- sapply(TS.TOC,"[[","geo_longitude")





