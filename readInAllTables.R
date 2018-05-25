library(RMySQL)
library(here)
sap = dbConnect(MySQL(),user = "root",dbname = 'sapropel')#connect to database (only works if you have MySQL setup and the sapropel my sql database)

allFields <- dbListTables(sap) #get all the fields

all <- list()
for(i in 1:length(allFields)){#loop through and get all the data
  all[[allFields[i]]] <- dbReadTable(sap, name = allFields[i])
}

save(list = "all",file = here("allTables.RData"))
