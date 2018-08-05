############ ITHIM application for SACOG - Phase II ##########

##### SACSIM data process #####

# File: DataProcess_SWITRS_GeoCode.R
# Purpose: Allocate TAZ level data into ZIP code level and add the ZCTA info into Pop files

library(foreign)

# it is a random process for allocating zcta into each resident record
set.seed(2018)

# set your working directory
setwd("/Users/Yizheng/Documents/02_Work/17_ITHIM-Phase II/06_R Scripts/02_Data Preparation/03_Other R scripts")

# arcgis output: TAZ info include area
SACOG.TAZ <- read.dbf("FROM GIS/SACOG-TAZ.dbf")
head(SACOG.TAZ)

# arcgis output from "intersect" function: include the TAZ's area info in each ZIP  
SACOG.intersect <- read.dbf("FROM GIS/Intersection-TAZ2ZIP.dbf")
head(SACOG.intersect)

# merge above databases
TAZ.percentage <- merge(SACOG.intersect,SACOG.TAZ,by='TAZ07')
head(TAZ.percentage)

# compute the percentage according to TAZ's area
TAZ.percentage$percentage <- TAZ.percentage$Shape_Area/TAZ.percentage$Area

# prepare the final table to store the percentage info
keep <- c("TAZ07","COUNTY.x","ZCTA5CE10","percentage")
TAZ.percentage.t <- TAZ.percentage[,names(TAZ.percentage)%in%keep]
head(TAZ.percentage.t)

# load population data sets
load('population file from SACSIM/pop.2012.cmplt')
load('population file from SACSIM/pop.2020.cmplt')
load('population file from SACSIM/pop.2036.cmplt')

pop.2027.cmplt <- read.dbf('population file from SACSIM/pa27_pop.dbf')
pop.S1.cmplt <- read.dbf('population file from SACSIM/POP_S1.dbf')
pop.S2.cmplt <- read.dbf('population file from SACSIM/POP_S2.dbf')
pop.S3.cmplt <- read.dbf('population file from SACSIM/POP_S3.dbf')


allocateZCTA <- function(pop.file){
  
  # add a column for zcta
  pop.file$zcta <- 0
  # head(pop.file)
  
  # get the TAZ list
  TAZ.list <- unique(pop.file$HHTAZ)
  
  # allocate the population into zcta
  for (i in 1:length(TAZ.list)){
    
    #extract the population and area percentage info according to TAZ
    temp.taz.pop <- pop.file[which(pop.file$HHTAZ==TAZ.list[i]),]
    temp.taz.percentage <- TAZ.percentage.t[which(TAZ.percentage.t$TAZ07==TAZ.list[i]),]
    
    pop.taz <- nrow(temp.taz.pop)
    
    #size <- round(temp.taz.percentage$percentage*pop.taz)
    
    x <- as.character(temp.taz.percentage$ZCTA5CE10)
    temp <- sample(x,size = pop.taz,replace = T,prob = temp.taz.percentage$percentage)
    
    pop.file[which(pop.file$HHTAZ==TAZ.list[i]),ncol(pop.file)] <- temp
    
  }
  
  return(pop.file)
}


pop.2012.zcta <- allocateZCTA(pop.2012.cmplt)
pop.2020.zcta <- allocateZCTA(pop.2020.cmplt)
pop.2036.zcta <- allocateZCTA(pop.2036.cmplt)

pop.2027.zcta <- allocateZCTA(pop.2027.cmplt)
pop.S1.zcta <- allocateZCTA(pop.S1.cmplt)
pop.S2.zcta <- allocateZCTA(pop.S2.cmplt)
pop.S3.zcta <- allocateZCTA(pop.S3.cmplt)

save(pop.2012.zcta,file = "pop.2012.zcta")
save(pop.2020.zcta,file = "pop.2020.zcta")
save(pop.2036.zcta,file = "pop.2036.zcta")

save(pop.2027.zcta,file = "pop.2027.zcta")
save(pop.S1.zcta,file = "pop.S1.zcta")
save(pop.S2.zcta,file = "pop.S2.zcta")
save(pop.S3.zcta,file = "pop.S3.zcta")

# output the zcta list for SACOG region
zcta.list <- as.data.frame(unique(SACOG.intersect$ZCTA5CE10))
zcta.list <- sort(zcta.list$`unique(SACOG.intersect$ZCTA5CE10)`)
write.csv(as.data.frame(zcta.list),file = "zcta_list_SACOG.csv")
