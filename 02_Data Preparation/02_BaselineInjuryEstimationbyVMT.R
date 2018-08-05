# Library definitions
library(foreign)

# Prevent scientific notation
options(scipen = 100)

# Set working drectory
setwd("/Users/Yizheng/Documents/02_Work/17_ITHIM-Phase II/06_R Scripts/GitHub/02_Data Preparation/00_Data")

# input the zcta ID
zcta.list <- read.csv("zcta_list_SACOG.csv")
n.zcta <- nrow(zcta.list)

# input baseline regionwide injury
baseline.injury.region.input <- read.csv("RegionWide_BaselineInjury.csv")
baseline.injury.region <- baseline.injury.region[,2:8]

# six mode names
ModeNames <- c("bike","walk","motorcycle","car","truck","bus")

# use read.dbf() in package 'foreign' to input the .dbf data
triptable.2012 <- read.dbf('trip_2012.dbf')
load('pop.2012.zcta')

# recode the population data (adding the group ID of age, gender and zcta)
recode.pop <- function(pop){
  
  # recode age category
  pop$ageID <- 
    ifelse(pop$AGE<=4,1,
           ifelse(pop$AGE<=14,2,
                  ifelse(pop$AGE<=29,3,
                         ifelse(pop$AGE<=44,4,
                                ifelse(pop$AGE<=59,5,
                                       ifelse(pop$AGE<=69,6,
                                              ifelse(pop$AGE<=79,7,8)))))))
  
  # recode gender category
  pop$genderID <- 
    ifelse(pop$SEX==1,1,
           ifelse(pop$SEX==2,2,99))
  
  pop <- merge(pop,zcta.list,by="zcta")
  
  # add a column "ID"  for combining household id and person id
  pop$ID <- paste0('h',pop$SERIALNO,'p',pop$PNUM)
  return(pop)
}

# merge the datasets of population and trip
prepTripPop <- function(pop,triptable){
  
  # compute the mean travel time and travel distance per capita by mode
  aggr.by.mode <- aggregate(triptable[,c("TRAVTIME","TRAVDIST")],list(triptable$SAMPN,triptable$PERSN,triptable$MODE),sum)
  # rename the variables
  names(aggr.by.mode) <- c('SAMPN',"PERSN","MODE","TIME","DISTANCE")
  # sort the data by SAMPN, PERSN, and MODE
  aggr.by.mode <- aggr.by.mode[order(aggr.by.mode$SAMPN,aggr.by.mode$PERSN,aggr.by.mode$MODE),]
  
  # add a column "ID" for combining household id and person id
  aggr.by.mode$ID <- paste0('h',aggr.by.mode$SAMPN,'p',aggr.by.mode$PERSN)
  
  # merge two data sets by ID
  trip.pop <- merge(aggr.by.mode,pop,by.x = "ID",by.y = "ID")
  
  return(trip.pop)
}

#recode the variables in population matrix
pop.2012 <- recode.pop(pop.2012.zcta)

#merge pop and trip data sets
trip.pop.2012 <- prepTripPop(pop.2012,triptable.2012)

#compute Total VMT by traffic mode for baseline injury estimation (zip level)
computeTotalVMTbymode<-function(zctaID){
  
  total.vmt.byMode <- matrix(NA,nrow = 6,ncol = 1,dimnames = list(ModeNames,paste0("zcta",zctaID)))
  
  #bike
  total.vmt.byMode[1,1] <- sum(trip.pop.2012$DISTANCE[which(trip.pop.2012$MODE==8&trip.pop.2012$zctaID==zctaID)])
  #walk
  total.vmt.byMode[2,1] <- sum(trip.pop.2012$DISTANCE[which(trip.pop.2012$MODE==9&trip.pop.2012$zctaID==zctaID)])
  
  #motorized vmt
  vmt <- sum(trip.pop.2012$DISTANCE[which(trip.pop.2012$MODE==5&trip.pop.2012$zctaID==zctaID)])+
    sum(trip.pop.2012$DISTANCE[which(trip.pop.2012$MODE==6&trip.pop.2012$zctaID==zctaID)])+
    sum(trip.pop.2012$DISTANCE[which(trip.pop.2012$MODE==7&trip.pop.2012$zctaID==zctaID)])
  
  #motorcycle
  total.vmt.byMode[3,1]<-vmt*0.02
  
  #car
  total.vmt.byMode[4,1]<-vmt*0.9
  
  #truck
  total.vmt.byMode[5,1]<-vmt*0.06
  
  #bus
  total.vmt.byMode[6,1]<-vmt*0.02  
  
  
  return(total.vmt.byMode)
  
}

# obtain the region wide total VMT
region.total.vmt.byMode <- matrix(NA,nrow = 6,ncol = 1,dimnames = list(ModeNames,"region-wide"))

#bike
region.total.vmt.byMode[1,1] <- sum(trip.pop.2012$DISTANCE[which(trip.pop.2012$MODE==8)])
#walk
region.total.vmt.byMode[2,1] <- sum(trip.pop.2012$DISTANCE[which(trip.pop.2012$MODE==9)])

#motorized vmt
region.vmt <- sum(trip.pop.2012$DISTANCE[which(trip.pop.2012$MODE==5)])+
  sum(trip.pop.2012$DISTANCE[which(trip.pop.2012$MODE==6)])+
  sum(trip.pop.2012$DISTANCE[which(trip.pop.2012$MODE==7)])

#motorcycle
region.total.vmt.byMode[3,1]<-region.vmt*0.02

#car
region.total.vmt.byMode[4,1]<-region.vmt*0.9

#truck
region.total.vmt.byMode[5,1]<-region.vmt*0.06

#bus
region.total.vmt.byMode[6,1]<-region.vmt*0.02 

#parameter definition
striking.para <- 0.5
victim.para <- 0.5

baseline.injury.zcta <- NULL
cuttingline <- rep("",9)

for (i in 1:n.zcta){
  
  temp.ratio <- matrix(NA,nrow = 6,ncol = (6+1)) # nrow =mode type; ncol = mode + one.party
  
  zcta.vmt <- computeTotalVMTbymode(i)
  
  for (m in 1:6){ # mode * road type * injury type
    
    for (n in 1:6){ #two parties
      
      temp.ratio[m,n] <- ((zcta.vmt[m]/region.total.vmt.byMode[m])^victim.para)*
        ((zcta.vmt[n]/region.total.vmt.byMode[n])^striking.para)
      
    }
    
    temp.ratio[m,7] <- zcta.vmt[m]/region.total.vmt.byMode[m]
  } 
  
  temp.baseline.injury.zcta <- as.matrix(baseline.injury.region * rbind(temp.ratio,temp.ratio,temp.ratio,temp.ratio,temp.ratio,temp.ratio)) 
  rownames(temp.baseline.injury.zcta)<-c(rep(ModeNames,6))
  
  
  col.name.1 <- c(rep("local fatal",6),rep("arterial fatal",6),rep("highway fatal",6)
                ,rep("local serious",6),rep("arterial serious",6),rep("highway serious",6))
  
  col.name.2 <- c(paste0("zcta",i),rep("",35))
  
  temp.baseline.injury.zcta.t <- cbind(temp.baseline.injury.zcta,col.name.1,col.name.2)
  
  baseline.injury.zcta <- rbind(baseline.injury.zcta,temp.baseline.injury.zcta.t,cuttingline)
}

write.csv(baseline.injury.zcta,file = "BaselineInjury_SACOG.csv")

