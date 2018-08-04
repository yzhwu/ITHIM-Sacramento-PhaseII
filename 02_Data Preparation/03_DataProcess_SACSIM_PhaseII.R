# This file is part of ITHIM SACOG - PhaseII.

# File: DataProcess_SACSIM_PhaseII.R
# Purpose: Process data from SACSIM - an activity based model develped by SACOG.
#          Obtain the active travel data for base year (2012) and future years (2020,2027,2036) and senarios (S1,S2,S3)

# Library definitions
library(foreign)

# Prevent scientific notation
options(scipen = 100)

# Set working drectory
setwd("/Users/Yizheng/Documents/02_Work/17_ITHIM-Phase II/06_R Scripts/01_Data Preparation/00_Data Sets")

# Part 1: Function Definition----------------------------------------------------------------------------

# calculate the relative values (relative to the value of "female 15-29" according to ITHIM)
CalRelativeMatrix <- function(x){
  
  # if the value in the reference cell equals to 0, we use a small number(0.01) to avoid NA.
  if (x[3,2]==0){
    ref = 0.01
    x[,c(1:2)] <- x[,c(1:2)]/ref
    x[3,2] <- 1
  }else{
    ref <- x[3,2]
    x[,c(1:2)] <- x[,c(1:2)]/ref
  } 
  
  return(x)
}

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
  
  # combine zcta into the table of population 
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

#compute the population mean walking and cycing time (min/week)
pop.mean.at.time <- function(trip.pop,pop){
  
  #numeric matrix for active travel data
  Pop.AT.para <- matrix(NA,nrow = n.zcta,ncol = 2,
                        dimnames = list(paste0("zcta",c(1:n.zcta)),c("walk","cycle"))) 
  
  
  for(i in 1:n.zcta){# zcta ID
    
    #population mean walking time (MODE=9) for each race group (unit: min/week)
    Pop.AT.para[i,1]=sum(trip.pop$TIME[which(trip.pop$MODE==9&trip.pop$zctaID==i)])/length(which(pop$zctaID==i))*7
    #population mean cycling time (MODE=8) for each race group (unit: min/week)
    Pop.AT.para[i,2]=sum(trip.pop$TIME[which(trip.pop$MODE==8&trip.pop$zctaID==i)])/length(which(pop$zctaID==i))*7
    
  }
  
  #replace NAs
  Pop.AT.para <- replace(Pop.AT.para, is.na(Pop.AT.para), 0)
  
  return(Pop.AT.para=Pop.AT.para)
}

# process the data and compute the active travel information
ActiveTravelDataOutput <- function(pop,trip.pop,zctaID){
  
  # numeric matrix for travel time
  travel.times <- travel.distance <- matrix(nrow = 72, ncol = 2) #row:mode*age;col:gender
  
  # numeric matrix for population distribution
  pop.age.gender <- matrix(nrow = 8,ncol = 2) #row:age; col:gender
  
  
  for (i in 1:2){ #gender
    #print(i)
    for (j in 1:8){ #age
      #print(j)
     
       # extract the population distribution
      pop.temp <- nrow(pop[which(pop$genderID==i&pop$ageID==j&pop$zctaID==zctaID),])
      pop.age.gender[j,i] <- pop.temp
      
      # compute the travel time and travel distance according to ageID,genderID,Mode, and ZCTA ID
      for (k in 1:9){ #mode
        #time
        time.temp <- sum(trip.pop$TIME[which(trip.pop$ageID==j&trip.pop$genderID==i&trip.pop$MODE==k&trip.pop$zctaID==zctaID)])
        travel.times[j + 8 * (k - 1), i] <- time.temp
        
        #distance
        distance.temp <- sum(trip.pop$DISTANCE[which(trip.pop$ageID==j&trip.pop$genderID==i&trip.pop$MODE==k&trip.pop$zctaID==zctaID)])
        travel.distance[j + 8 * (k - 1), i] <- distance.temp
      }
    }
  }
  
  # compute the travel time and distance per capita (if pop=0,then the output=0)
  walk.time <- ifelse(pop.age.gender==0,0,travel.times[65:72,]/pop.age.gender)
  walk.distance <- ifelse(pop.age.gender==0,0,travel.distance[65:72,]/pop.age.gender)
  cycle.time <- ifelse(pop.age.gender==0,0,travel.times[57:64,]/pop.age.gender)
  cycle.distance <- ifelse(pop.age.gender==0,0,travel.distance[57:64,]/pop.age.gender)
  
  # compute the relative values
  re.walk.time <- CalRelativeMatrix(walk.time)
  re.walk.distance <- CalRelativeMatrix(walk.distance)
  re.cycle.time <- CalRelativeMatrix(cycle.time)
  re.cycle.distance <- CalRelativeMatrix(cycle.distance)
  
  # compute the speed (mph) 
  walk.speed <- walk.distance/(walk.time/60)
  walk.speed[is.na(walk.speed)] <- 0
  cycle.speed <- cycle.distance/(cycle.time/60)
  cycle.speed[is.na(cycle.speed)] <- 0
  
  # compute the relative value for speed
  re.walk.speed <- CalRelativeMatrix(walk.speed)
  re.cycle.speed <- CalRelativeMatrix(cycle.speed)
  
  # add dimnames
  dimname <- list(c(paste0("ageCat",1:8)),c("male","female"))
  dimnames(re.walk.time)<-dimnames(re.cycle.time)<-
    dimnames(re.walk.speed)<-dimnames(re.cycle.speed)<-dimnames(pop.age.gender)<-dimname
  
  return(list(
    travel.distance = travel.distance,
    travel.times = travel.times,
    pop.age.gender = pop.age.gender,
    re.walk.time = re.walk.time,
    re.cycle.time = re.cycle.time,
    re.walk.speed = re.walk.speed,
    re.cycle.speed = re.cycle.speed
  ))
  
}

# function of shaping the outout file
format.output <- function(pop,trip.pop){
  
  format.at <- NULL
  format.pop <- NULL
  
  for (i in 1:n.zcta){ # zctaID
    print(i)
    # compute the active travel data
    Travel.Output <- ActiveTravelDataOutput(pop,trip.pop,zctaID = i)
    
    format.at <- rbind(format.at,
                       cbind(Travel.Output$re.walk.time,c("relative.walk.time",rep("",7))),cuttingline, #relative walking time
                       cbind(Travel.Output$re.cycle.time,c("relative.cycle.time",rep("",7))),cuttingline, # relative cycling time
                       cbind(Travel.Output$re.walk.speed,c("relative.walk.speed",rep("",7))),cuttingline, # relative walking speed
                       cbind(Travel.Output$re.cycle.speed,c("relative.cycle.speed",rep("",7))),cuttingline # relative cycling speed
    )
    
    # obtain the population 
    format.pop <- rbind(format.pop,Travel.Output$pop.age.gender[,1:2],cuttingline[1:2])
  }
  # add a column for zctaID
  col.zctaID.at <- NULL
  col.zctaID.pop <- NULL
   
  for (i in 1:n.zcta){
    col.zctaID.at <- cbind(col.zctaID.at,c(paste0("zcta",i),rep("",35)))
    col.zctaID.pop <- cbind(col.zctaID.pop,c(paste0("zcta",i),rep("",8)))
  }
  
  col.zctaID.at.matrix <- matrix(col.zctaID.at,36*n.zcta,1)
  col.zctaID.pop.matrix <- matrix(col.zctaID.pop,9*n.zcta,1)
  
  final.format.at <- cbind(format.at,col.zctaID.at.matrix)
  final.format.pop <- cbind(format.pop,col.zctaID.pop.matrix)
  
  return(list(final.format.at = final.format.at,
              final.format.pop = final.format.pop))
}

#compute VMT by traffic mode for injury module
computeVMTbymode <- function(zctaID,trip.pop,pop){
  ModeNames <- c("bike","walk","motorcycle","car","truck","bus")
  mode.vmt <- matrix(NA,nrow = 6,ncol = 1,dimnames = list(ModeNames,paste0("zcta",zctaID)))
  
  pop <- length(which(pop$zctaID==zctaID))
  
  #bike
  mode.vmt[1,1] <- sum(trip.pop$DISTANCE[which(trip.pop$MODE==8&trip.pop$zctaID==zctaID)])/pop
  #walk
  mode.vmt[2,1] <- sum(trip.pop$DISTANCE[which(trip.pop$MODE==9&trip.pop$zctaID==zctaID)])/pop
  
  #motorized vmt
  vmt <- sum(trip.pop$DISTANCE[which(trip.pop$MODE==5&trip.pop$zctaID==zctaID)])/(3.5*pop)+sum(trip.pop$DISTANCE[which(trip.pop$MODE==6&trip.pop$zctaID==zctaID)])/(2*pop)+
    sum(trip.pop$DISTANCE[which(trip.pop$MODE==7&trip.pop$zctaID==zctaID)])/(pop)
    
  #motorcycle
  mode.vmt[3,1]<-vmt*0.02
  
  #car
  mode.vmt[4,1]<-vmt*0.9
  
  #truck
  mode.vmt[5,1]<-vmt*0.06
  
  #bus
  mode.vmt[6,1]<-vmt*0.02  
  
  
  return(mode.vmt)
}

# format the output file
format.personVehicleDistance <- function(vmt){
  col.vmt <- NULL
  
  for(i in 1:n.zcta){
    col.vmt <- cbind(col.vmt,rbind(vmt[[i]],matrix("",1,1),dist.local,matrix("",1,1),dist.arterial,matrix("",1,1),dist.highway,matrix("",1,1),occ),
                     matrix("",34,1))
  }
  
  return(col.vmt=col.vmt)
  
}


# Part 2: Data Processing---------------------------------------

# input the zcta ID
zcta.list <- read.csv("zcta_list_SACOG.csv")
n.zcta <- nrow(zcta.list)

# percentage information from ITHIM (distribution of traffic mode for each road type)
dist.local <- matrix(c("0.53","0.75","0.18","0.18","0.18","0.36"),nrow = 6,ncol = 1)
dist.arterial <- matrix(c("0.47","0.25","0.25","0.25","0.25","0.63"),nrow = 6,ncol = 1)
dist.highway <- matrix(c("0.000001","0.000001","0.57","0.57","0.57","0.01"),nrow = 6,ncol = 1)
occ <- matrix(c("1","1","1","1.75","1","10"),nrow = 6,ncol = 1)
ModeNames <- c("bike", "walk", "motorcycle", "car", "truck", "bus") #traffic mode
rownames(dist.local)<-rownames(dist.arterial)<- rownames(dist.highway)<-rownames(occ)<-ModeNames

# use read.dbf() in package 'foreign' to input the .dbf data
triptable.2012 <- read.dbf('trip_2012.dbf')

triptable.2020 <- read.dbf('trip_2020.dbf')
triptable.2027 <- read.dbf('trip_2027.dbf')
triptable.2036 <- read.dbf('trip_2036.dbf')

triptable.S1 <- read.dbf('trip_S1.dbf')
triptable.S2 <- read.dbf('trip_S2.dbf')
triptable.S3 <- read.dbf('trip_S3.dbf')

# load the processed syn pop
load('pop.2012.zcta')

load('pop.2027.zcta')
load('pop.2020.zcta')
load('pop.2036.zcta')

load('pop.S1.zcta')
load('pop.S2.zcta')
load('pop.S3.zcta')


#recode the variables in population matrix
pop.2012 <- recode.pop(pop.2012.zcta)

pop.2020 <- recode.pop(pop.2020.zcta)
pop.2027 <- recode.pop(pop.2027.zcta)
pop.2036 <- recode.pop(pop.2036.zcta)

pop.S1 <- recode.pop(pop.S1.zcta)
pop.S2 <- recode.pop(pop.S2.zcta)
pop.S3 <- recode.pop(pop.S3.zcta)

#merge pop and trip data sets
trip.pop.2012 <- prepTripPop(pop.2012,triptable.2012)

trip.pop.2020 <- prepTripPop(pop.2020,triptable.2020)
trip.pop.2027 <- prepTripPop(pop.2027,triptable.2027)
trip.pop.2036 <- prepTripPop(pop.2036,triptable.2036)

trip.pop.S1 <- prepTripPop(pop.S1,triptable.S1)
trip.pop.S2 <- prepTripPop(pop.S2,triptable.S2)
trip.pop.S3 <- prepTripPop(pop.S3,triptable.S3)

#output the mean population mean walk time and cycle time
Pop.AT.para.2012 <- pop.mean.at.time(trip.pop.2012,pop.2012)

Pop.AT.para.2020 <- pop.mean.at.time(trip.pop.2020,pop.2020)
Pop.AT.para.2027 <- pop.mean.at.time(trip.pop.2027,pop.2027)
Pop.AT.para.2036 <- pop.mean.at.time(trip.pop.2036,pop.2036)

Pop.AT.para.S1 <- pop.mean.at.time(trip.pop.S1,pop.S1)
Pop.AT.para.S2 <- pop.mean.at.time(trip.pop.S2,pop.S2)
Pop.AT.para.S3 <- pop.mean.at.time(trip.pop.S3,pop.S3)

cuttingline <- matrix("",1,3)

# save the output (population mean active travel time) into .csv files
write.csv(cbind(Pop.AT.para.2012,c("2012",rep("",(n.zcta-1)))),file = "02_PopulationMeanATTime.2012.csv")

write.csv(cbind(Pop.AT.para.2020,c("2020",rep("",(n.zcta-1)))),file = "02_PopulationMeanATTime.2020.csv")
write.csv(cbind(Pop.AT.para.2027,c("2027",rep("",(n.zcta-1)))),file = "02_PopulationMeanATTime.2027.csv")
write.csv(cbind(Pop.AT.para.2036,c("2036",rep("",(n.zcta-1)))),file = "02_PopulationMeanATTime.2036.csv")

write.csv(cbind(Pop.AT.para.S1,c("S1",rep("",(n.zcta-1)))),file = "02_PopulationMeanATTime.S1.csv")
write.csv(cbind(Pop.AT.para.S2,c("S2",rep("",(n.zcta-1)))),file = "02_PopulationMeanATTime.S2.csv")
write.csv(cbind(Pop.AT.para.S3,c("S3",rep("",(n.zcta-1)))),file = "02_PopulationMeanATTime.S3.csv")


# output the detail active transport information for each zcta and save them as .csv files
Travel.Output.2012 <- format.output(pop.2012,trip.pop.2012)

Travel.Output.2020 <- format.output(pop.2020,trip.pop.2020)
Travel.Output.2027 <- format.output(pop.2027,trip.pop.2027)
Travel.Output.2036 <- format.output(pop.2036,trip.pop.2036)

#
Travel.Output.S1 <- format.output(pop.S1,trip.pop.S1)
Travel.Output.S2 <- format.output(pop.S2,trip.pop.S2)
Travel.Output.S3 <- format.output(pop.S3,trip.pop.S3)

write.csv(Travel.Output.2012$final.format.at,file = 'ActiveTransport.2012.csv')
write.csv(Travel.Output.2012$final.format.pop,file = 'Population.2012.csv')

write.csv(Travel.Output.2020$final.format.at,file = 'ActiveTransport.2020.csv')
write.csv(Travel.Output.2020$final.format.pop,file = 'Population.2020.csv')

write.csv(Travel.Output.2027$final.format.at,file = 'ActiveTransport.2027.csv')
write.csv(Travel.Output.2027$final.format.pop,file = 'Population.2027.csv')

write.csv(Travel.Output.2036$final.format.at,file = 'ActiveTransport.2036.csv')
write.csv(Travel.Output.2036$final.format.pop,file = 'Population.2036.csv')

write.csv(Travel.Output.S1$final.format.at,file = 'ActiveTransport.S1.csv')
write.csv(Travel.Output.S1$final.format.pop,file = 'Population.S1.csv')

write.csv(Travel.Output.S2$final.format.at,file = 'ActiveTransport.S2.csv')
write.csv(Travel.Output.S2$final.format.pop,file = 'Population.S2.csv')

write.csv(Travel.Output.S3$final.format.at,file = 'ActiveTransport.S3.csv')
write.csv(Travel.Output.S3$final.format.pop,file = 'Population.S3.csv')


#compute VMT by traffic mode for injury module 
vmt.baseline <- lapply(c(1:n.zcta),function(x) computeVMTbymode(x,trip.pop.2012,pop.2012))

vmt.2020 <- lapply(c(1:n.zcta),function(x) computeVMTbymode(x,trip.pop.2020,pop.2020))
vmt.2027 <- lapply(c(1:n.zcta),function(x) computeVMTbymode(x,trip.pop.2027,pop.2027))
vmt.2036 <- lapply(c(1:n.zcta),function(x) computeVMTbymode(x,trip.pop.2036,pop.2036))

vmt.S1 <- lapply(c(1:n.zcta),function(x) computeVMTbymode(x,trip.pop.S1,pop.S1))
vmt.S2 <- lapply(c(1:n.zcta),function(x) computeVMTbymode(x,trip.pop.S2,pop.S2))
vmt.S3 <- lapply(c(1:n.zcta),function(x) computeVMTbymode(x,trip.pop.S3,pop.S3))

format.vmt.baseline <- format.personVehicleDistance(vmt.baseline)

format.vmt.2020 <- format.personVehicleDistance(vmt.2020)
format.vmt.2027 <- format.personVehicleDistance(vmt.2027)
format.vmt.2036 <- format.personVehicleDistance(vmt.2036)

format.vmt.S1 <- format.personVehicleDistance(vmt.S1)
format.vmt.S2 <- format.personVehicleDistance(vmt.S2)
format.vmt.S3 <- format.personVehicleDistance(vmt.S3)

write.csv(format.vmt.baseline,file = "00_PersonVehicleDistance_Baseline.csv")

write.csv(format.vmt.2020,file = "00_PersonVehicleDistance_2020.csv")
write.csv(format.vmt.2027,file = "00_PersonVehicleDistance_2027.csv")
write.csv(format.vmt.2036,file = "00_PersonVehicleDistance_2036.csv")

write.csv(format.vmt.S1,file = "00_PersonVehicleDistance_S1.csv")
write.csv(format.vmt.S2,file = "00_PersonVehicleDistance_S2.csv")
write.csv(format.vmt.S3,file = "00_PersonVehicleDistance_S3.csv")
