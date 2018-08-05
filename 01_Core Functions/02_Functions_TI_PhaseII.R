# This file is part of ITHIM Sacramento PhaseII.

# File: 02_Functions_TI_PhaseII.R
# Purpose: Functions for calculating the estimated change of health burden related to traffic injury

# set working directory
setwd("/Users/Yizheng/Documents/02_Work/17_ITHIM-Phase II/06_R Scripts/GitHub/01_Core functions")

# Structure:
# Part 1: Read External Data Sources and Define Parameter
# Part 2: Function Definition - Data Inputs
# Part 3: Function Definition - Data Process (Baseline injury + Scenario Injury Multiplier)
# Part 4: Function Definition - Compute Scenario Injury and final injury results
# Part 5: Output the results as .csv files

# Code:
# Part 1 Read External Data Sources and Define Parameter --------------------------------------------------

# number of traffic modes, injuried types, and road types
nTrafficModeV <- 6L #victim mode
nTrafficModeS <- 7L #striking mode (include one-party)
nInjuriedType <- 2L #fatal & serious
ModeNames <- c("bike", "walk", "motorcycle", "car", "truck", "bus") #traffic mode
RoadTypes <- c("local", "arterial", "highway") # road type
nRoadType <- length(RoadTypes)

# disease burden
dbNames <- c("Deaths","DALYs")

# zcta list
zcta.list <- read.csv("00_Data/01_Population/03_zcta_list_SACOG.csv")
n.zcta <- nrow(zcta.list)

# population - SACOG
Pop.file.input <- read.csv("00_Data/01_Population/02_Population_SACOG_Baseline.csv")
Pop.file <- Pop.file.input[,2:3]

# population - US
US.pop <- read.csv("00_Data/01_Population/01_Population_US_PhaseII.csv")
US.pop <- matrix(cbind(US.pop[,2],US.pop[,3]),16,1)

# input the GBD data
GBD.injury <- read.csv("00_Data/04_GBD/03_GBD_US_TrafficInjury.csv")
baseline.injury.complete <- read.csv("00_Data/05_baseline injury/01_BaselineInjury_SACOG_PhaseII.csv")

# Part 2 Function Definition - Data Inputs ------------------------------------------------------

# function for reading csv files of injury data and VMT
# data source: SWITRS 2006-2016 (Statewide Integrated Traffic Reporting System)
# data source: Transportation Injury Mapping System (TIMS by US Berkeley)
# 11-year annual average number of road traffic injuries

input.csv <- function(zctaID,scenarioID){
  #scenarioID: 0-2012,1-2020,2-2036,3-2027,4-S1,5-S2,6-S3
  
  baseline.injury <- baseline.injury.complete[(37*zctaID-36):(37*zctaID-1),2:8]
  
  # input the person & vehicle distance matrix
  # include the distance distribution for three road types and occupancy for each traffic mode
  filenames.dist <- list.files(path="00_Data/06_PersonVehicleDistance")
  person.vehicle.distance_input.matrix <- matrix(NA,34,1)
  person.vehicle.distance_input <- read.csv(paste0("00_Data/06_PersonVehicleDistance/",
                                                   filenames.dist[scenarioID+1]))
  
  person.vehicle.distance_input.matrix[,1]<-person.vehicle.distance_input[1:34,(2*zctaID+1)]
  
  #GBD.local
  GBD.local <- GBD.injury[,2:5] * 
    matrix(cbind(Pop.file[(9*zctaID-8):(9*zctaID-1),1],Pop.file[(9*zctaID-8):(9*zctaID-1),2]),16,1)/ US.pop
  
  
  return(list(
    baseline.injury = baseline.injury,
    person.vehicle.distance_input.matrix = person.vehicle.distance_input.matrix,
    GBD.local = GBD.local
    
  ))
}

# Part 3 Function Definition - Data Process --------------------------------------------------

# Function of creating the matrix of baseline injuries
createBaselineInjury <- function(injury){
  # build the list of baseline injury data (the sheet "Baseline injuries" in ITHIM spreadsheet)
  injury.baseline <- list()
  for (n in 1:(nInjuriedType*nRoadType)){
    injury.baseline[[n]] <-injury[((n-1)*nTrafficModeV+1):(nTrafficModeV*n),1:(nTrafficModeS)]
  }
  injury.baseline <- lapply(injury.baseline,function(x) {
    row.names(x) <- ModeNames[1:6]
    return (x)
  })
  names(injury.baseline) <- c("local.fatal","arterial.fatal","highway.fatal",
                              "local.serious","arterial.serious","highway.serious")
  
  return(injury.baseline)
}



# Function of create the Distribution of Person & Vehicle Distance by Road Types
# create the sheet "Dist by road type" in ITHIM spreadsheet 
createScenarioInjuryMultiplier <- function(zctaID,scenarioID){
  
  ### for baseline ###
  # person distance for all modes (miles per day)
  # source: SACSIM15
  input.baseline.temp <- input.csv(zctaID,scenarioID = 0)
  t.person.distance.baseline <- input.baseline.temp$person.vehicle.distance_input.matrix[1:6,1]
  
  # matrix of distance (person) distribution by road types and modes for baseline (%)
  percent.person.baseline <- matrix(NA,nrow = 6,ncol = 3,
                                    dimnames=list(ModeNames,c("local","arterial","highway")))
    
  percent.person.baseline[,1] <- input.baseline.temp$person.vehicle.distance_input.matrix[8:13,1]
  percent.person.baseline[,2] <- input.baseline.temp$person.vehicle.distance_input.matrix[15:20,1]
  percent.person.baseline[,3] <- input.baseline.temp$person.vehicle.distance_input.matrix[22:27,1]
  
  # matrix of distance (person) by road types and modes for baseline (miles per day)
  distance.baseline.person <- t.person.distance.baseline * percent.person.baseline
  
  # matrix of occupancy
  # source: hard coded estimate
  occ.baseline <- input.baseline.temp$person.vehicle.distance_input.matrix[29:34,1]
   
  ### for scenario ###
  # person distance (person) for all road types and all modes (miles per day)
  # data source: SACSIM15
  input.scenario.temp <-input.csv(zctaID,scenarioID = scenarioID)
  
  t.person.distance.scenario <- input.scenario.temp$person.vehicle.distance_input.matrix[1:6,1]
  
  # matrix of distance (person) distribution by road types and modes for scenario (%)
  percent.person.scenario <- 
    matrix(NA,nrow = 6,ncol = 3,dimnames = list(ModeNames,c("local","arterial","highway")))
  
  percent.person.scenario[,1] <- input.scenario.temp$person.vehicle.distance_input.matrix[8:13,1]
  percent.person.scenario[,2] <- input.scenario.temp$person.vehicle.distance_input.matrix[15:20,1]
  percent.person.scenario[,3] <- input.scenario.temp$person.vehicle.distance_input.matrix[22:27,1]
  
  # matrix of distance (person) by road types for scenario (miles per day)
  distance.scenario.person <- t.person.distance.scenario * percent.person.scenario
  
  # matrix of occupancy
  occ.scenario <- input.scenario.temp$person.vehicle.distance_input.matrix[29:34,1]
  
  # matrix of distance (vehicle) by road types (miles per day)
  distance.baseline.vehicle <- distance.baseline.person/occ.baseline
  distance.scenario.vehicle <- distance.scenario.person/occ.scenario
  
  # combine those matrix into a list
  dist<-list(
    distance.baseline.person = distance.baseline.person,
    
    distance.scenario.person = distance.scenario.person,
    
    distance.baseline.vehicle = distance.baseline.vehicle,
    
    distance.scenario.vehicle = distance.scenario.vehicle
    
  )
  
  ### create the multiplier
  # safety factors (source: hard coded estimate)
  str.veh.safety.RR <- 0.5
  speed.safety <- 1.0
  other <- 1.0
  victim.safety <- 0.5
  
  # compute the list of scenario injury multiplier (i:row;j:col;k:road type)
  scenario.multiplier <- rep(list((matrix(NA,nrow=nTrafficModeV,ncol=nTrafficModeS,
                                          dimnames=list(ModeNames,c(ModeNames,"one.party"))))), nRoadType)
    
  names(scenario.multiplier) <- RoadTypes
  
  # using different calculation method for different party numbers (1vs2)
  for (k in 1:3){ #road type
    for (i in 1:6){ #victim vehicle mode
      # for one party collisions
      scenario.multiplier[[k]][i,7]<- 
        (dist$distance.scenario.person[i,k]/dist$distance.baseline.person[i,k])^victim.safety*speed.safety*other
      scenario.multiplier[[k]][i,7] <- replace(scenario.multiplier[[k]][i,7],is.nan(scenario.multiplier[[k]][i,7]),1)
      scenario.multiplier[[k]][i,7] <- replace(scenario.multiplier[[k]][i,7],is.infinite(scenario.multiplier[[k]][i,7]),1)
      
      for (j in 1:6){ #striking vehicle mode
        scenario.multiplier[[k]][i,j] <- 
          (dist$distance.scenario.person[i,k]/dist$distance.baseline.person[i,k])^victim.safety*(dist$distance.scenario.vehicle[j,k]/dist$distance.baseline.vehicle[j,k])^str.veh.safety.RR*speed.safety*other
        scenario.multiplier[[k]][i,j] <- replace(scenario.multiplier[[k]][i,j],is.nan(scenario.multiplier[[k]][i,j]),1)
        scenario.multiplier[[k]][i,j] <- replace(scenario.multiplier[[k]][i,j],is.infinite(scenario.multiplier[[k]][i,j]),1)
        
      }
      
    }
  }
  return(
    scenario.multiplier = scenario.multiplier)
  
}


# Part 4 Function Definition - Compute Scenario Injury and final injury results --------------------------------------------------

##### Scenario Injuries
#compute the scenario injuries (the product of baseline injury and scenario multiplier)
computeScenarioInjury <- function(injury.baseline,scenario.multiplier){
  
  
  injury.scenario <- mapply(function(x,y) x*y,
                                injury.baseline,
                                c(scenario.multiplier,scenario.multiplier),SIMPLIFY=FALSE)
  
  return(injury.scenario)
}

##### Injuries results
createInjuryResults <- function(zctaID,scenarioID){
  
  
  baseline.injury <- input.csv(zctaID = zctaID,scenarioID = 0)[[1]]
  
  # format baseline injury
  baseline.injury.format <-  createBaselineInjury(baseline.injury)
  
  # scenario multiplier
  scenario.multiplier <- createScenarioInjuryMultiplier(zctaID = zctaID,scenarioID = scenarioID)
  
  # scenario injury
  scenario.injury <- computeScenarioInjury(baseline.injury.format,scenario.multiplier)
  
  # compute the total injury
  total.injury.baseline <- lapply(baseline.injury.format,sum) 
  
  total.injury.scenario <- lapply(scenario.injury,sum) 
  
  # compute the relative risk of fatality 
  RR.fatality <- (total.injury.scenario[[1]]+
                        total.injury.scenario[[2]]+total.injury.scenario[[3]])/
    (total.injury.baseline[[1]]+total.injury.baseline[[2]]+total.injury.baseline[[3]])
  
  
  # compute the relative risk of serious injuries
  RR.serious <-(total.injury.scenario[[4]]+total.injury.scenario[[5]]+total.injury.scenario[[6]])/
    (total.injury.baseline[[4]]+total.injury.baseline[[5]]+total.injury.baseline[[6]])
  
  # GBD
  GBD.temp <- input.csv(zctaID = zctaID,scenarioID = 0)[[3]]
  
  # compute the reduction of deaths and DALYs by using the RR computed above
  Reduction.Death.disaggr <- matrix(GBD.temp[,1]*(1-RR.fatality),16,1)
  
  Reduction.yll.disaggr <- matrix(GBD.temp[,2]*(1-RR.fatality),16,1)
  
  Reduction.yld.disaggr <- matrix(GBD.temp[,3]*(1-RR.serious),16,1)
 
  Reduction.DALYs.disaggr <- Reduction.yll.disaggr+Reduction.yld.disaggr
  
  return(list(
    
    Reduction.Death=sum(Reduction.Death.disaggr),
    Reduction.DALYs=sum(Reduction.DALYs.disaggr),
    
    Reduction.Death.disaggr=Reduction.Death.disaggr,
    Reduction.DALYs.disaggr=Reduction.DALYs.disaggr
    
  ))
}

##### function for computing the age.std results
computeAgeStdOutput.injury <- function(zctaID,scenario){
  
  death.age.gender <- scenario$Reduction.Death.disaggr/matrix(
    cbind(Pop.file[((9*zctaID-8):(9*zctaID-1)),1],
          Pop.file[((9*zctaID-8):(9*zctaID-1)),2]),16,1)*100000
  death.age.gender <- replace(death.age.gender,is.na(death.age.gender),0)
  
  
  DALYs.age.gender <- scenario$Reduction.DALYs.disaggr/matrix(
    cbind(Pop.file[((9*zctaID-8):(9*zctaID-1)),1],
          Pop.file[((9*zctaID-8):(9*zctaID-1)),2]),16,1)*100000
  DALYs.age.gender <- replace(DALYs.age.gender,is.na(DALYs.age.gender),0)
  
  
  
  age.std.death <- sum(death.age.gender * US.pop)/sum(US.pop)
  
  age.std.DALYs <- sum(DALYs.age.gender * US.pop)/sum(US.pop)
  
  return(list(
    age.std.death = age.std.death,
    age.std.DALYs = age.std.DALYs
    
  ))
  
}

# Part 5 Output the results as .csv files --------------------------------------------------

# build several matrix to store the formated results
format.output.abs.change.death <- format.output.abs.change.DALYs <- format.output.age.std.change.death <-
  format.output.age.std.change.DALYs <- matrix(NA,nrow = n.zcta,ncol = 6,dimnames = list(paste0("zcta",1:n.zcta),c("2020","2027","2036","S1","S2","S3")))

for (i in 1:n.zcta){
  
  for (j in 1:6){#scenario ID
    
    temp.result <- createInjuryResults(i,j)
    
    format.output.abs.change.death[i,j] <- temp.result$Reduction.Death
    
    format.output.abs.change.DALYs[i,j] <- temp.result$Reduction.DALYs
    
    format.output.age.std.change.death[i,j] <- computeAgeStdOutput.injury(i,temp.result)$age.std.death
    
    format.output.age.std.change.DALYs[i,j] <- computeAgeStdOutput.injury(i,temp.result)$age.std.DALYs
  } 
  
}

write.csv(format.output.abs.change.death,file = "absolute change of deaths from TI.csv")
write.csv(format.output.abs.change.DALYs,file = "absolute change of DALYs from TI.csv")
write.csv(format.output.age.std.change.death,file = "age standardized change of deaths from TI.csv")
write.csv(format.output.age.std.change.DALYs,file = "age standardized change of DALYs from TI.csv")