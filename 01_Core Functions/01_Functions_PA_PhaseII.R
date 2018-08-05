# This file is part of ITHIM Sacramento Phase II.

# File: 01_Functions_PA_PhaseII.R
# Purpose: Functions for calculating the estimated change of health burden related to physical activity
#         at ZCTA level (Phase II finer scale)

setwd("/Users/Yizheng/Documents/02_Work/17_ITHIM-Phase II/06_R Scripts/GitHub/01_Core functions")

# Structure:
# Part 1: Function Definition - Data Inputs
# Part 2: Function Definition - Data Process (GBD + Total Exposure + Relative Risk)
# Part 3: Function Definition - Computing Health Burden + adjust output format
# Part 4: Read external data sources and define parameter
# Part 5: Computation and output results as .csv file

# Code:
# Part 1 Function Definition - Data Inputs -------------------------------------------------------------

# function for reading .csv files by ZCTA
read.csv.files <- function(zctaID){
  
  # input the SACOG population file (source: 2012 ACS PUMS one-year estimates)
  Pop_Input <- as.matrix(Pop.file[(zctaID*9-8):(zctaID*9-1),2:3])
 
  # input the parameters of active transport of baseline,2020, 2027,and 2036
  # and for scenarios (S1,S2,S3)
  # (include relative walking/cycling time and speed)
  AT_Input.baseline <- AT.file.baseline[(zctaID*36-35):(zctaID*36-1),1:4]
  
  AT_Input.2020 <- AT.file.2020[(zctaID*36-35):(zctaID*36-1),1:4]
  AT_Input.2036 <- AT.file.2036[(zctaID*36-35):(zctaID*36-1),1:4]
  AT_Input.2027 <- AT.file.2027[(zctaID*36-35):(zctaID*36-1),1:4]
  AT_Input.S1 <- AT.file.S1[(zctaID*36-35):(zctaID*36-1),1:4]
  AT_Input.S2 <- AT.file.S2[(zctaID*36-35):(zctaID*36-1),1:4]
  AT_Input.S3 <- AT.file.S3[(zctaID*36-35):(zctaID*36-1),1:4]
  
  #input the GBD data
  gbd.deaths.SACOG <- GBD_Input_SACOG[zctaID+1]
  
  # combine all inputs of baseline into a list
  InputPara <- InputPara(Pop_Input,nonTravelMET_Input,gbd.deaths.SACOG)
  
  # return required parameters
  return(list(
    InputPara = InputPara,
    
    AT_Input.baseline = AT_Input.baseline,
    AT_Input.2020 = AT_Input.2020,
    AT_Input.2036 = AT_Input.2036,
    AT_Input.2027 = AT_Input.2027,
    AT_Input.S1 = AT_Input.S1,
    AT_Input.S2 = AT_Input.S2,
    AT_Input.S3 = AT_Input.S3,
    
    #population mean active travel time
    AT_Pop_MeanWalkTime.baseline =as.numeric(AT_Pop_MeanTime.baseline[zctaID,2]),
    AT_Pop_MeanCycleTime.baseline = as.numeric(AT_Pop_MeanTime.baseline[zctaID,3]),
    AT_Pop_MeanWalkTime.2020 = as.numeric(AT_Pop_MeanTime.2020[zctaID,2]),
    AT_Pop_MeanCycleTime.2020 = as.numeric(AT_Pop_MeanTime.2020[zctaID,3]),
    AT_Pop_MeanWalkTime.2036 = as.numeric(AT_Pop_MeanTime.2036[zctaID,2]),
    AT_Pop_MeanCycleTime.2036 = as.numeric(AT_Pop_MeanTime.2036[zctaID,3]),
    AT_Pop_MeanWalkTime.2027 = as.numeric(AT_Pop_MeanTime.2027[zctaID,2]),
    AT_Pop_MeanCycleTime.2027 = as.numeric(AT_Pop_MeanTime.2027[zctaID,3]),
    AT_Pop_MeanWalkTime.S1 = as.numeric(AT_Pop_MeanTime.S1[zctaID,2]),
    AT_Pop_MeanCycleTime.S1 = as.numeric(AT_Pop_MeanTime.S1[zctaID,3]),
    AT_Pop_MeanWalkTime.S2 = as.numeric(AT_Pop_MeanTime.S2[zctaID,2]),
    AT_Pop_MeanCycleTime.S2 = as.numeric(AT_Pop_MeanTime.S2[zctaID,3]),
    AT_Pop_MeanWalkTime.S3 = as.numeric(AT_Pop_MeanTime.S3[zctaID,2]),
    AT_Pop_MeanCycleTime.S3 = as.numeric(AT_Pop_MeanTime.S3[zctaID,3])
   
  ))
  
}

# function for shaping the input data sets (.csv files)
InputPara <- function (Pop_Input,nonTravelMET_Input,gbd_Input){
  # input the population and calculate the proportion of population
  
  PopProp <- Pop_Input/sum(Pop_Input)
  PopProp <- as.matrix(PopProp)
  
  # input the whole US population in 2010. source: US Census 2010
  allPop <- matrix (c(Pop_Input_US[1:8,2],Pop_Input_US[1:8,3]), 
                    byrow = TRUE, ncol = 1, nrow = nAgeClass*2, 
                    dimnames = list((c(paste0("maleAgeClass ",1:nAgeClass),
                                       paste0("femaleAgeClass ",1:nAgeClass))),"Population"))
  
  # input the non travel METs
  nonTravelMET <- as.matrix(nonTravelMET_Input[,2:6])
  
  dimnames(nonTravelMET) <- list((c(paste0("maleAgeClass ",1:nAgeClass),
                                    paste0("femaleAgeClass ",1:nAgeClass))),
                                 seq(0.1,0.9,by=0.2))
  
  #return required parameters
  return(list(
    SACOG.Pop = Pop_Input,
    PopProp = PopProp,
    allPop = allPop,
    nonTravelMET = nonTravelMET,
    gbd_Input = as.matrix(gbd_Input)
  )
  )
}

# Part 2 Function Definition - Data Process (GBD + total exposure + RR)-------------------------------------------------------------

# function for creating total exposure matrix 
TotalExposure <- function(PopMeanWalkTime, PopMeanCycleTime, AT_Input, PopProp, nonTravelMET){
  # #test
  # PopMeanWalkTime <- a$AT_Pop_MeanWalkTime.baseline
  # PopMeanCycleTime <- a$AT_Pop_MeanCycleTime.baseline
  # AT_Input <- a$AT_Input.baseline
  # PopProp <- a$InputPara$PopProp
  # nonTravelMET <- a$InputPara$nonTravelMET
  
  
  # The population mean walking/cycling speed (mph)
  #"It is common practice in MPOs to assume average walk speed of 3 mph and bicycle speed of 12 mph" 
  # from ITHIM user's manual
  PopMeanWalkSpeed <- 3.0
  PopMeanCycleSpeed <- 12.0
  cv <- -0.0108 * (PopMeanWalkTime + PopMeanCycleTime) / 7 + 1.2682 + 0.7
  
  # Numerical matrices for the relative walking time (relative to the value of "female 15-29") 
  # and the mean walking time
  # Source: SACSIM15
  Rwt <- as.matrix(AT_Input[1:8,2:3])
  dimnames(Rwt) =list(paste0("ageClass ",1:nAgeClass),c("M","F"))
  meanWalkTime <- Rwt / PopProp / sum(PopProp*Rwt) * PopMeanWalkTime * PopProp
  meanWalkTime <- replace(meanWalkTime, is.na(meanWalkTime), 0)
  
  # Numerical matrices for the relative cycling time (relative to the value of "female 15-29") 
  # and the mean cycling time 
  # Source: SACSIM15
  Rct <- as.matrix(AT_Input[10:17,2:3])
  dimnames(Rct) = list(paste0("ageClass ",1:nAgeClass),c("M","F"))
  meanCycleTime <- Rct / PopProp / sum(PopProp * Rct) * PopMeanCycleTime * PopProp
  meanCycleTime <- replace(meanCycleTime,is.na(meanCycleTime),0)
  
  # Numerical matrices for the proportion of mean cycling time to total active transport time
  PropMeanCycleTime <- meanCycleTime / (meanWalkTime+meanCycleTime)
  PropMeanCycleTime <- replace(PropMeanCycleTime,is.na(PropMeanCycleTime),0)
  
  # Numerical matrices for the relative walking speed (relative to the value of "female 15-29") 
  # and the mean walking speed
  Rws <- as.matrix(AT_Input[19:26,2:3])
  dimnames(Rws) =list(paste0("ageClass ",1:nAgeClass),c("M","F"))
  meanWalkSpeed <- Rws / PopProp / sum(PopProp * Rws) * PopMeanWalkSpeed * PopProp
  meanWalkSpeed <- replace(meanWalkSpeed,is.na(meanWalkSpeed),0)
  
  # Numerical matrices for the relative cycling speed (relative to the value of "female 15-29") 
  # and the mean cycling speed
  Rcs <- as.matrix(AT_Input[28:35,2:3])
  dimnames(Rcs) =list(paste0("ageClass ",1:nAgeClass),c("M","F"))
  meanCycleSpeed <- Rcs / PopProp / sum(PopProp * Rcs) * PopMeanCycleSpeed * PopProp
  meanCycleSpeed <- replace(meanCycleSpeed,is.na(meanCycleSpeed),0)
  
  # Numerical matrices for the mean walking/cycling MET values
  meanWalkMET <- ifelse(1.2216 * meanWalkSpeed + 0.0838 < 2.5, 2.5, 1.2216 * meanWalkSpeed + 0.0838)
  meanCycleMET <- matrix(6, byrow = TRUE, ncol = 2, nrow = nAgeClass, 
                         dimnames = list(paste0("ageClass ", 1:nAgeClass), c("M","F")))
  
  # Compute Quintiles of Active Transport Time
  
  # Total Active Transport Time
  totalATTime <- meanWalkTime + meanCycleTime
  
  # Numerical matrices for the log-normal distribution (mean, sd, log mean, log sd)
  meanATtime <- c(totalATTime[,1],totalATTime[,2])
  sd <- meanATtime * cv
  logMean <- log( meanATtime / sqrt ( 1 + ( meanATtime * cv / meanATtime )^2) )
  logMean <- replace(logMean,is.na(logMean),0)
  logSD <- sqrt( log( 1 + ( meanATtime * cv / meanATtime )^2))
  logSD <- replace(logSD,is.na(logSD),0)
  
  lognorm <- matrix(c(meanATtime,sd,logMean,logSD), byrow=FALSE, ncol = 4, nrow = nAgeClass*2,
                    dimnames = list(c(paste0("maleAgeClass ",1:nAgeClass),
                                      paste0("femaleAgeClass ",1:nAgeClass)),
                                    c("Mean","SD","log Mean","log sd")))
  
  # Compute quintiles of total AT time
  quintiles <- seq(0.1, 0.9, by=0.2)
  quintVec <- c()
  for (quant in quintiles) {
    quintVec <- c(quintVec, mapply(qlnorm,lognorm[,3],lognorm[,4],p=quant))
  }
  
  quintTotalATTime <- matrix (quintVec, byrow=FALSE, ncol = 5, nrow = nAgeClass*2,
                              dimnames = list((c(paste0("maleAgeClass ",1:nAgeClass),
                                                 paste0("femaleAgeClass ",1:nAgeClass))),
                                              seq(0.1,0.9,by=0.2)))
  quintTotalATTime[which(logMean==0),] <- 0
  
  # Compute the quintiles of total walking/cycling time
  PropMeanCycleTimeCol <- c(PropMeanCycleTime[,1],PropMeanCycleTime[,2])
  
  quintWalkTime <- quintTotalATTime * ( 1 - PropMeanCycleTimeCol)
  quintCycleTime <- quintTotalATTime - quintWalkTime
  
  # Compute the walking/cycling MET-hours and total AT MET-hours
  meanWalkMETCol <- c(meanWalkMET[,1], meanWalkMET[,2])
  quintWalkMET <- quintWalkTime * meanWalkMETCol / 60
  
  meanCycleMETCol <- c(meanCycleMET[,1],meanCycleMET[,2])
  quintCycleMET <- quintCycleTime * meanCycleMETCol / 60
  
  quintTotalTravelMET <- quintWalkMET + quintCycleMET
  
  # adding up total AT METs and non travel METs
  totalExposure <- ifelse( quintTotalTravelMET + nonTravelMET > 2.5, quintTotalTravelMET + nonTravelMET, 0.1)
  
  # if the population in that category equals 0, then total exposure =0
  pop.prop.reformat <- matrix(PopProp, 16, 1)
  totalExposure[which(pop.prop.reformat==0),] <- 0
  
  #return the matrix of total exposure
  return(
    totalExposure <- totalExposure
  )
}

# function for computing relative risks of physical activity  
create.PA.RR <- function(){
  
  RR.lit <- exposure <- matrix(NA,nrow = nAgeClass, ncol = 2,
                               dimnames=list(paste0("agClass",1:nAgeClass),c("F","M")))
  
  # all cause mortality (source: Woodcock)
  exposure[1:nAgeClass,1:2] <- 11
  RR.lit[1:nAgeClass,1:2] <- 0.81
  
  #compute RR matrix
  RR <- RR.lit^( 1 / exposure )^k
  
  #reshape RR matrix
  reshapeRR <- function(RR, nQuantiles = 5){
    matrix(c(RR[,"M"],RR[,"F"]),nrow = nAgeClass * 2,ncol = nQuantiles,
           dimnames = list((c(paste0("maleAgeClass ",1:nAgeClass),
                              paste0("femaleAgeClass ",1:nAgeClass))),seq(0.1,0.9,by=0.2)))
    
  }
  
  RR <- reshapeRR(RR,nQuantiles = 5)
  
  return(RR)
  
}

#function for computing local disease burden (scaling process)
computeLocalGBD <- function (death_target,TargetPop,InputPara){

  #obtain the gbd and the death data of U.S.
  gbd_US <- gbd_Input_US[,2:5]
  death_US <- as.numeric(gbd_Input_US[,2])
  
  # calculate the RR (the ratio of death numbers for target area with those for whole U.S.)
  RR_gbd <- matrix(NA,ncol = 1,nrow = 2*nAgeClass,dimnames = list((c(paste0("maleAgeclass",1:nAgeClass),paste0("femaleAgeclass",1:nAgeClass))),"RR"))
  RR_gbd <- (death_target/TargetPop)/(death_US/InputPara$allPop)
  RR_gbd <- replace(RR_gbd,is.na(RR_gbd)|is.infinite(RR_gbd),1.0)
  
  # obtain the local gbd data
  gbd.local<-RR_gbd*gbd_US/InputPara$allPop*TargetPop
  
  return(gbd.local)
}

#function for computing local disease burden
Extract_LocalGBD <- function(InputPara){
  
  death_target <- InputPara$gbd_Input
  
  TargetPop <- matrix (InputPara$PopProp * sum(InputPara$SACOG.Pop), 
                   byrow = TRUE, ncol = 1, nrow = nAgeClass * 2, 
                   dimnames = list((c(paste0("maleAgeClass ",1:nAgeClass),
                                      paste0("femaleAgeClass ",1:nAgeClass))),"Population"))
  death_target[which(TargetPop==0)] <- 0
  
  gbd.local <- computeLocalGBD(death_target,TargetPop,InputPara)
  
  return(
    gbd.local = gbd.local)
}

# Part 3 Function Definition - Computing Health Burden --------------------------------------------------------

#functions for computing health outcome
computeHealthOutcome <- function (RR.PA,BaselineTotalExpo,ScenarioTotalExpo,gbd.local){
  
  #Compute RR for an exposure of x MET
  RR.Baseline <- RR.PA^( BaselineTotalExpo^k )
  RR.Scenario <- RR.PA^( ScenarioTotalExpo^k )
  
  #Compute Ratio of DB relative to group 1
  RatioDB.Baseline <- RR.Baseline / RR.Baseline[,1]
  RatioDB.Scenario <- RR.Scenario / RR.Scenario[,1]
  sum.RatioDB.Baseline <- rowSums(RatioDB.Baseline)
  sum.RatioDB.Scenario <- rowSums(RatioDB.Scenario)
  
  #Compute New Burden and AF
  new.burden <- rowSums(RR.Scenario) / rowSums(RR.Baseline)
  AF <- 1 - new.burden
  
  #Compute the health outcomes
  #Define a function for outputing health outcomes
  fun.outcome <- function(x,y){
    x[,1] <- y
    x[,c(2:5)] <- x[,c(2:5)]*y
    return(x)}
  
  #Compute deaths per group
  dproj.scenario.firstCol <- new.burden * gbd.local$deaths / sum.RatioDB.Scenario
  dproj.scenario <- fun.outcome(RatioDB.Scenario,dproj.scenario.firstCol)
  
  dproj.baseline.firstCol <- gbd.local$deaths / sum.RatioDB.Baseline
  dproj.baseline <- fun.outcome(RatioDB.Baseline,dproj.baseline.firstCol)
  
  #Compute YLL per group
  yll.scenario.firstCol <- new.burden * gbd.local$yll / sum.RatioDB.Scenario
  yll.scenario <- fun.outcome(RatioDB.Scenario,yll.scenario.firstCol)
  
  yll.baseline.firstCol <- gbd.local$yll / sum.RatioDB.Baseline
  yll.baseline <- fun.outcome(RatioDB.Baseline,yll.baseline.firstCol)
  
  #Compute YLD per group
  yld.scenario.firstCol <- new.burden * gbd.local$yld / sum.RatioDB.Scenario
  yld.scenario <- fun.outcome(RatioDB.Scenario, yld.scenario.firstCol)
  
  yld.baseline.firstCol <- gbd.local$yld / sum.RatioDB.Baseline
  yld.baseline <- fun.outcome(RatioDB.Baseline,yld.baseline.firstCol)
  
  #Compute the delta Burden, total delta Burden, and the proportion
  delta.Burden <- (matrix(NA,nrow = nAgeClass * 2,ncol = 4,
                          dimnames = list((c(paste0("maleAgeClass ",1:nAgeClass),
                                             paste0("femaleAgeClass ",1:nAgeClass))),
                                          c("delta.Deaths","delta.YLL","delta.YLD","DALYS"))))
  
  delta.Burden[,1] <- rowSums(dproj.scenario) - rowSums(dproj.baseline) #deaths
  delta.Burden[,2] <- rowSums(yll.scenario) - rowSums(yll.baseline)     #yll
  delta.Burden[,3] <- rowSums(yld.scenario) - rowSums(yld.baseline)     #yld
  delta.Burden[,4] <- delta.Burden[,2] + delta.Burden[,3]               #dalys
  
  total.delta.Burden <- colSums(delta.Burden)
  total.gbd.local <- ifelse(colSums(gbd.local)!=0,colSums(gbd.local),0.0001)
  
  prop.delta.Burden <- total.delta.Burden / total.gbd.local
  
  return(list(
    AF = AF,
    new.burden = new.burden,
    delta.Burden = delta.Burden,
    prop.delta.Burden = prop.delta.Burden
  ))
}

#output the detail heath outcome for each zcta
output.HealthOutcome <- function(zctaID){
  
  # reading the csv files after inputting zctaID
  All.InputPara <- read.csv.files(zctaID = zctaID)
  
  #Create the total exposure matrices by inputing parameters 
  #(mean walking time(min per week), mean cycling time(min per week))
  BaselineTotalExpo <- TotalExposure(All.InputPara$AT_Pop_MeanWalkTime.baseline,
                                     All.InputPara$AT_Pop_MeanCycleTime.baseline,
                                     All.InputPara$AT_Input.baseline,
                                     All.InputPara$InputPara$PopProp,
                                     All.InputPara$InputPara$nonTravelMET)
  
  
  ScenarioTotalExpo.2020 <-TotalExposure(All.InputPara$AT_Pop_MeanWalkTime.2020,
                                         All.InputPara$AT_Pop_MeanCycleTime.2020,
                                         All.InputPara$AT_Input.2020,
                                         All.InputPara$InputPara$PopProp,
                                         All.InputPara$InputPara$nonTravelMET)
  
  
  ScenarioTotalExpo.2036 <-TotalExposure(All.InputPara$AT_Pop_MeanWalkTime.2036,
                                         All.InputPara$AT_Pop_MeanCycleTime.2036,
                                         All.InputPara$AT_Input.2036,
                                         All.InputPara$InputPara$PopProp,
                                         All.InputPara$InputPara$nonTravelMET)
  
  ScenarioTotalExpo.2027 <-TotalExposure(All.InputPara$AT_Pop_MeanWalkTime.2027,
                                         All.InputPara$AT_Pop_MeanCycleTime.2027,
                                         All.InputPara$AT_Input.2027,
                                         All.InputPara$InputPara$PopProp,
                                         All.InputPara$InputPara$nonTravelMET)
  
  ScenarioTotalExpo.S1 <-TotalExposure(All.InputPara$AT_Pop_MeanWalkTime.S1,
                                         All.InputPara$AT_Pop_MeanCycleTime.S1,
                                         All.InputPara$AT_Input.S1,
                                         All.InputPara$InputPara$PopProp,
                                         All.InputPara$InputPara$nonTravelMET)
  
  ScenarioTotalExpo.S2 <-TotalExposure(All.InputPara$AT_Pop_MeanWalkTime.S2,
                                       All.InputPara$AT_Pop_MeanCycleTime.S2,
                                       All.InputPara$AT_Input.S2,
                                       All.InputPara$InputPara$PopProp,
                                       All.InputPara$InputPara$nonTravelMET)
  
  ScenarioTotalExpo.S3 <-TotalExposure(All.InputPara$AT_Pop_MeanWalkTime.S3,
                                       All.InputPara$AT_Pop_MeanCycleTime.S3,
                                       All.InputPara$AT_Input.S3,
                                       All.InputPara$InputPara$PopProp,
                                       All.InputPara$InputPara$nonTravelMET)
  
  #compute the relative risks of Physical Activity (1MET)
  RR.PA <- create.PA.RR()
  
  #compute local disease burden, and create a list to store local gbd for all races
  LocalGBD <- Extract_LocalGBD(All.InputPara$InputPara)
  
  #compute health outcomes
  HealthOutcome.2020 <- computeHealthOutcome(RR.PA,BaselineTotalExpo,ScenarioTotalExpo.2020,LocalGBD)
  HealthOutcome.2036 <- computeHealthOutcome(RR.PA,BaselineTotalExpo,ScenarioTotalExpo.2036,LocalGBD)
  HealthOutcome.2027 <- computeHealthOutcome(RR.PA,BaselineTotalExpo,ScenarioTotalExpo.2027,LocalGBD)
  HealthOutcome.S1 <- computeHealthOutcome(RR.PA,BaselineTotalExpo,ScenarioTotalExpo.S1,LocalGBD)
  HealthOutcome.S2 <- computeHealthOutcome(RR.PA,BaselineTotalExpo,ScenarioTotalExpo.S2,LocalGBD)
  HealthOutcome.S3 <- computeHealthOutcome(RR.PA,BaselineTotalExpo,ScenarioTotalExpo.S3,LocalGBD)
  
  return(list(
    HealthOutcome.2020 = HealthOutcome.2020,
    HealthOutcome.2036 = HealthOutcome.2036,
    HealthOutcome.2027 = HealthOutcome.2027,
    
    HealthOutcome.S1 = HealthOutcome.S1,
    HealthOutcome.S2 = HealthOutcome.S2,
    HealthOutcome.S3 = HealthOutcome.S3
  ))
}

#function for computing health outcome (absolute total change)
AbsHealthOutcome <- function(zctaID){
  
  Change.Death.matrix.2020 <- Change.Death.matrix.2036 <-Change.Death.matrix.2027<-
    Change.Death.matrix.S1<-Change.Death.matrix.S2<-Change.Death.matrix.S3<-
    matrix(NA,nrow = n.zcta,ncol = 1,dimnames = list(paste0("zcta",1:n.zcta),"Change.Death"))
  
  Change.DALYs.matrix.2020 <- Change.DALYs.matrix.2036 <- Change.DALYs.matrix.2027<-
    Change.DALYs.matrix.S1<-Change.DALYs.matrix.S2<-Change.DALYs.matrix.S3<-
    matrix(NA,nrow = n.zcta,ncol = 1,dimnames = list(paste0("zcta",1:n.zcta),"Change.DALYs"))
  
  m=1
  for(j in 1:zctaID){
    temp <- output.HealthOutcome(j)
    
    Change.Death.matrix.2020[m,1]<-colSums(temp$HealthOutcome.2020$delta.Burden)[1]
    Change.DALYs.matrix.2020[m,1]<-colSums(temp$HealthOutcome.2020$delta.Burden)[4]
    Change.Death.matrix.2036[m,1]<-colSums(temp$HealthOutcome.2036$delta.Burden)[1]
    Change.DALYs.matrix.2036[m,1]<-colSums(temp$HealthOutcome.2036$delta.Burden)[4]
    Change.Death.matrix.2027[m,1]<-colSums(temp$HealthOutcome.2027$delta.Burden)[1]
    Change.DALYs.matrix.2027[m,1]<-colSums(temp$HealthOutcome.2027$delta.Burden)[4]
    
    Change.Death.matrix.S1[m,1]<-colSums(temp$HealthOutcome.S1$delta.Burden)[1]
    Change.DALYs.matrix.S1[m,1]<-colSums(temp$HealthOutcome.S1$delta.Burden)[4]
    Change.Death.matrix.S2[m,1]<-colSums(temp$HealthOutcome.S2$delta.Burden)[1]
    Change.DALYs.matrix.S2[m,1]<-colSums(temp$HealthOutcome.S2$delta.Burden)[4]
    Change.Death.matrix.S3[m,1]<-colSums(temp$HealthOutcome.S3$delta.Burden)[1]
    Change.DALYs.matrix.S3[m,1]<-colSums(temp$HealthOutcome.S3$delta.Burden)[4]
    
    m=m+1
  }
  
  return(list(
    Change.Death.matrix.2020=Change.Death.matrix.2020,
    Change.Death.matrix.2027=Change.Death.matrix.2027,
    Change.Death.matrix.2036=Change.Death.matrix.2036,
    Change.Death.matrix.S1=Change.Death.matrix.S1,
    Change.Death.matrix.S2=Change.Death.matrix.S2,
    Change.Death.matrix.S3=Change.Death.matrix.S3,
    
    Change.DALYs.matrix.2020=Change.DALYs.matrix.2020,
    Change.DALYs.matrix.2027=Change.DALYs.matrix.2027,
    Change.DALYs.matrix.2036=Change.DALYs.matrix.2036,
    Change.DALYs.matrix.S1=Change.DALYs.matrix.S1,
    Change.DALYs.matrix.S2=Change.DALYs.matrix.S2,
    Change.DALYs.matrix.S3=Change.DALYs.matrix.S3
    
    
  ))
}

#functions for computing age-normalized health outcome
computeAgeStdOutput <- function(All.InputPara,HealthOutcome){
  
  # input the US population as the reference
  US.pop <- All.InputPara$InputPara$allPop
  
  #shape the matrix 
  local.pop <- matrix(data=All.InputPara$InputPara$SACOG.Pop,nrow = 16,ncol = 1)
  
  delta.death <- HealthOutcome$delta.Burden[,1]
  delta.DALYs <- HealthOutcome$delta.Burden[,4]
  
  death.rate <- replace(delta.death/local.pop*100000,is.na(delta.death/local.pop),0) 
  DALYs.rate <- replace(delta.DALYs/local.pop*100000,is.na(delta.DALYs/local.pop),0) 
  
  age.std.death <- age.std.DALYs <- matrix(NA,1,1)
  
  #scale process
  age.std.death[1,1]=sum(death.rate[,1]*US.pop)/sum(US.pop)
  age.std.DALYs[1,1]=sum(DALYs.rate[,1]*US.pop)/sum(US.pop)
  
  return(list(
    age.std.death = age.std.death,
    age.std.DALYs = age.std.DALYs
  ))
}

#output the age.std health outcome
AgeStdHealthOutcome <- function(zctaID) {
  
  AgeStdDeath.matrix.2020 <- AgeStdDeath.matrix.2036 <-AgeStdDeath.matrix.2027<-
    AgeStdDeath.matrix.S1<-AgeStdDeath.matrix.S2<-AgeStdDeath.matrix.S3<-
    matrix(NA,nrow = n.zcta,ncol = 1,dimnames = list(paste0("zcta",1:n.zcta),"AgeStdChange.deaths"))
  
  AgeStdDALYs.matrix.2020 <- AgeStdDALYs.matrix.2036 <-AgeStdDALYs.matrix.2027<-
    AgeStdDALYs.matrix.S1<-AgeStdDALYs.matrix.S2<-AgeStdDALYs.matrix.S3<-
    matrix(NA,nrow = n.zcta,ncol = 1,dimnames = list(paste0("zcta",1:n.zcta),"AgeStdChange.DALYs"))
  
  j=1
  for (i in 1:zctaID){
    HealthOutcome <- output.HealthOutcome(i)
    All.InputPara <- read.csv.files(i)
    
    temp.2020 <- computeAgeStdOutput(All.InputPara,HealthOutcome$HealthOutcome.2020)
    
    temp.2036 <- computeAgeStdOutput(All.InputPara,HealthOutcome$HealthOutcome.2036)
    
    temp.2027 <- computeAgeStdOutput(All.InputPara,HealthOutcome$HealthOutcome.2027)
  
    temp.S1 <- computeAgeStdOutput(All.InputPara,HealthOutcome$HealthOutcome.S1)
    
    temp.S2 <- computeAgeStdOutput(All.InputPara,HealthOutcome$HealthOutcome.S2)
    
    temp.S3 <- computeAgeStdOutput(All.InputPara,HealthOutcome$HealthOutcome.S3)
   
    
    AgeStdDeath.matrix.2020[j,] <- temp.2020$age.std.death
    AgeStdDeath.matrix.2036[j,] <- temp.2036$age.std.death
    AgeStdDeath.matrix.2027[j,] <- temp.2027$age.std.death
    AgeStdDeath.matrix.S1[j,] <- temp.S1$age.std.death
    AgeStdDeath.matrix.S2[j,] <- temp.S2$age.std.death
    AgeStdDeath.matrix.S3[j,] <- temp.S3$age.std.death
    
    
    AgeStdDALYs.matrix.2020[j,] <- temp.2020$age.std.DALYs
    AgeStdDALYs.matrix.2036[j,] <- temp.2036$age.std.DALYs
    AgeStdDALYs.matrix.2027[j,] <- temp.2027$age.std.DALYs
    AgeStdDALYs.matrix.S1[j,] <- temp.S1$age.std.DALYs
    AgeStdDALYs.matrix.S2[j,] <- temp.S2$age.std.DALYs
    AgeStdDALYs.matrix.S3[j,] <- temp.S3$age.std.DALYs
    
    j=j+1
  }
  
  return(list(
    AgeStdDeath.matrix.2020=AgeStdDeath.matrix.2020,
    AgeStdDeath.matrix.2027=AgeStdDeath.matrix.2027,
    AgeStdDeath.matrix.2036=AgeStdDeath.matrix.2036,
    AgeStdDeath.matrix.S1=AgeStdDeath.matrix.S1,
    AgeStdDeath.matrix.S2=AgeStdDeath.matrix.S2,
    AgeStdDeath.matrix.S3=AgeStdDeath.matrix.S3,
    
    AgeStdDALYs.matrix.2020=AgeStdDALYs.matrix.2020,
    AgeStdDALYs.matrix.2027=AgeStdDALYs.matrix.2027,
    AgeStdDALYs.matrix.2036=AgeStdDALYs.matrix.2036,
    AgeStdDALYs.matrix.S1=AgeStdDALYs.matrix.S1,
    AgeStdDALYs.matrix.S2=AgeStdDALYs.matrix.S2,
    AgeStdDALYs.matrix.S3=AgeStdDALYs.matrix.S3
    
    
  ))
  
}

# Part 4 Read external data sources and define parameter -------------------------------------------------

# Number of age & demographic categories
nAgeClass <- 8L

# paramter of Physical Activity Risk Function (power)
k <- 0.5

# disease burden
dbNames <- c('Deaths', 'DALYs')

# population input
Pop_Input_US <- read.csv("00_Data/01_Population/01_Population_US_PhaseII.csv")
Pop.file <- read.csv("00_Data/01_Population/02_Population_SACOG_Baseline.csv")

# zcta list
zcta.list <- read.csv("00_Data/01_Population/03_zcta_list_SACOG.csv")
n.zcta <- nrow(zcta.list)

# input the .csv files of active travel data
AT.file.baseline <- read.csv("00_Data/02_ActiveTransport/01_ActiveTransport_Baseline_PhaseII/01_ActiveTransport.2012.csv")
AT_Pop_MeanTime.baseline <- read.csv("00_Data/02_ActiveTransport/01_ActiveTransport_Baseline_PhaseII/02_PopulationMeanATTime.2012.csv")

AT.file.2020 <- read.csv("00_Data/02_ActiveTransport/02_ActiveTransport_2020_PhaseII/01_ActiveTransport.2020.csv")
AT_Pop_MeanTime.2020 <- read.csv("00_Data/02_ActiveTransport/02_ActiveTransport_2020_PhaseII/02_PopulationMeanATTime.2020.csv")

AT.file.2036 <- read.csv("00_Data/02_ActiveTransport/03_ActiveTransport_2036_PhaseII/01_ActiveTransport.2036.csv")
AT_Pop_MeanTime.2036 <- read.csv("00_Data/02_ActiveTransport/03_ActiveTransport_2036_PhaseII/02_PopulationMeanATTime.2036.csv")

AT.file.2027 <- read.csv("00_Data/02_ActiveTransport/04_ActiveTransport_2027_PhaseII/01_ActiveTransport.2027.csv")
AT_Pop_MeanTime.2027 <- read.csv("00_Data/02_ActiveTransport/04_ActiveTransport_2027_PhaseII/02_PopulationMeanATTime.2027.csv")

AT.file.S1 <- read.csv("00_Data/02_ActiveTransport/05_ActiveTransport_S1_PhaseII/01_ActiveTransport.S1.csv")
AT_Pop_MeanTime.S1 <- read.csv("00_Data/02_ActiveTransport/05_ActiveTransport_S1_PhaseII/02_PopulationMeanATTime.S1.csv")

AT.file.S2 <- read.csv("00_Data/02_ActiveTransport/06_ActiveTransport_S2_PhaseII/01_ActiveTransport.S2.csv")
AT_Pop_MeanTime.S2 <- read.csv("00_Data/02_ActiveTransport/06_ActiveTransport_S2_PhaseII/02_PopulationMeanATTime.S2.csv")

AT.file.S3 <- read.csv("00_Data/02_ActiveTransport/07_ActiveTransport_S3_PhaseII/01_ActiveTransport.S3.csv")
AT_Pop_MeanTime.S3 <- read.csv("00_Data/02_ActiveTransport/07_ActiveTransport_S3_PhaseII/02_PopulationMeanATTime.S3.csv")

# input the GBD file
gbd_Input_US <- read.csv("00_Data/04_GBD/01_GBD_US_AllCause.csv")
GBD_Input_SACOG <- read.csv("00_Data/04_GBD/02_GBD_SACOG_AllCause.csv")

# input the matrix of Non-travel METs 
# source: CHIS2005 (Per capita weekly non-travel related physical activity expressed as metabolic equivalent tasks (kcal/kg body weight/hr of activity))
nonTravelMET_Input <- read.csv("00_Data/03_nonTravelMET/01_nonTravelMET_PhaseII.csv")

# Part 5 Computation and output results as .csv file -------------------------------------------------

# absolute change of health outcomes
abs.change <- AbsHealthOutcome(n.zcta)
age.std.change <- AgeStdHealthOutcome(n.zcta)

# output the .csv file for absolute change of death
format.output.abs.change.death <- cbind(abs.change$Change.Death.matrix.2020,abs.change$Change.Death.matrix.2027,
                                  abs.change$Change.Death.matrix.2036,abs.change$Change.Death.matrix.S1,
                                  abs.change$Change.Death.matrix.S2,abs.change$Change.Death.matrix.S3)
colnames(format.output.abs.change.death) <- c("2020","2027","2036","S1","S2","S3")

write.csv(format.output.abs.change.death,file = "absolute change of death from PA.csv")

# output the .csv file for absolute change of DALYs
format.output.abs.change.DALYs <- cbind(abs.change$Change.DALYs.matrix.2020,abs.change$Change.DALYs.matrix.2027,
                                        abs.change$Change.DALYs.matrix.2036,abs.change$Change.DALYs.matrix.S1,
                                        abs.change$Change.DALYs.matrix.S2,abs.change$Change.DALYs.matrix.S3)
colnames(format.output.abs.change.DALYs) <- c("2020","2027","2036","S1","S2","S3")

write.csv(format.output.abs.change.DALYs,file = "absolute change of DALYs from PA.csv")

# output the .csv file for age standardized change of death
format.output.age.std.change.death <- cbind(age.std.change$AgeStdDeath.matrix.2020,age.std.change$AgeStdDeath.matrix.2027,
                                            age.std.change$AgeStdDeath.matrix.2036,age.std.change$AgeStdDeath.matrix.S1,
                                            age.std.change$AgeStdDeath.matrix.S2,age.std.change$AgeStdDeath.matrix.S3)
colnames(format.output.age.std.change.death) <- c("2020","2027","2036","S1","S2","S3")

write.csv(format.output.age.std.change.death,file = "age standardized change of death from PA.csv")

# output the .csv file for age standardized change of DALYs
format.output.age.std.change.DALYs <- cbind(age.std.change$AgeStdDALYs.matrix.2020,age.std.change$AgeStdDALYs.matrix.2027,
                                            age.std.change$AgeStdDALYs.matrix.2036,age.std.change$AgeStdDALYs.matrix.S1,
                                            age.std.change$AgeStdDALYs.matrix.S2,age.std.change$AgeStdDALYs.matrix.S3)
colnames(format.output.abs.change.DALYs) <- c("2020","2027","2036","S1","S2","S3")

write.csv(format.output.abs.change.DALYs,file = "age standardized change of DALYs from PA.csv")
