# This file is part of ITHIM SACOG - PhaseII.

# File: DataProcess_Population and mortality at zcta level.R
# Purpose: obtain the zcta population from census.
#          estimate the mortality by multiplying regional-wide mortality rates


# library definition
library(tidycensus)
library(tidyverse)
library(tigris)

options(tigris_use_cache = TRUE, tigris_class="sf")

setwd("/Users/Yizheng/Documents/02_Work/17_ITHIM-Phase II/06_R Scripts/GitHub/02_Data Preparation/00_Data")


################# extract the population file at zcta level from census #####################################
#install api
census_api_key("6d083b62f8fafc1bb421d71882b7a1b1f9587be5")

#input the zcta list
zcta.list <- read.csv("zcta_list_SACOG.csv")

# variable definiation
AgeGenderVariables <-c(paste0("P012000",3:9),paste0("P01200",10:25), paste0("P01200",27:49),"P0010001")

# extract the population in age-gender group from census
AgeGenderRaw <- get_decennial(geography = "zcta", variables = AgeGenderVariables, year = 2010, state = "CA", output = "wide", sumfile = "sf1")  %>%                 
  filter(GEOID %in% zcta.list$zcta)

# format the matrix
AgeGender <- transmute(AgeGenderRaw, GEOID = GEOID, AgeGender_PopDet = P0010001,
                       # estimating GBD age/gender combinations
                       male1 = rowSums(AgeGenderRaw[,3]), male2 = rowSums(AgeGenderRaw[,4:5]), male3 = rowSums(AgeGenderRaw[,6:11]),
                       male4 = rowSums(AgeGenderRaw[,12:14]), male5 = rowSums(AgeGenderRaw[,15:17]), male6 = rowSums(AgeGenderRaw[,18:21]),
                       male7 = rowSums(AgeGenderRaw[,22:23]), male8 = rowSums(AgeGenderRaw[,24:25]),
                       female1 = rowSums(AgeGenderRaw[,26]), female2 = rowSums(AgeGenderRaw[,27:28]), female3 = rowSums(AgeGenderRaw[,29:34]),
                       female4 = rowSums(AgeGenderRaw[,35:37]), female5 = rowSums(AgeGenderRaw[,38:40]), female6 = rowSums(AgeGenderRaw[,41:44]),
                       female7 = rowSums(AgeGenderRaw[,45:46]), female8 = rowSums(AgeGenderRaw[,47:48]))

# number of zctas
n.zip <- nrow(zcta.list)

# format the output file
format.ageGender <- NULL

for (i in 1:n.zip){
  
  temp <- matrix(NA,nrow = 8, ncol = 3,dimnames = list(paste0("ageCat",1:8),c("male","female","id")))
  
  #male
  temp[1,1] <- AgeGender$male1[which(AgeGender$GEOID==zcta.list[i,2])]
  temp[2,1] <- AgeGender$male2[which(AgeGender$GEOID==zcta.list[i,2])]
  temp[3,1] <- AgeGender$male3[which(AgeGender$GEOID==zcta.list[i,2])]
  temp[4,1] <- AgeGender$male4[which(AgeGender$GEOID==zcta.list[i,2])]
  temp[5,1] <- AgeGender$male5[which(AgeGender$GEOID==zcta.list[i,2])]
  temp[6,1] <- AgeGender$male6[which(AgeGender$GEOID==zcta.list[i,2])]
  temp[7,1] <- AgeGender$male7[which(AgeGender$GEOID==zcta.list[i,2])]
  temp[8,1] <- AgeGender$male8[which(AgeGender$GEOID==zcta.list[i,2])]
  
  #female
  temp[1,2] <- AgeGender$female1[which(AgeGender$GEOID==zcta.list[i,2])]
  temp[2,2] <- AgeGender$female2[which(AgeGender$GEOID==zcta.list[i,2])]
  temp[3,2] <- AgeGender$female3[which(AgeGender$GEOID==zcta.list[i,2])]
  temp[4,2] <- AgeGender$female4[which(AgeGender$GEOID==zcta.list[i,2])]
  temp[5,2] <- AgeGender$female5[which(AgeGender$GEOID==zcta.list[i,2])]
  temp[6,2] <- AgeGender$female6[which(AgeGender$GEOID==zcta.list[i,2])]
  temp[7,2] <- AgeGender$female7[which(AgeGender$GEOID==zcta.list[i,2])]
  temp[8,2] <- AgeGender$female8[which(AgeGender$GEOID==zcta.list[i,2])]
  
  #cuttingline
  temp <- rbind(temp,matrix(NA,nrow = 1,ncol=3))
  
  #zctaID
  temp[1,3] <- paste0("zcta",i)
  
  format.ageGender <- rbind(format.ageGender,temp)
}

#output the population at zcta level
write.csv(format.ageGender,file = "Population_SACOG_Baseline.csv")

################# estimate the mortality at zcta level #####################################
# 


regional.mort <- read.csv("RegionwideMortalityRate_SACOG.csv")

output.mort <- NULL

for(i in 1:n.zip){
  temp <- matrix(NA,nrow = 16, ncol = 1)
  
  #male
  temp[1,1] <- AgeGender$male1[which(AgeGender$GEOID==zcta.list[i,2])]
  temp[2,1] <- AgeGender$male2[which(AgeGender$GEOID==zcta.list[i,2])]
  temp[3,1] <- AgeGender$male3[which(AgeGender$GEOID==zcta.list[i,2])]
  temp[4,1] <- AgeGender$male4[which(AgeGender$GEOID==zcta.list[i,2])]
  temp[5,1] <- AgeGender$male5[which(AgeGender$GEOID==zcta.list[i,2])]
  temp[6,1] <- AgeGender$male6[which(AgeGender$GEOID==zcta.list[i,2])]
  temp[7,1] <- AgeGender$male7[which(AgeGender$GEOID==zcta.list[i,2])]
  temp[8,1] <- AgeGender$male8[which(AgeGender$GEOID==zcta.list[i,2])]
  
  #female
  temp[9,1] <- AgeGender$female1[which(AgeGender$GEOID==zcta.list[i,2])]
  temp[10,1] <- AgeGender$female2[which(AgeGender$GEOID==zcta.list[i,2])]
  temp[11,1] <- AgeGender$female3[which(AgeGender$GEOID==zcta.list[i,2])]
  temp[12,1] <- AgeGender$female4[which(AgeGender$GEOID==zcta.list[i,2])]
  temp[13,1] <- AgeGender$female5[which(AgeGender$GEOID==zcta.list[i,2])]
  temp[14,1] <- AgeGender$female6[which(AgeGender$GEOID==zcta.list[i,2])]
  temp[15,1] <- AgeGender$female7[which(AgeGender$GEOID==zcta.list[i,2])]
  temp[16,1] <- AgeGender$female8[which(AgeGender$GEOID==zcta.list[i,2])]
  
  temp.mort <- temp*regional.mort$Mort.Rate
  
  output.mort <- cbind(output.mort,temp.mort)
  
}

dimnames(output.mort)<-list(c(paste0("maleAgeCat",1:8),paste0("femaleAgeCat",1:8)),paste0("zcta",1:n.zip))

#output
write.csv(output.mort,file = "GBD_SACOG_AllCause.csv")
