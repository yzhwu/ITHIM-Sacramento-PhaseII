# ITHIM-Sacramento-PhaseII
Development of Fine Grained Spatial Resolution (ZCTA Level) for an Integrated HIA Tool for the Sacramento Region

## Source code ##

### Data preparation ###

#### 1. Population and mortality at ZCTA level: ***/01_Data Preparation/01_Population+Mortality_zcta_level.R***

Data Source: 

Region-wide moratlity rates taken from California Department of Public Health ([CDPH](http://https://www.cdph.ca.gov/Programs/CHSI/Pages/Data-and-Statistics-.aspx)) Statistic Vitals: 
***/01_Data Preparation/00_Data sets/01_RegionwideMortalityRate_SACOG.csv***

Population at ZCTA level taken from [2010 Decennial Census.](https://www.census.gov/programs-surveys/decennial-census/decade.2010.html)

#### 2. Baseline traffic injury at ZCTA level: ***/01_Data Preparation/02_BaselineInjuryEstimationbyVMT.R***

Data Source: 

Region-wide baseline traffic injury taken from Internet Statewide Integrated Traffic Records System ([SWITRS](https://www.chp.ca.gov/programs-services/services-information/switrs-internet-statewide-integrated-traffic-records-system)): 
***/01_Data Preparation/00_Data sets/02_RegionWide_BaselineInjury.csv***

### Core functions ###

1. Physical activity module: ***/02_Core Functions/01_Functions_PA_PhaseII.R***

2. Traffic injury module: ***/02_Core Functions/02_Functions_TI_PhaseII.R***
