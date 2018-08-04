# ITHIM-Sacramento-PhaseII
This repository contains code that implements the [Integrated Transport and Health Impact Model (ITHIM)](http://www.cedar.iph.cam.ac.uk/research/modelling/ithim/) for the six-county [Sacramento Area Council of Governments (SACOG)](http://www.sacog.org) region at fine-grained spatial scale (ZCTA level).

## Source code ##

### Data preparation ###

#### 1. Population and mortality at ZCTA level: ***/01_Data Preparation/01_Population+Mortality_zcta_level.R***

Data Source: 

* Region-wide moratlity rates taken from California Department of Public Health ([CDPH](http://https://www.cdph.ca.gov/Programs/CHSI/Pages/Data-and-Statistics-.aspx)) Statistic Vitals: ***/01_Data Preparation/00_Data/01_RegionwideMortalityRate_SACOG.csv***

* Population at ZCTA level taken from [2010 Decennial Census.](https://www.census.gov/programs-surveys/decennial-census/decade.2010.html)

#### 2. Baseline traffic injury at ZCTA level: ***/01_Data Preparation/02_BaselineInjuryEstimationbyVMT.R***

Data Source: 

* Region-wide baseline traffic injury taken from Internet Statewide Integrated Traffic Records System ([SWITRS](https://www.chp.ca.gov/programs-services/services-information/switrs-internet-statewide-integrated-traffic-records-system)) and Transportation Injury Mapping System ([TIMS](https://tims.berkeley.edu)): ***/01_Data Preparation/00_Data/02_RegionWide_BaselineInjury.csv***

#### 3. Data process for [SACSIM15](http://www.sacog.org/sites/main/files/file-attachments/plnrscmte_sacog_travel_model_wkshp_27mar2014.pdf): ***/01_Data Preparation/03_DataProcess_SACSIM_PhaseII.R***

### Core functions ###

1. Physical activity module: ***/02_Core Functions/01_Functions_PA_PhaseII.R***

2. Traffic injury module: ***/02_Core Functions/02_Functions_TI_PhaseII.R***
