# Integrating passive acoustic monitoring and point counts to assess occupancy of secretive marsh birds in Tennessee  

### [Haley Holiman](https://scholar.google.com/citations?hl=en&user=cqgF1VoAAAAJ),
### Allison C. Keever, Auriel M. V. Fournier, Abigail G. Blake-Bradshaw, David J. Hanni, Bradley S. Cohen
### Data/code DOI: 

#### Please contact the first author for questions about the code or data: Haley Holiman (haley.holiman@siu.edu or lhaleyh97@gmail.com)
__________________________________________________________________________________________________________________________________________

## Abstract:  
1. Objective 1: Single Season Occupancy analysis to compare three survey methods: Point Count (PC), Autonomous Recording Unit Survey (ARU), and an integration of both (ARU + PC)
2. PObjective 2: Occupancy analysis using ARU + PC survey method to assess variables affecting detection probability and habitat selection of marsh birds in Tennessee	

## Repository Directory

### [Scripts](./scripts): Contains code for cleaning, processing, and analyzing survey and supplemental data
* 101_Data Prep.R - create encounter histories from PC and ARU data from Part 1
* 102_Models.R - run single and dynamic occupancy models from Part 1
* 103_Figures.R - plot results of analysis from Part 1
* 201_DataPrep.R - prep for unmarked framework for habitat analysis
* 202_Models.R - dynamic occupancy models using combined data 

### [Data](./Data): Contains raw and processed data used in scripts
* pointcountdata_2023: contains point count data from marsh bird surveys, May-July 2023, Central TN sites
* Pointcountdata_2022: contains point count data from marsh bird surveys, May-July 2022, West TN sites
* allrounds_standarizedrecordingdays_2022:contains ARU data of marshbird detections from BirdNet analysis for 2022
* allrounds_standardizedrecordingdays_2023:contains ARU data of marshbird detections from BirdNet analysis for 2022
* noaacovs2022: weather data downloaded from noaa.gov
* noaacovs 2023: weather data downloaded from noaa.gov
* obsCovs_2022: observation covarites for 2022 
* Pointcountdata_2022: point count raw data from 2022 MB surveys
* pointcountdata_2023: point count raw data from 2023 MB surveys
* recordingdaycov: recording day data for each site in 2022
* recordingdaycov_23: recording day data for each site in 2023
* spatcovs_2022_ally: spatial data extracted from rasters for 2022
* spatcovs_2023_ally: spatial data extracted from rasters for 2023
* yearlysitecovs: vegetation data from 2022
* yearlysitecovs_23: vegetation data from 2023

### [Plots](./plots): Contains figures and output of results
