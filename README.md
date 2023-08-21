# Modelling of aflatoxin produced by Aspergillus flavus using multimodel approach
\\
*Author: Pasith Prayoonrat*

*Created: May 2023*

This is the repository of the work attempted by Pasith Prayoonrat on the project regarding the modelling of aflatoxin produced by Aspergillus flavus as a dissertation for the Computational Methods in Ecology and Evolution course at Imperial College and the Centre for Agricultural and Bioscience International (CABI).
## Table of Contents
1. Code - containing R codes used during the project
2. Data - files required to run the scripts
3. Result - maps and data (mostly are images data which are too big to upload to github)
4. Sandbox - scripts of attempted code 

### In Code you shall find:
 
 Script       | Description
 ------------- | -------------
 gbif.R |  R code for gathering data from the Global Biodiversity Information database
 SDM.R | Ensemble Species Distribution Model code
 analysis.R | analysis CLIMEX output and applying linear trend
 function.R | functions used in the codes
 visualization.R | mapping visualization 

 ### In Data you shall find:

 Script       | Description
 ------------- | -------------
1970_maize.csv |  maize literature database used 
Aflatoxin_literature.csv | maize literature database gathered
DxResults_noirrigation.csv | no irrigation result file from CLIMEX
gbif_occurrence.csv | occurrences data of A.flavus gathered from Global Biodiversity Information Facility
maize_coordinates.csv | maize coordinates containing lat and lon of occurrences data found by CABI and Google Scholar
map-data.csv | data used to create mapping data 

 ### In sandbox you shall find:

  
 Script       | Description
 ------------- | -------------
Composite_map.R | used to create composite map attempt
GIS2_Practical.R | Practical on GIS
HelloWorld.class | Java tutorial
HelloWorld.java | Java tutorial
MAXENT.R | MAXENT SDM attempt
ML_methods.R | Machine learning SDM attempt
bioclim_GLM.R | Bioclim SDM attempt
model_fitting.R | model fitting with SDM attempt
overlapping_map.R | attempt to create overlapping map in R
random_code.R | random code that wasn't use in the project