#Title###############################################
# T.VULTURE MOVE ANALYSIS. Script 00: DOWNLOAD DATA #
#Function############################################
## Downloads the movement data directly from the Movebank online repository.

#(S42 NOTE): Doesn't work with UH Proxy, Connection timeout error message. There is a chunk of code dedicated to deal with proxys (the possible cause of the problem) in the Animove Script (ref: Lesson_2_2022).

# `Move2` (New) Workflow --------------------------------------------------------------------------------
## Packages --------------------------------------------------------------------------------------------
library(move2)
library(dplyr)

# 1. Sign in into Movebank ----------------------------------------------------------------------------
movebank_store_credentials("Saul42") # Store and save in the key-ring the credentials for the login to Movebank.


## 2. Get information from a specific dataset ----------------------------------------------------------
#### Check the metadata of the study/dataset
movebank_download_study_info(study_id = 1393954358) %>% 
  print(width = Inf)

#### Check for reference data of animals, deployments and tags
movebank_download_deployment(study_id = 1393954358)


## 3. Download the movement data -----------------------------------------------------------------------
### NOTE: Downloading the whole dataset is not recommended, as it's a large amount of data that will take a
###  lot of time and might crash R.

#### Download a dataset from a specific animal
movebank_download_study(study_id = 1918503,
                        attributes = NULL,
                        sensor_type_id = "radio-transmitter",
                        #individual_id = c(1918727, 1918739))
                        individual_local_identifier = c(239,360))

#### Download a dataset from a specific animal in a defined time period
#### e.g. between "2022-03-01 00:00:00" and "2022-03-15 00:00:00".



# `Move` (Old) Workflow --------------------------------------------------------------------------------
## Packages --------------------------------------------------------------------------------------------
library(move)


## 1. Sign in into Movebank ----------------------------------------------------------------------------
credentials <- movebankLogin() # Store the username and password for login to Movebank. 
                               
## Browse the database for studies using keywords
searchMovebankStudies(x = "Cuba", login = credentials) 


## 2. Get information from a specific dataset ----------------------------------------------------------

#### Check the metadata of the study/dataset
View(getMovebankStudy(study = "Cathartes aura MPIAB Cuba",login = credentials))

#### Check for all reference data of animals, deployments and tags
View(getMovebankReferenceTable(study = "Cathartes aura MPIAB Cuba",login = credentials)[1:4,])

##### Check reduced reference data of animals 
##### (A subset of the last table, with more specific data for the tracked animals)
View(getMovebankAnimals(study = "Cathartes aura MPIAB Cuba",login = credentials)[1:4,])


## 3. Download the movement data -----------------------------------------------------------------------
### NOTE: Downloading the whole dataset is not recommended, as it's a large amount of data that will take a
###  lot of time and might crash R. 

#### Download the whole dataset (NOT RECOMMENDED UNLESS REALLY GOOD INTERNET IS AVAILABLE)
vult <- getMovebankData(study = "Cathartes aura MPIAB Cuba",login = credentials)

#### Download a dataset from a specific animal
vult9178 <- getMovebankData(study = "Cathartes aura MPIAB Cuba",login = credentials, 
                            animalName = "9178")

#### Download a dataset from a specific animal in a defined time period
#### e.g. between "2022-03-01 00:00:00" and "2022-03-15 00:00:00". Time format: 'yyyyMMddHHmmssSSS'
vult9178.cut <- getMovebankData(study = "Cathartes aura MPIAB Cuba", login = credentials, 
                               local_identifier = "Cuba-7198", 
                               timestamp_start = "20220301000000000",
                               timestamp_end = "20220315000000000")

## 4. Download the NON-LOCATION data -------------------------------------------------------------------

### Check sensors available in a specific study
getMovebankSensors(study = "Cathartes aura MPIAB Cuba", login = cred) [1:10,]

### As the sensors are listed using a code, access the list of all available sensor types on Movebank and their codes
getMovebankSensors(login = cred) [,3:5]

### Download accelerometer data as a data.frame
vult9178.acc <- getMovebankNonLocationData(study = "Cathartes aura MPIAB Cuba", sensorID = "Acceleration", # (S42 NOTE) #QSTN #It's Acceleration the name of the SensorID in the T.Vulture case?
                                           animalName = "9178", login = cred)