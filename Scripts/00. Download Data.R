#####################################################
# T.VULTURE MOVE ANALYSIS. Script 00: DOWNLOAD DATA #
#####################################################
## Downloads the movement data directly from the Movebank online repository.

#beta (Script under testing, not intended for use)
# Note(S42): Doesn't work with UH Proxy, Connection timeout error message. There is a chunk of code dedicated to deal with proxys (the possible cause of the problem) in the Script of the Lesson 02, Animove 22.


# Packages --------------------------------------------------------------------------------------------
library(move)


# 1. Sign in into Movebank ----------------------------------------------------------------------------
credentials <- movebankLogin() # Store the username and password for login to Movebank. 
                               
## Browse the database for studies using keywords
searchMovebankStudies(x = "Cuba", login = credentials) 


# 2. Get information from a specific dataset ----------------------------------------------------------

### Check the metadata of the study/dataset
View(getMovebankStudy(study = "Cathartes aura MPIAB Cuba",login = credentials))

### Check for all reference data of animals, deployments and tags
View(getMovebankReferenceTable(study = "Cathartes aura MPIAB Cuba",login = credentials)[1:4,])

#### Check reduced reference data of animals 
#### (A subset of the last table, with more specific data for the tracked animals)
View(getMovebankAnimals(study = "Cathartes aura MPIAB Cuba",login = credentials)[1:4,])


# 3. Download the movement data -----------------------------------------------------------------------

### Download the whole dataset
vult <- getMovebankData(study = "Cathartes aura MPIAB Cuba",login = credentials)

### Download a dataset from a specific animal
vult9178 <- getMovebankData(study = "Cathartes aura MPIAB Cuba",login = credentials, 
                            animalName = "9178")

### Download a dataset from a specific animal in a defined time period
### e.g. between "2022-03-01 00:00:00" and "2022-03-15 00:00:00". Time format: 'yyyyMMddHHmmssSSS'
vult9178_1 <- getMovebankData(study = "Cathartes aura MPIAB Cuba", login = credentials, 
                              local_identifier = "Cuba-7198", 
                              timestamp_start = "20220301000000000",
                              timestamp_end = "20220315000000000")
