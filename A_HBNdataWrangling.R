# Import and merging of the HBN data
# Andreas Pedroni 2018

rm(list=ls(all=TRUE))
library(data.table)
library(tidyverse)

# Options/Settings
datadirectory = "/Volumes/methlab/HealthyBrainNetwork/HBN_BehavioralData/Data_Download_07_01_2018/"

# Include the tables despite duplicate entries? (recommended to be false)
includeTablesWithDuplicateEntries = FALSE

# gather a list of all available tables and the BasisDemoFile (basic demographic information) and the consensus diagnosis table 
AllCsvFiles = list.files(path=paste(datadirectory,"assessment_data/",sep=""))
BasicDemoFile = dir(paste(datadirectory,"assessment_data/",sep=""), pattern = "*Basic_Demos")
ConsensusDxFile = dir(paste(datadirectory,"assessment_data/",sep=""), pattern = "*ConsensusDx")
AllCsvFiles = setdiff(AllCsvFiles,c(BasicDemoFile,ConsensusDxFile))


# load the basis information: Basic_Demos
basisDemo = read.csv(paste(datadirectory,"assessment_data/",BasicDemoFile,sep=""),header = T)
basisDemo = basisDemo[-1,] # remove first row, because it has double header

# load the consesus diagnosis: 9994_ConsensusDx_20181008
ConsensusDx = read.csv(paste(datadirectory,"assessment_data/",ConsensusDxFile,sep=""),header = T)
ConsensusDx = ConsensusDx[-1,] # remove first row, because it has double header

# add factor labels for the diagnosis given
# 1= No, 2= Yes, 3= Dropped out of study before diagnosis was given
levels(ConsensusDx$NoDX) = c("No","Yes","Dropped out before diagnosis was given","NA")

# Create Basic Data table and save
BasicData = merge(basisDemo,ConsensusDx[,c("Anonymized.ID",setdiff(colnames(ConsensusDx),colnames(basisDemo)))], by.x="Anonymized.ID", by.y = "Anonymized.ID",all.x = TRUE) # the setdiff part is to avoid duplicate variables
write.csv(BasicData, paste(datadirectory,"MergedBehavioralData/BasicData.csv",sep=""),row.names = FALSE)

# Loop through all the files and add the data:
TablesWithRemovedEntries = NULL
AllData = BasicData
VarNamesBasicData = names(BasicData)

for (i in 1:length(AllCsvFiles)) {  
#for (i in 1:11) {  
  TMPfile = read.csv(paste(datadirectory,"assessment_data/",AllCsvFiles[i],sep=""),header = T)
  TMPfile = TMPfile[-1,] # remove first row, because it has double header
  
  # check if a subject has multiple entries and delete one if it is the same
  uniqueIDs = as.character(unique(TMPfile$Anonymized.ID))
  if (length(uniqueIDs) < dim(TMPfile)[1]) {
    # retain only distinct rows from table
    TMPfile = TMPfile %>% distinct()
  }
  
  # check if there are still multiple entries per subject, if not merge
  if (length(uniqueIDs) == dim(TMPfile)[1]) {
    AllData =  merge(AllData,TMPfile[,c("Anonymized.ID",setdiff(colnames(TMPfile),colnames(AllData)))], by.x="Anonymized.ID", by.y = "Anonymized.ID", all.x = TRUE)
    print(paste("merging file ",i,":", AllCsvFiles[i], sep=""))
  }
  
  else {
    # if there are multiple entries it will be saved
    NumUniqueEntries = sum(!duplicated(TMPfile$Anonymized.ID))
    NumEntries = length(TMPfile$Anonymized.ID)
    TMPfile = TMPfile[!duplicated(TMPfile$Anonymized.ID),]
    print(paste("skipping file: ",AllCsvFiles[i]," with removed rows",sep=""))
    TablesWithRemovedEntries = rbind(TablesWithRemovedEntries, cbind(AllCsvFiles[i],NumUniqueEntries,NumEntries))
    
    if (includeTablesWithDuplicateEntries == TRUE) {
       AllData =  merge(AllData,TMPfile[,c("Anonymized.ID",setdiff(colnames(TMPfile),colnames(AllData)))], by.x="Anonymized.ID", by.y = "Anonymized.ID", all.x = TRUE)
       print(paste("WARNING!!! merging file ",i,":", AllCsvFiles[i]," despite duplicate entries" ,sep=""))
    }
  }
}

# Save the file with all the variables
write.csv(AllData, file = paste(datadirectory,"MergedBehavioralData/AllData.csv",sep=""),row.names = FALSE)

# Save the file with information about tables with duplicate entries
TablesWithRemovedEntries  = data.frame(TablesWithRemovedEntries)
names(TablesWithRemovedEntries) = c("Table","NumUniqueEntries", "NumEntries")
write.table(TablesWithRemovedEntries,paste(datadirectory,"MergedBehavioralData/TablesWithRemovedEntries.csv",sep=""),sep=",",row.names = F,quote = F  )