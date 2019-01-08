# Script to extract scores with specific tags 

rm(list=ls(all=TRUE))
setwd("~/Dropbox/CMI_Behavioral_Analyses/AndreasBehavioralDataAnalysis/")
library(data.table)
library(tidyverse)
require(gdata)
datadirectory = "/Volumes/methlab/HealthyBrainNetwork/HBN_BehavioralData/Data_Download_07_01_2018/"

# read the table with the variable names for the main scores (i.e. not the single item values)
variableNameTable = read.xls ("./Ressources/Quest_Scales_Tags.xlsx", sheet = 1, header = TRUE)
variableNameTable$Identifiers = as.character(variableNameTable$Identifiers) 

AllData = read.csv(paste(datadirectory,"MergedBehavioralData/AllData.csv",sep=""))

# reduce the tags to those that are available in Alldata (the tags file needs to be updated)
availableTags = intersect(names(AllData),as.character(variableNameTable$Identifiers))

sel = NULL
for (i in 1:dim(variableNameTable)[1]) {
  if (any(as.character(variableNameTable$Identifiers[i]) %in% availableTags  ) == FALSE) {
    sel = c(sel,-i)
  }
}
variableNameTable = variableNameTable[sel,]


# Different ways in selecting the Variables:
# by Variable Name
Selection = as.character(variableNameTable$Identifiers[1]) 
# by Tag 
Selection = as.character(variableNameTable$Identifiers[as.character(variableNameTable$Main.Tag) == "Cognition"]) 

SelectedData = AllData[,c("Anonymized.ID",Selection)]
write.csv(SelectedData, paste(datadirectory,"MergedBehavioralData/SelectedData.csv",sep=""),row.names = FALSE)