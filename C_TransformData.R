# This script transforms the selected Data if B_HBNextractScores.R 
# 1. Plot histograms of the data 
# 2. Inspect them visually. Non-normally distributed
# variables can be indivated in select variables to be transformed in section:
# "select Variables to be Transformed"
# Data with ceiling or floor could be binned 
# 3. Plot the histograms again
# 4. Save the binned data 

rm(list=ls(all=TRUE))
library(rcompanion)
library(Hmisc)


setwd("~/Dropbox/CMI_Behavioral_Analyses/AndreasBehavioralDataAnalysis/")
datadirectory = "/Volumes/methlab/HealthyBrainNetwork/HBN_BehavioralData/Data_Download_07_01_2018/"

# options

#filter complete cases 
filterCompleteCases = TRUE

# plot histograms for all measures? 
plotHistograms = TRUE

# Read data
Data = read.csv(paste(datadirectory,"MergedBehavioralData/SelectedData.csv",sep=""))

if (filterCompleteCases == TRUE) {
  
  # only get the complete cases
  Data = Data[complete.cases(Data),]

  }

# # Check for subjects with many missings
nMeasures = dim(Data)[2]
 
## Plot a Histograms with raw data 
 
pdf(file=paste(datadirectory,"MergedBehavioralData/HistogramsOrig.pdf",sep=""), height=14, width=12)
#quartz()
par(mfrow=c(ceiling(sqrt(nMeasures-1)), floor(sqrt(nMeasures-1))), mar=c(3,3,3,1))
 
for (m in  2:dim(Data)[2]) {
   
  dat <- as.numeric(Data[,(m)])
  minmax <- range(dat, na.rm=T)
  #n_breaks <- 20
  hist(dat, main=names(Data)[m], xlab="", xlim=minmax, las=1)
  
  }

dev.off()


## Visually inspect for skewed data (data that has floor or ceiling effects) and apply transformation 

excludeVars = c()
tobeExcludedVars = variable.names(Data[,excludeVars])

# select Variables to be Transformed ------------------------------------------------------------------
TransformVars = variable.names(Data[,c(2,3)]) # give the indices of the variable that need to be transformed
BinVars = variable.names(Data[,c(2,3)]) # give the index of the variable that need to be binned

#save(file="tobeBinExclVars.RData",list=c("tobeBinnedVars","tobeExcludedVars"))

# Tukey ladder of power transformation
for (i in 1:length(TransformVars)) {
  Data[,TransformVars[i]] = transformTukey(Data[,TransformVars[i]], plotit=FALSE)
}

## Or Binning 
my.bins <- function(var, min_size) {

 vals <-  Data[,var]
 x <- table(vals)


   cuts <- as.numeric(names(x)[1])
   size_cum <- as.numeric(x[1])
   for (i in 2:length(x)) {
     if (size_cum > min_size) {
       cuts <- c(cuts, as.numeric(names(x)[i]))
       size_cum <- 0
     } else size_cum <- size_cum + as.numeric(x[i])
   }
   
   # do binning
   binned <- Hmisc::cut2(vals, cuts)
   
   # if last group has N < min_size, add to 2nd-last group
   if (tail(table(binned), 1) < min_size) cuts <- cuts[1:(length(cuts)-1)]
   binned <- ordered(Hmisc::cut2(vals, cuts))
   
   # relabel bins
   binned_new <- binned
   levels(binned_new) <- 0:(length(levels(binned_new))-1)
   
   # save ranges of bins
   # Data_levels[var] <<- list(levels=data.frame("level"=levels(binned_new), values=levels(binned), cutpoints=cuts))

 return(binned_new)
 }

## call the binning function for each variable to be binned
for (i in 1:length(BinVars))
{
  binned_new = my.bins(BinVars[i],10)
  Data[,BinVars[i]] = binned_new

}


pdf(file=paste(datadirectory,"MergedBehavioralData/HistogramsTransformed.pdf",sep=""), height=14, width=12)
#quartz()
par(mfrow=c(ceiling(sqrt(nMeasures-1)), floor(sqrt(nMeasures-1))), mar=c(3,3,3,1))

for (m in  2:dim(Data)[2]) {
  
  dat <- as.numeric(Data[,(m)])
  minmax <- range(dat, na.rm=T)
  #n_breaks <- 20
  hist(dat, main=names(Data)[m], xlab="", xlim=minmax, las=1)
  
}
dev.off()

write.csv(Data, paste(datadirectory,"MergedBehavioralData/TransformedData.csv",sep=""),row.names = FALSE)
