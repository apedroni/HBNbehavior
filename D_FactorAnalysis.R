# This script calculates a heterogeneous correlation matrix of the transformed data coming from C_TransformData.R
# (if all data is normally distributed one could also just give the data to the following funtions)
# Estimates the optimal number of factors using a parallel analysis
# Calculates the factor analysis with this number of factors
# A Pedroni 2018

rm(list=ls(all=TRUE))
library(polycor)
library(psych)

setwd("~/Dropbox/CMI_Behavioral_Analyses/AndreasBehavioralDataAnalysis/")
datadirectory = "/Volumes/methlab/HealthyBrainNetwork/HBN_BehavioralData/Data_Download_07_01_2018/"

# options

# Read data
Data = read.csv(paste(datadirectory,"MergedBehavioralData/TransformedData.csv",sep=""))

cormat <- hetcor(Data[,-1], std.err=F, use="pairwise.complete.obs", ML=TRUE)$cor
#write.file.csv(x = cormat, f = 'CorrMat.csv')

# Identifies the number of factors using a parallel analysis
set.seed(123)
parallel = fa.parallel(cormat,
                       n.obs = dim(Data)[1],
                       fm = 'ml',
                       fa = 'fa',
                       n.iter = 50,
                       error.bars = TRUE,
                       SMC = TRUE,
                       quant = .95)

# Use the number of factors and run the FA or take a different solution 
nfact = parallel$nfact

faResult =  fa(cormat,
            nfactors = nfact,
            n.obs = dim(Data)[1],
            rotate = "varimax", # depending on the assumptions of the data
            fm = 'ml'
            )
print(faResult,sort=TRUE)
plot(faResult)
summary(faResult)
