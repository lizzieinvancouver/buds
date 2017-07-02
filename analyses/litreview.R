## Started 18 May 2017 ##
## Looking at which studies compare to Dan Flynn's 2015 expt ##

# housekeeping
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)

setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses") 

stud <- read.csv("output/studytype.table.csv", header=TRUE)

stud$studyID <- paste(stud$datasetID, stud$study)

man3 <- subset(stud, forcetemps.count>1 & expchill.count>1 & photoperiods.count>1)
man4 <- subset(stud, forcetemps.count>1 & expchill.count>1 & photoperiods.count>1 & latitude.count>1)
man3
man4
