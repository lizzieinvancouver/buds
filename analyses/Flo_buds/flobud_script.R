
# Clear workspace
rm(list=ls()) 
options(stringsAsFactors=FALSE)
graphics.off()

# Load libraries
install.packages("dplyer")
install.packages("tidyr")
install.packages("xlsx")
library(xlsx)
library(dplyr)
library(tidyr)
library(ggplot2)

# Set Working Directory
setwd("~/Documents/git/buds/analyses/data")
flobud<-read.xlsx("Budburst Datasheet 2015-05-15.xlsx", sheetName= "Data Long New", header=TRUE)
## this takes for ever, just save it as a csv.