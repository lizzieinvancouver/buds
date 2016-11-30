## Making a Distance from Budburst to Leaf Out Script
# In Buds folder using Dan Flynn's data
# Clear workspace
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
graphics.off()

# Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)

## From Dan's Script - Pheno Budburst Analysis.R
# Analysis of bud burst experiment 2015. 

library(memisc) # for getSummary 
library(xtable)
library(scales) # for alpha
library(ggplot2)

library(caper) # for pgls
library(png) # readPNG for Fig 1

setwd("~/Documents/git/buds/analyses")

######################################################
(toload <- sort(dir("./input")[grep("Budburst Data", dir('./input'))], T)[1])

load(file.path("input", toload))
#####################################################

# Prep 

dx$spn <- as.numeric(dx$sp)
levels(dx$warm) = c(0,1); levels(dx$photo) = c(0, 1); levels(dx$site) = 1:2; levels(dx$chill) = 1:3
dx$warm <- as.numeric(dx$warm)
dx$photo <- as.numeric(dx$photo)
dx$chill <- as.numeric(dx$chill)
dx$site <- as.numeric(dx$site)

# Chill dummy variables
dx$chill1 = ifelse(dx$chill == 2, 1, 0) 
dx$chill2 = ifelse(dx$chill == 3, 1, 0) 

with(dx, table(chill1, chill2)) # all three levels in here

dxb <- dx[!is.na(dx$bday),]
dxl <- dx[!is.na(dx$lday),]

bdaymean <- t(with(dxb, tapply(bday, list(site, sp), mean, na.rm=T)))
ldaymean <- t(with(dxl, tapply(lday, list(site, sp), mean, na.rm=T)))

leafoutdays <- data.frame(bdaymean, ldaymean)

####### More from Dan's scripts - Additional Plots and Processing.R
# BB to LO distance, by treatment, colored by overall leafout

#unique(dx$sp)[is.na(match(unique(dx$spn), gotsite))] # just checking: these are indeed the sp only at HF.

sumerb <- summary(doym.b)$summary

sumerb[!is.na(match(rownames(sumerb), paste("b_chill1[", nochill, "]", sep=""))),] = NA
sumerb[!is.na(match(rownames(sumerb), paste("b_chill2[", nochill, "]", sep=""))),] = NA
sumerb[!is.na(match(rownames(sumerb), paste("b_inter_wc1[", nochill, "]", sep=""))),] = NA
sumerb[!is.na(match(rownames(sumerb), paste("b_inter_wc2[", nochill, "]", sep=""))),] = NA
sumerb[!is.na(match(rownames(sumerb), paste("b_inter_pc1[", nochill, "]", sep=""))),] = NA
sumerb[!is.na(match(rownames(sumerb), paste("b_inter_pc2[", nochill, "]", sep=""))),] = NA
sumerb[!is.na(match(rownames(sumerb), paste("b_inter_sc1[", nochill, "]", sep=""))),] = NA
sumerb[!is.na(match(rownames(sumerb), paste("b_inter_sc2[", nochill, "]", sep=""))),] = NA


sumerb[!is.na(match(rownames(sumerb), paste("b_site[", nosite, "]", sep=""))),] = NA
sumerb[!is.na(match(rownames(sumerb), paste("b_inter_ws[", nosite, "]", sep=""))),] = NA
sumerb[!is.na(match(rownames(sumerb), paste("b_inter_ps[", nosite, "]", sep=""))),] = NA
sumerb[!is.na(match(rownames(sumerb), paste("b_inter_sc1[", nosite, "]", sep=""))),] = NA
sumerb[!is.na(match(rownames(sumerb), paste("b_inter_sc2[", nosite, "]", sep=""))),] = NA
##########

lod <- bbd <- sp <- treat <- vector()
treats <- 
  c("CS0", "WS0", "CL0", "WL0", 
    "CS1", "WS1", "CL1", "WL1",
    "CS2", "WS2", "CL2", "WL2")

for(i in treats){ # i = "CL2"
  dxx <- dx[dx$treatcode == i,]
  md = tapply(dxx$lday, dxx$sp, mean, na.rm=T)
  mdbb = tapply(dxx$bday, dxx$sp, mean, na.rm=T)
  
  lod <- c(lod, md)
  bbd <- c(bbd, mdbb)
  sp <- c(sp, as.character(unique(dx$sp)))
  treat <- c(treat, rep(i, length(unique(dx$sp))))
}

lobbdist <- data.frame(sp, treat, lod, bbd)
lobbdist$dist <- lobbdist$lod - lobbdist$bbd
lobbdist$overall <- adv[match(lobbdist$sp, adv$sp), "overall"]
lobbdist$overallb <- adv[match(lobbdist$sp, adv$sp), "overallb"]

pdf(file.path(figpath, "LOBB_dist.pdf"), width = 12, height = 15)

ggplot(lobbdist, aes(overall, dist)) + 
  geom_point(aes(col = lod), cex=3, alpha=0.8) + 
  facet_wrap(~treat, ncol = 3) + 
  ylab("Distance between leafout and budburst") +
  xlab("Overall leafout day") +
  geom_smooth(method = "lm", se = F) +
  theme_bw() + scale_color_continuous(low = "dodgerblue3", high = "goldenrod1", name="Overall day \nof leafout") 

dev.off()#;system(paste("open", file.path(figpath, "LOBB_dist.pdf"), "-a/Applications/Preview.app"))

## 26 October 2016 - Cat
## Attempt to create a timeline chart and eventally use Stan to determine
## the effects of latitude and temperature on risk time
## This is take two with a new datasheet from the USNPN, follows bb.leafout.R and darewearm.R

# Clear workspace
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
graphics.off()

# Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)

# Set Working Directory
setwd("~/Documents/git/buds/analyses")
timeline<-read.csv("input/Budburst.csv",header=TRUE)
attach(timeline)

phases<-c("Budburst","Leaves")

pheno<-timeline%>%
  select(Site_ID, Genus, Species, Individual_ID, Phenophase_Description, First_Yes_DOY, First_Yes_Year, Latitude, Longitude) %>%
  unite(species, Genus, Species, sep="_") %>%
  filter(Phenophase_Description %in% phases) %>%
  filter(Site_ID == 6242) %>%
  rename(Year = First_Yes_Year) 

# Make dataframe that includes Risk for each species
y0<-pheno%>%
  filter(Year=="2015")%>%
  group_by(species, Individual_ID, Phenophase_Description)%>%
  arrange(Individual_ID) %>%
  filter(row_number()==1) %>%
  spread(Phenophase_Description, First_Yes_DOY)
y0<-na.omit(y0)
y0$Risk <- y0$Leaves - y0$Budburst
y0<-filter(y0, Risk > 0)
y0<-filter(y0, Risk < 31)

y1<-pheno%>%
  filter(Year=="2016")%>%
  group_by(species, Individual_ID, Phenophase_Description)%>%
  arrange(Individual_ID) %>%
  filter(row_number()==1) %>%
  spread(Phenophase_Description, First_Yes_DOY)
y1<-na.omit(y1)
y1$Risk <- y1$Leaves - y1$Budburst
y1<-filter(y1, Risk > 0)
y1<-filter(y1, Risk < 31)

dat<-full_join(y0,y1)

bud<- dat %>%
  select(species, Budburst) %>%
  group_by(species)%>%
  summarise_each(funs(mean), Budburst) %>%
  arrange(species)

leaves<- dat %>%
  select(species, Leaves) %>%
  group_by(species)%>%
  summarise_each(funs(mean), Leaves) %>%
  arrange(species)

basic<- full_join(bud, leaves)
basic$Risk<- basic$Leaves - basic$Budburst

ggplot((basic), aes(x=Budburst, y=species)) + geom_point(aes(x= basic$Budburst)) + 
  geom_segment(aes(y = species, yend = species, x = Budburst, xend = Leaves)) + 
  geom_point(aes(x=basic$Leaves)) + theme(legend.position="none") +
  geom_point(position = position_dodge(.5)) + geom_point(aes(col=species)) + xlab("Budburst to Leaf Out") +
  ylab("Species")
