library(pdftools)
library(tidyverse)
library(ggplot2)
library(data.table)
library(readxl)
library(purrr)
library(Cairo)
library(dplyr)
library(RColorBrewer)
library(extrafont)
library(pROC)
library(ROCit)
library(randomForest)
library(Cairo)
library(ggthemes)
library(reshape2)
library(shiny)
library(zoo)
library(scales)
library(writexl)
source('~/github/R-General/.Rprofile')


#Drawn out for Deer 25 ----
## Load spreadsheets
AHarvestRaw <- as.data.table(read_excel("Raw data/Harvest_and_public/Harvest starting data/2024 Statewide Pronghorn Harvest_Raw.xlsx", sheet="Normal"))
#AHarvestSC <- as.data.table(read_excel("Raw data/Harvest_and_public/2024 Statewide Elk Harvest_Raw.xlsx", sheet="SC"))

##Remove all the empty NA columns
AHarvestRaw[, names(AHarvestRaw)[
  sapply(AHarvestRaw, function(x) all(is.na(x) | x == FALSE | x== TRUE))
] := NULL]

#Remove the rows with headers
AHarvestRaw<-AHarvestRaw[Unit!="Unit" & !grepl("include", Unit) & !grepl("Include", Unit)]

#Add categories
##Add category numbers everytime there is a non-numeric row (i.e. the row that has "total listed at the bottom of each category)
AHarvestRaw[, groupID := cumsum(!is.na(as.numeric(Unit)) == FALSE)]

##Create the category maps in the right order, pulled from the pdf
category_map_ant<-c("All manners of take"       ,                                   
               "All Rifle Seasons"        ,                             
               "All Regular Rifle Seasons", 
               "All Late Rifle Seasons",  
               "All Archery Seasons", 
               "All Muzzleloader Seasons"   ,                      
               "All Private Land Only Seasons",  
               "All Ranching for Wildlife Seasons",                        
               "All Damage, Auction/Raffle & Misc. Hunts",        
               'Dummy')




#Map the names to the numbered categories
AHarvestRaw[, Category := category_map_ant[groupID+1]]



#remove all "Total" rows without per unit data
AHarvestFinal24<-AHarvestRaw[Unit!="Total"]

#combine doe and fawn into "Antlerless"
AHarvestFinal24$Does<-as.numeric(AHarvestFinal24$Does)
AHarvestFinal24$Fawns<-as.numeric(AHarvestFinal24$Fawns)

AHarvestFinal24[,"Antlerless":=Does + Fawns]


write_xlsx(AHarvestFinal24,'Raw data/Harvest_and_public/AntHarvest24Final.xlsx')

