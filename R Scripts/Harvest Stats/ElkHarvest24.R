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
EHarvestRaw <- as.data.table(read_excel("Raw data/Harvest_and_public/Harvest starting data/2024 Statewide Elk Harvest_Raw.xlsx", sheet="Normal"))
EHarvestSC <- as.data.table(read_excel("Raw data/Harvest_and_public/Harvest starting data/2024 Statewide Elk Harvest_Raw.xlsx", sheet="SC"))

##Remove all the empty NA columns
EHarvestRaw[, names(EHarvestRaw)[
  sapply(EHarvestRaw, function(x) all(is.na(x) | x == FALSE | x== TRUE))
] := NULL]

#Remove the rows with headers
EHarvestRaw<-EHarvestRaw[Unit!="Unit" & !grepl("include", Unit) & !grepl("Include", Unit)]

#Add categories
##Add category numbers everytime there is a non-numeric row (i.e. the row that has "total listed at the bottom of each category)
EHarvestRaw[, groupID := cumsum(!is.na(as.numeric(Unit)) == FALSE)]

##Create the category maps in the right order, pulled from the pdf
cat_cpw_map_elk<-c('2024 Elk Harvest, Hunters and Recreation Days for All Manners of Take',
               '2024 Elk Harvest, Hunters and Recreation Days for All Rifle Seasons',
               '2024 Elk Harvest, Hunters and Recreation Days for All Ranching for Wildlife Seasons',
               '2024 Elk Harvest, Hunters and Recreation Days for Early High Country Seasons',
               '2024 Elk Harvest, Hunters and Recreation Days for Late Seasons (Includes PLOs)',
               '2024 Elk Harvest, Hunters and Recreation Days for Private Land Only Seasons',
               '2024 Elk Harvest, Hunters and Recreation Days for First Rifle Seasons',
               '2024 Elk Harvest, Hunters and Recreation Days for Second Rifle Seasons',
               '2024 Elk Harvest, Hunters and Recreation Days for Third Rifle Seasons',
               '2024 Elk Harvest, Hunters and Recreation Days for Fourth Rifle Seasons',
               '2024 Elk Harvest, Hunters and Percent Success for All Rifle Seasons',
               '2024 Elk Harvest, Hunters and Percent Success for First Rifle Seasons',
               '2024 Elk Harvest, Hunters and Percent Success for Second Rifle Seasons                                                47',
               '2024 Elk Harvest, Hunters and Percent Success for Third Rifle Seasons                                                   51',
               '2024 Elk Harvest, Hunters and Percent Success for Fourth Rifle Seasons                                                 55',
               '2024 Elk Harvest, Hunters and Recreation Days for All Archery Seasons                                                  59',
               '2024 Elk Harvest, Hunters and Recreation Days for All Muzzleloader Seasons                                         64',
               '2024 Elk Harvest, Hunters and Percent Success for All Muzzleloader Seasons                                         68',
               '2024 Elk Harvest, Hunters and Recreation Days for 1st Season Limited Antlered Seasons                     72',
               '2024 Elk Harvest, Hunters and Recreation Days for 2nd Season Limited Antlered Seasons                     75',
               '2024 Elk Harvest, Hunters and Recreation Days for 3rd Season Limited Antlered Seasons                     77',
               '2024 Elk Harvest, Hunters and Recreation Days for 4th Season Limited Antlered Seasons                     79',
               '2024 Elk Harvest, Hunters and Recreation Days for 1st Season Limited Antlerless Seasons                   82',
               '2024 Elk Harvest, Hunters and Recreation Days for 2nd Season Limited Antlerless Seasons                  85',
               '2024 Elk Harvest, Hunters and Recreation Days for 2nd Split Season Limited Antlerless Seasons         89',
               '2024 Elk Harvest, Hunters and Recreation Days for 3rd Season Limited Antlerless Seasons                   90',
               '2024 Elk Harvest, Hunters and Recreation Days for 3rd Split Season Limited Antlerless Seasons         94',
               '2024 Elk Harvest, Hunters and Recreation Days for 4th Season Limited Antlerless Seasons                   95',
               '2024 Elk Harvest, Hunters and Recreation Days for 4th Split Season Limited Antlerless Seasons         99',
               '2024 Elk Harvest, Hunters and Recreation Days for 1st Season Limited Either-sex Seasons                 100',
               '2024 Elk Harvest, Hunters and Recreation Days for 2nd Season Limited Either-sex Seasons                101',
               '2024 Elk Harvest, Hunters and Recreation Days for 3rd Season Limited Either-sex Seasons                102',
               '2024 Elk Harvest, Hunters and Recreation Days for 4th Season Limited Either-sex Seasons                103',
               '2024 Elk Harvest, Hunters and Recreation Days for Early PLO Seasons Only                                          104',
               '2024 Elk Harvest, Hunters and Recreation Days for Late PLO Seasons Only                                           107',
               '2024 Elk Harvest, Hunters and Recreation Days for All Late Seasons (Does not include PLOs)           108',
               '2024 Elk Harvest, Hunters and Percent Success for All Late Seasons (Does not include PLOs)           110',
               '2024 Elk Harvest, Hunters and Recreation Days for All Bosque del Oso Seasons',
               '2024 Elk Harvest, Hunters and Recreation Days for All Damage, AFA and Auction/Raffle Hunts',
'Dummy')


category_map_elk <- c(   "All manners of take"                                                 ,
                     "All Rifle Seasons"                                                   ,
                     "All Ranching For Wildlife Seasons"                                   ,
                     "All Early High Country Seasons"                                                   ,
                     "Late Seasons (Including PLOs)"                                       ,
                     "Private Land Only Seasons"                                           ,
                     "First Rifle Seasons (All Harvest - Includes PLOs)"                   ,
                     "Second Rifle Seasons (All Harvest - Includes PLOs)"                  ,
                     "Third Rifle Seasons (All Harvest - Includes PLOs)"                   ,
                     "Fourth Rifle Seasons (All Harvest - Includes PLOs)"                  ,
                     "All Antlered Rifle Seasons"                                          ,
                     "Antlered First Rifle Seasons (No PLO or Either-sex tags included)"   ,
                     "Antlered Second Rifle Seasons (No PLO or Either-sex tags included)"  ,
                     "Antlered Third Rifle Seasons (No PLO or Either-sex tags included)"   ,
                     "Antlered Fourth Rifle Seasons (No PLO or Either-sex tags included)"  ,
                     "All Archery Seasons"                                                 ,
                     "All Muzzleloader Seasons"                                            ,
                     "Antlered Muzzleloader"                                               ,
                     "First Season Limited Antlered"                                       ,
                     "Second Season Limited Antlered"                                      ,
                     "Third Season Limited Antlered"                                       ,
                     "Fourth Season Limited Antlered"                                      ,
                     "First Season Limited Antlerless"                                     ,
                     "Second Season Limited Antlerless"                                    ,
                     "2nd Split Season Limited Antlerless"                                 ,
                     "Third Season Limited Antlerless"                                     ,
                     "3rd Split Season Limited Antlerless"                                 ,
                     "Fourth Season Limited Antlerless"                                    ,
                     "Fourth Season Split Limited Antlerless"                              ,
                     "First Season Limited Either-sex Seasons",
                     "Second Season Limited Either-sex Seasons"                            ,
                     "Third Season Limited Either-sex Seasons"                             ,
                     "Fourth Season Limited Either-sex Seasons"                            ,
                     "Early PLO Seasons"                                                   ,
                     "Late PLO Seasons"                                                    ,
                     "All Late Seasons (Not including PLOs)"                               ,
                     "Late Season Antlered (Not including PLOs)"                           ,
                     "All Bosque del Oso Seasons"                                          ,
                     "All Damage, AFA and Auction/Raffle Hunts"                            ,
                     
                   "Dummy"
                   
)   # one per block, in order


#Map the names to the numbered categories
EHarvestRaw[, CategoryFull := category_map_elk[groupID+1]]
EHarvestRaw[, CategoryCPW := cat_cpw_map_elk[groupID+1]]



#remove all "Total" rows without per unit data
EHarvestRaw<-EHarvestRaw[Unit!="Total"]

#duplicate the percent success categories so that we can split them into anterless and antlered
dup_map<- c(
  "All Antlered Rifle Seasons"="All Antlerless Rifle Seasons",                                        
  "Antlered First Rifle Seasons (No PLO or Either-sex tags included)"="Antlerless First Rifle Seasons (No PLO or Either-sex tags included)", 
  "Antlered Second Rifle Seasons (No PLO or Either-sex tags included)"="Antlerless Second Rifle Seasons (No PLO or Either-sex tags included)",
  "Antlered Third Rifle Seasons (No PLO or Either-sex tags included)"="Antlerless Third Rifle Seasons (No PLO or Either-sex tags included)", 
  "Antlered Fourth Rifle Seasons (No PLO or Either-sex tags included)"="Antlerless Fourth Rifle Seasons (No PLO or Either-sex tags included)",
  "Antlered Muzzleloader"="Antlerless Muzzleloader"                                             
)


to_dup <- EHarvestRaw[CategoryFull %in% names(dup_map)]

to_dup[, CategoryFull := dup_map[CategoryFull]]

EHarvestRaw <- rbind(EHarvestRaw, to_dup)


#Map the old columns to new columns that I use in my final version for the %success categories
#make numeric first
EHarvestRaw$Bulls<-as.numeric(EHarvestRaw$Bulls)
EHarvestRaw$Cows<-as.numeric(EHarvestRaw$Cows)
EHarvestRaw$Calves<-as.numeric(EHarvestRaw$Calves)
EHarvestRaw$`Total Harvest`<-as.numeric(EHarvestRaw$`Total Harvest`)
EHarvestRaw$`Total Rec. Days`<-as.numeric(EHarvestRaw$`Total Rec. Days`)
EHarvestRaw$`Percent Success`<-as.numeric(EHarvestRaw$`Percent Success`)
EHarvestRaw$`Total Hunters`<-as.numeric(EHarvestRaw$`Total Hunters`)




EHarvestRaw[grepl("Antlered", CategoryFull) & grepl("Percent", CategoryCPW), `:=`(
  Unit1 = Unit,
  Category = CategoryFull,
  Bulls1 = Bulls,
  Antlerless1 = 0,
  `Total Harvest1` = Bulls,
  `Total Hunters1` = Cows,
  `Percent Success1` = Calves,
  `Total Rec. Days1` = "Not Reported"
)]


EHarvestRaw[grepl("Antlerless", CategoryFull)& grepl("Percent", CategoryCPW), `:=`(
  Unit1 = Unit,
  Category = CategoryFull,
  Bulls1 = 0,
  Antlerless1 = `Total Harvest`,
  `Total Harvest1` = `Total Harvest`,
  `Total Hunters1` = `Percent Success`,
  `Percent Success1` = `Total Rec. Days`,
  `Total Rec. Days1` = "Not Reported"
)]

#Map the old columns to new columns that I use in my final version for the standard categories
EHarvestRaw[!grepl("Percent",CategoryCPW), `:=`(
  Unit1 = Unit,
  Category = CategoryFull,
  Bulls1 = Bulls,
  Antlerless1 = Cows + Calves,
  `Total Harvest1` = `Total Harvest`,
  `Total Hunters1` = `Total Hunters`,
  `Percent Success1` = `Percent Success`,
  `Total Rec. Days1` = `Total Rec. Days`
)]

#Remove the previous columns
EHarvestRaw[,(1:11):=NULL]

#Reset names to match what's on website columns
newnames<-c("Unit", "Category","Bulls",  "Antlerless","Total Harvest",  
            "Total Hunters", "Percent Success", "Total Rec. Days")
setnames(EHarvestRaw,names(EHarvestRaw),newnames)

#Add in bosque and season choice

EHarvestSC[Season=="Archery",Category:="Bosque del Oso Archery"]
EHarvestSC[Season=="Muzzleloader",Category:="Bosque del Oso Muzzleloader"]
EHarvestSC[Season=="3rd Rifle",Category:="Bosque del Oso 3rd Rifle"]
EHarvestSC[Season== "1st Rifle", Category:="Bosque del Oso 1st Rifle"]
EHarvestSC[Season== "2nd Rifle", Category:="Bosque del Oso 2nd Rifle"]
EHarvestSC[Season== "4th Rifle", Category:="Bosque del Oso 4th Rifle"]

EHarvestSC[Season%in%c("Archery", "Muzzleloader", "1st Rifle", "2nd Rifle", "3rd Rifle", "4th Rifle"), Unit:="851"]
    #EHarvestSC<-EHarvestSC[!Unit%in%c("Total", "Units", "Season") & !is.na(Unit) ]

#make numeric Season choice and Bosque
EHarvestSC$Bulls<-as.numeric(EHarvestSC$Bulls)
EHarvestSC$Cows<-as.numeric(EHarvestSC$Cows)
EHarvestSC$Calves<-as.numeric(EHarvestSC$Calves)
EHarvestSC$`Total Harvest`<-as.numeric(EHarvestSC$`Total Harvest`)
EHarvestSC$`Total Rec. Days`<-as.numeric(EHarvestSC$`Total Rec. Days`)
EHarvestSC$`Percent Success`<-as.numeric(EHarvestSC$`Percent Success`)
EHarvestSC$`Total Hunters`<-as.numeric(EHarvestSC$`Total Hunters`)

#Reomve "Total" lines
  #EHarvestSC<-EHarvestSC[!grepl("Total",Unit)]


#Remap columns to new clean columns for each category Season choice and Bosque
# EHarvestSC[Category=="All Season Choice", `:=`(
#   Unit1 = Unit,
#   Bulls1 = Bulls,
#   Antlerless1 = Cows + Calves,
#   `Total Harvest1` = `Total Harvest`,
#   `Total Hunters1` = `Total Hunters`,
#   `Percent Success1` = `Percent Success`,
#   `Total Rec. Days1` = `Total Rec. Days`
# )]
# 
# EHarvestSC[Category!="All Season Choice"  & grepl("Choice", Category), `:=`(
#   Unit1 = Unit,
#   Bucks1 = Does,
#   Antlerless1 = Fawns + `Total Harvest`,
#   `Total Harvest1` = `Total Hunters`,
#   `Total Hunters1` = "Not Reported",
#   `Percent Success1` = "Not Reported",
#   `Total Rec. Days1` = `Percent Success`
# )]
# 
# EHarvestSC[grepl("Bosque", Category), `:=`(
#   Unit1 = Unit,
#   Bucks1 = ...4,
#   Antlerless1 = ...8 + ...12,
#   `Total Harvest1` = `Total Harvest`,
#   `Total Hunters1` = `Total Hunters`,
#   `Percent Success1` = `Percent Success`,
#   `Total Rec. Days1` = `Total Rec. Days`
# )]

#remove rows with no data Season choice and Bosque
EHarvestSC[, names(EHarvestSC)[
  sapply(EHarvestSC, function(x) all(is.na(x)))
] := NULL]

# #Remove old columns Season choice and Bosque
# EHarvestSC[,(1:11):=NULL]
# #Rename columns Season choice and Bosque
# newnamesSC<-c("Category","Unit","Bucks",  "Antlerless","Total Harvest",  
#               "Total Hunters", "Percent Success", "Total Rec. Days")
# setnames(EHarvestSC,names(EHarvestSC),newnamesSC)


#Bind Season choice and Bosque back to main Harvest

EHarvestFinal24<-rbind(EHarvestSC,EHarvestRaw, use.names=TRUE, fill=TRUE)


write_xlsx(EHarvestFinal24,'Raw data/Harvest_and_public/ElkHarvest24Final.xlsx')

