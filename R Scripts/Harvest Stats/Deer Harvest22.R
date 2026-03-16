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
DHarvestRaw <- as.data.table(read_excel("Raw data/Harvest_and_public/Harvest starting data/2022 Statewide Deer Harvest_Raw.xlsx", sheet="Normal", col_types='text'))
DHarvestSC <- as.data.table(read_excel("Raw data/Harvest_and_public/Harvest starting data/2022 Statewide Deer Harvest_Raw.xlsx", sheet="SeasonChoice"))

##Remove all the empty NA columns
DHarvestRaw[, names(DHarvestRaw)[
  sapply(DHarvestRaw, function(x) all(is.na(x) | x == FALSE | x== TRUE))
] := NULL]

#Remove the rows with headers
DHarvestRaw<-DHarvestRaw[Unit!="Unit"]

#Add categories
##Add category numbers everytime there is a non-numeric row (i.e. the row that has "total listed at the bottom of each category)
DHarvestRaw[, groupID := cumsum(!is.na(as.numeric(Unit)) == FALSE)]

##Create the category maps in the right order, pulled from the pdf
category_map <- c( "All manners of take",                                
                   "All Rifle",                                          
                   "All High Country Rifle Seasons",                     
                   "All Plains Rifle Seasons (Includes PLOs)",           
                   "All Plains Repeat",
                   "All Ranching For Wildlife Seasons",                  
                   "Early and Late Seasons (Includes PLOs)",             
                   "2nd Rifle Seasons" ,                                 
                   "3rd Rifle Seasons",                                  
                   "4th Rifle Seasons",                                  
                   "All Archery Seasons",                                
                   "Either Sex Muzzleloader",                            
                   "Antlered Muzzleloader",                              
                   "All Whitetail Only Seasons",                         
                   "All Whitetail Only Archery Seasons",                 
                   "All Whitetail Only Muzzleloader Seasons",            
                   "All Whitetail Only Rifle Seasons",                   
                   "All Exurban Seasons",
                   "All Damage, Auction/Raffle & Misc. Hunts",           
                   "2nd season rifle Antlered (Does not include PLO)",   
                   "3rd season rifle Antlered (Does not include PLO)",   
                   "4th season rifle Antlered (Does not include PLO)",  
                   "Dummy"
                   
                                   )   # one per block, in order


#Map the names to the numbered categories
DHarvestRaw[, CategoryFull := category_map[groupID+1]]


#remove all "Total" rows without per unit data
DHarvestRaw<-DHarvestRaw[Unit!="Total" & Unit!="All Plains Repeat"]

#duplicate the percent success categories so that we can split them into antlerless and antlered
dup_map<- c(
  "Antlered Muzzleloader"="Antlerless Muzzleloader",                 
  "2nd season rifle Antlered (Does not include PLO)"="2nd season rifle Antlerless (Does not include PLO)",   
  "3rd season rifle Antlered (Does not include PLO)"="3rd season rifle Antlerless (Does not include PLO)",   
  "4th season rifle Antlered (Does not include PLO)"="4th  season rifle Antlerless (Does not include PLO)"
)


to_dup <- DHarvestRaw[CategoryFull %in% names(dup_map)]

to_dup[, CategoryFull := dup_map[CategoryFull]]

DHarvestRaw <- rbind(DHarvestRaw, to_dup)


#Map the old columns to new columns that I use in my final version for the %success categories
#make numeric first
DHarvestRaw$Bucks<-as.numeric(DHarvestRaw$Bucks)
DHarvestRaw$Does<-as.numeric(DHarvestRaw$Does)
DHarvestRaw$Fawns<-as.numeric(DHarvestRaw$Fawns)
DHarvestRaw$`Total Harvest`<-as.numeric(DHarvestRaw$`Total Harvest`)
DHarvestRaw$`Total Rec. Days`<-as.numeric(DHarvestRaw$`Total Rec. Days`)
DHarvestRaw$`Percent Success`<-as.numeric(DHarvestRaw$`Percent Success`)
DHarvestRaw$`Total Hunters`<-as.numeric(DHarvestRaw$`Total Hunters`)


DHarvestRaw[is.na(Bucks), Bucks:=...5]
DHarvestRaw[is.na(Fawns), Fawns:=...18]
DHarvestRaw[is.na(`Total Harvest`), `Total Harvest`:=...25]
DHarvestRaw[is.na(`Percent Success`), `Percent Success`:=...38]
DHarvestRaw[is.na(`Total Rec. Days`), `Total Rec. Days`:=...45]
DHarvestRaw[,c("...5","...18","...25","...38","...45"):=NULL]







DHarvestRaw[grepl("Antlered", CategoryFull), `:=`(
  Unit1 = Unit,
  Category = CategoryFull,
  Bucks1 = Bucks,
  Antlerless1 = 0,
  `Total Harvest1` = Bucks,
  `Total Hunters1` = "...10",
  `Percent Success1` = Fawns,
  `Total Rec. Days1` = "Not Reported"
)]


DHarvestRaw[grepl("Antlerless", CategoryFull), `:=`(
  Unit1 = Unit,
  Category = CategoryFull,
  Bucks1 = 0,
  Antlerless1 = `Total Harvest`,
  `Total Harvest1` = `Total Harvest`,
  `Total Hunters1` = `Percent Success`,
  `Percent Success1` = `Total Rec. Days`,
  `Total Rec. Days1` = "Not Reported"
)]

#Map the old columns to new columns that I use in my final version for the standard categories
DHarvestRaw[!grepl("Antlerless",CategoryFull) & !grepl("Antlered",CategoryFull), `:=`(
  Unit1 = Unit,
  Category = CategoryFull,
  Bucks1 = Bucks,
  Antlerless1 = Fawns + Does,
  `Total Harvest1` = `Total Harvest`,
  `Total Hunters1` = `Total Hunters`,
  `Percent Success1` = `Percent Success`,
  `Total Rec. Days1` = `Total Rec. Days`
)]

#Remove the previous columns
DHarvestRaw[,(1:16):=NULL]

#Reset names to match what's on website columns
newnames<-c("Unit", "Category","Bucks",  "Antlerless","Total Harvest",  
            "Total Hunters", "Percent Success", "Total Rec. Days")
setnames(DHarvestRaw,names(DHarvestRaw),newnames)

#Add in bosque and season choice

DHarvestSC[Bucks=="Archery",Category:="Season Choice Archery"]
DHarvestSC[Bucks=="Muzzleloader",Category:="Season Choice Muzzleloader"]
DHarvestSC[Bucks=="Rifle",Category:="Season Choice Rifle"]
DHarvestSC[Unit== "Archery", Category:="Bosque del Oso Archery"]
DHarvestSC[Unit== "Muzzleloader", Category:="Bosque del Oso Muzzleloader"]
DHarvestSC[Unit== "1st Rifle", Category:="Bosque del Oso 1st Rifle"]
DHarvestSC[Unit== "2nd Rifle", Category:="Bosque del Oso 2nd Rifle"]
DHarvestSC[Unit%in%c("Archery", "Muzzleloader", "1st Rifle", "2nd Rifle"), Unit:="851"]
DHarvestSC<-DHarvestSC[!Unit%in%c("Total", "Units", "Season") & !is.na(Unit) ]
DHarvestSC[is.na(Category), Category:="All Season Choice"]

#make numeric Season choice and Bosque
DHarvestSC$Bucks<-as.numeric(DHarvestSC$Bucks)
DHarvestSC$Does<-as.numeric(DHarvestSC$Does)
DHarvestSC$Fawns<-as.numeric(DHarvestSC$Fawns)
DHarvestSC$`Total Harvest`<-as.numeric(DHarvestSC$`Total Harvest`)
DHarvestSC$`Total Rec. Days`<-as.numeric(DHarvestSC$`Total Rec. Days`)
DHarvestSC$`Percent Success`<-as.numeric(DHarvestSC$`Percent Success`)
DHarvestSC$`Total Hunters`<-as.numeric(DHarvestSC$`Total Hunters`)
DHarvestSC$...22<-as.numeric(DHarvestSC$...22)
DHarvestSC$...29<-as.numeric(DHarvestSC$...29)



#Reomve "Total" lines
DHarvestSC<-DHarvestSC[!grepl("Total",Unit)&!grepl("Recreation",Unit)]


#Remap columns to new clean columns for each category Season choice and Bosque
DHarvestSC[Category=="All Season Choice"|grepl("Bosque", Category), `:=`(
  Unit1 = Unit,
  Bucks1 = Bucks,
  Antlerless1 = Fawns + Does,
  `Total Harvest1` = `Total Harvest`,
  `Total Hunters1` = `Total Hunters`,
  `Percent Success1` = `Percent Success`,
  `Total Rec. Days1` = `Total Rec. Days`
)]

DHarvestSC[Category!="All Season Choice"  & grepl("Choice", Category), `:=`(
  Unit1 = Unit,
  Bucks1 = Does,
  Antlerless1 = Fawns + `Total Harvest`,
  `Total Harvest1` = "Total Hunters",
  `Total Hunters1` = "Not Reported",
  `Percent Success1` = "Not Reported",
  `Total Rec. Days1` = "Percent Success"
)]


# DHarvestSC$...13<-as.numeric(DHarvestSC$...13)
# DHarvestSC$...19<-as.numeric(DHarvestSC$...19)
# DHarvestSC[grepl("Bosque", Category), `:=`(
#   Unit1 = Unit,
#   Bucks1 = ...7,
#   Antlerless1 = ...13 + ...19,
#   `Total Harvest1` = `Total Harvest`,
#   `Total Hunters1` = `Total Hunters`,
#   `Percent Success1` = `Percent Success`,
#   `Total Rec. Days1` = `Total Rec. Days`
# )]

#remove rows with no data Season choice and Bosque
DHarvestSC[, names(DHarvestSC)[
  sapply(DHarvestSC, function(x) all(is.na(x)))
] := NULL]

#Remove old columns Season choice and Bosque
DHarvestSC[,(1:9):=NULL]
#Rename columns Season choice and Bosque
newnamesSC<-c("Category","Unit","Bucks",  "Antlerless","Total Harvest",  
            "Total Hunters", "Percent Success", "Total Rec. Days")
setnames(DHarvestSC,names(DHarvestSC),newnamesSC)


#Bind Season choice and Bosque back to main Harvest

DHarvestFinal22<-rbind(DHarvestSC,DHarvestRaw, use.names=TRUE, fill=TRUE)


write_xlsx(DHarvestFinal22,'Raw data/Harvest_and_public/DeerHarvest22Final.xlsx')

