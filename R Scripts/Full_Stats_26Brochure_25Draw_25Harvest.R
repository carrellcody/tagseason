
#libraries to load----
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
library(randomForest)
library(Cairo)
library(ggthemes)
library(reshape2)
library(shiny)
library(zoo)
library(scales)
library(stringr)
source('~/github/R-General/.Rprofile')
#Shared Spreadsheets
PublicLandGMU<-as.data.table(read_csv("Raw data/Harvest_and_public/gmu_public_land.csv"))

setnames(PublicLandGMU,"GMUID","Unit")

highdemand<-as.data.table(read_excel("Raw data/CPW/2026 BG Hunt codes for Brochures_Manual_update.xlsx", 
                                     sheet = "High Demand"))
hybrid<-as.data.table(read_excel("Raw data/CPW/2026 BG Hunt codes for Brochures_Manual_update.xlsx", 
                                     sheet = "Hybrid26"))



## Load Deer spreadsheets----
DOADeer25 <- as.data.table(read_excel("Raw data/Drawn out at reports and exported Excel sheets/DeerDrawnOut2025.xlsx", 
                                      sheet = "Sheet1"))
DOADeer25CPW<-as.data.table(read_excel("Raw data/CPW/2025 Min Pref Points Extract final.xlsx", 
                                       sheet = "Deer"))

GMUDeer26<-as.data.table(read_excel("Raw data/CPW/2026 BG Hunt codes for Brochures_Manual_update.xlsx", 
                                    sheet = "Deer"))
#DAU, pulled from harvest sheet manually and pulled from the DAU pop estimate PDF
DeerDAUHarvest25<-as.data.table(read_excel("Raw data/DAU/2025DeerDAUFromHarvest.xlsx"))
DeerDAUPopAndMap24 <-as.data.table(read_excel("Raw data/DAU/2024 Post-Hunt Deer Population Estimates w Ratios.xlsx"))

#Drawn out results for last two years
Deerdraw23<-as.data.table(read_excel("Raw data/Drawn out at reports and exported Excel sheets/organizedDOASheets/DeerDraw23.xlsx"))
Deerdraw24<-as.data.table(read_excel("Raw data/Drawn out at reports and exported Excel sheets/organizedDOASheets/DeerDraw24.xlsx"))

#New format Harvest
DeerHarvest25<-as.data.table(read_excel("Raw data/Harvest_and_public/NewFormat/DeerHarvest25Final.xlsx"))

#Old harvest stats/format
DeerHarvest24<-as.data.table(read_excel("Raw data/Harvest_and_public/DeerHarvest24Final.xlsx"))
DeerHarvest23<-as.data.table(read_excel("Raw data/Harvest_and_public/DeerHarvest23Final.xlsx"))
DeerHarvest22<-as.data.table(read_excel("Raw data/Harvest_and_public/DeerHarvest22Final.xlsx"))

#Drawn out for Deer 25 ----
##remove any empty rows
DOADeer25<-DOADeer25[!is.na(...3)]
##change names of columns
setnames(DOADeer25,names(DOADeer25),c('Tag','List','title','DOAAdultR','DOAAdultNR','DOAYouthR','DOAYouthNR','DOALandUnRes','DOALandRes'))
##Forward fill the codes into the both rows

DOADeer25[, Tag := na.locf(Tag, na.rm = FALSE)]
DOADeer25[is.na(List),num:=1]
DOADeer25[!is.na(List),num:=2]
nam<-names(DOADeer25)
##remove the names of the variables  we're going to split wide on and any extra unnecessary column
nam<-nam[-1]
nam<-nam[-9]
##Fix the ones where the tag # was listed twice due to falling on page line. This will be specific for each year
DOADeer25<-unique(DOADeer25)
Duplicate2<-DOADeer25[num==2]
Duplicate1<-DOADeer25[num==1]

DOADeerdups2<-unique(Duplicate2[duplicated(Tag) | duplicated(Tag, fromLast=TRUE)]$Tag)
DOADeerdups1<-unique(Duplicate1[duplicated(Tag) | duplicated(Tag, fromLast=TRUE)]$Tag)

DOADeer25<-DOADeer25[(Tag==DOADeerdups1 | Tag==DOADeerdups2) & title=="Drawn Out At", num:=2]
DOADeer25<-DOADeer25[(Tag==DOADeerdups1| Tag==DOADeerdups2) & title=="# Drawn at Final Level", num:=1]

DOADeer25 <- DOADeer25[!duplicated(DOADeer25, by = c("Tag","num"))]



##Split wide using dcast
DOADeer25 <- data.table::dcast(
  DOADeer25,
  Tag ~ num,
  value.var = nam
)

DOADeer25[,List_1:=NULL]
setnames(DOADeer25, "List_2", "List")
##Split and add columns to calculate chances
# NNames<-names(DOADeer25)
# NNames<-NNames[-c(1:7)]
# NNames<-NNames[-c(31:41)]

setnames(DOADeer25, c("DOAAdultR_2",'DOAAdultNR_2',"DOAYouthR_2","DOAYouthNR_2","DOALandUnRes_2","DOALandRes_2"),
         c("A_R_drawn_out_level","A_NR_drawn_out_level","Y_R_drawn_out_level","Y_NR_drawn_out_level","L_U_drawn_out_level","L_R_drawn_out_level"))

##Split the x of y column into x and y columns and ad a ratio.
DOADeer25[, c("A_R_successful_apps_at_DOL", "A_R_total_apps_at_DOL") := tstrsplit(DOAAdultR_1, "of", fixed = TRUE)]
DOADeer25[, c("A_NR_successful_apps_at_DOL", "A_NR_total_apps_at_DOL") := tstrsplit(DOAAdultNR_1, "of", fixed = TRUE)]
DOADeer25[, c("Y_R_successful_apps_at_DOL", "Y_R_total_apps_at_DOL") := tstrsplit(DOAYouthR_1, "of", fixed = TRUE)]
DOADeer25[, c("Y_NR_successful_apps_at_DOL", "Y_NR_total_apps_at_DOL") := tstrsplit(DOAYouthNR_1, "of", fixed = TRUE)]
DOADeer25[, c("L_U_successful_apps_at_DOL", "L_U_total_apps_at_DOL") := tstrsplit(DOALandUnRes_1, "of", fixed = TRUE)]
DOADeer25[, c("L_R_successful_apps_at_DOL", "L_R_total_apps_at_DOL") := tstrsplit(DOALandRes_1, "of", fixed = TRUE)]

# Convert to numeric
DOADeer25$A_R_successful_apps_at_DOL<-as.numeric(DOADeer25$A_R_successful_apps_at_DOL)
DOADeer25$A_R_total_apps_at_DOL<-as.numeric(DOADeer25$A_R_total_apps_at_DOL)

DOADeer25$A_NR_successful_apps_at_DOL<-as.numeric(DOADeer25$A_NR_successful_apps_at_DOL)
DOADeer25$A_NR_total_apps_at_DOL<-as.numeric(DOADeer25$A_NR_total_apps_at_DOL)

DOADeer25$Y_R_successful_apps_at_DOL<-as.numeric(DOADeer25$Y_R_successful_apps_at_DOL)
DOADeer25$Y_R_total_apps_at_DOL<-as.numeric(DOADeer25$Y_R_total_apps_at_DOL)

DOADeer25$Y_NR_successful_apps_at_DOL<-as.numeric(DOADeer25$Y_NR_successful_apps_at_DOL)
DOADeer25$Y_NR_total_apps_at_DOL<-as.numeric(DOADeer25$Y_NR_total_apps_at_DOL)

DOADeer25$L_R_successful_apps_at_DOL<-as.numeric(DOADeer25$L_R_successful_apps_at_DOL)
DOADeer25$L_R_total_apps_at_DOL<-as.numeric(DOADeer25$L_R_total_apps_at_DOL)

DOADeer25$L_U_successful_apps_at_DOL<-as.numeric(DOADeer25$L_U_successful_apps_at_DOL)
DOADeer25$L_U_total_apps_at_DOL<-as.numeric(DOADeer25$L_U_total_apps_at_DOL)

# Add ratio column
DOADeer25[,"A_R_chance_at_DOL"  :=  A_R_successful_apps_at_DOL/ A_R_total_apps_at_DOL]
DOADeer25[,"A_NR_chance_at_DOL"  :=  A_NR_successful_apps_at_DOL/ A_NR_total_apps_at_DOL]
DOADeer25[,"Y_R_chance_at_DOL"  :=  Y_R_successful_apps_at_DOL/ Y_R_total_apps_at_DOL]
DOADeer25[,"Y_NR_chance_at_DOL"  :=  Y_NR_successful_apps_at_DOL/ Y_NR_total_apps_at_DOL]
DOADeer25[,"L_U_chance_at_DOL"  :=  L_U_successful_apps_at_DOL/ L_U_total_apps_at_DOL]
DOADeer25[,"L_R_chance_at_DOL"  :=  L_R_successful_apps_at_DOL/ L_R_total_apps_at_DOL]

#ratio column (Chance at DOL) should equal 100% (1) if the Drawn out level is "Leftover"
DOADeer25[A_R_drawn_out_level=="Leftover",
          A_R_chance_at_DOL:=1]
DOADeer25[A_NR_drawn_out_level=="Leftover",
          A_NR_chance_at_DOL:=1]
DOADeer25[Y_NR_drawn_out_level=="Leftover",
          Y_NR_chance_at_DOL:=1]
DOADeer25[Y_R_drawn_out_level=="Leftover",
          Y_R_chance_at_DOL:=1]
DOADeer25[L_U_drawn_out_level=="Leftover",
          L_U_chance_at_DOL:=1]
DOADeer25[L_R_drawn_out_level=="Leftover",
          L_R_chance_at_DOL:=1]


#Specify chances with first choice
##If leftover or choice 2/3/4 it's 100% chance
DOADeer25[A_R_drawn_out_level=="Leftover"|
            grepl("Choice",A_R_drawn_out_level),
          A_R_chance_with_first_choice:=1]
DOADeer25[A_NR_drawn_out_level=="Leftover"|
            grepl("Choice",A_NR_drawn_out_level),
          A_NR_chance_with_first_choice:=1]
DOADeer25[Y_NR_drawn_out_level=="Leftover"|
            grepl("Choice",Y_NR_drawn_out_level),
          Y_NR_chance_with_first_choice:=1]
DOADeer25[Y_R_drawn_out_level=="Leftover"|
            grepl("Choice",Y_R_drawn_out_level),
          Y_R_chance_with_first_choice:=1]
DOADeer25[L_U_drawn_out_level=="Leftover"|
            grepl("Choice",L_U_drawn_out_level),
          L_U_chance_with_first_choice:=1]
DOADeer25[L_R_drawn_out_level!="Leftover"|
            grepl("Choice",L_R_drawn_out_level),
          L_R_chance_with_first_choice:=1]
#if not leftover or choice 2/3/4, it's the same as the chance at drawn out level
DOADeer25[A_R_drawn_out_level!="Leftover"&
            !grepl("Choice",A_R_drawn_out_level),
          A_R_chance_with_first_choice:=A_R_chance_at_DOL]
DOADeer25[A_NR_drawn_out_level!="Leftover"&
            !grepl("Choice", A_NR_drawn_out_level),
          A_NR_chance_with_first_choice:=A_NR_chance_at_DOL]
DOADeer25[Y_NR_drawn_out_level!="Leftover"&
            !grepl("Choice",Y_NR_drawn_out_level),
          Y_NR_chance_with_first_choice:=Y_NR_chance_at_DOL]
DOADeer25[Y_R_drawn_out_level!="Leftover"&
            !grepl("Choice",Y_R_drawn_out_level),
          Y_R_chance_with_first_choice:=Y_R_chance_at_DOL]
DOADeer25[L_U_drawn_out_level!="Leftover"&
            !grepl("Choice",L_U_drawn_out_level),
          L_U_chance_with_first_choice:=L_U_chance_at_DOL]
DOADeer25[L_R_drawn_out_level!="Leftover"&
            !grepl("Choice", L_R_drawn_out_level),
          L_R_chance_with_first_choice:=L_R_chance_at_DOL]


#Go long and split out each class into rows


# longDOADeer25 <- data.table::melt(
#   DOADeer25,
#   measure.vars = patterns("_drawn_out_level$",
#                           "_successful_apps_at_DOL$"
#   #   total_apps_at_DOL         = "_total_apps_at_DOL$",
#   #   chance_at_DOL             = "_chance_at_DOL$",
#   #   chance_with_first_choice  = "_chance_with_first_choice$"
#   ),
#   value.name = c("Drawn_out_level",
#                 "Successful_apps_at_DOL"
#                  # "Total_apps_at_DOL",
#                  # "Chance_at_DOL",
#                  # "Chance_with_First_choice"
#                  ),
#   variable.name = "Class"
#  )
#
longDOADeer25 <- data.table::melt(
  DOADeer25,
  id.vars = NULL,   # keep any existing ID columns, if you have them
  measure.vars = patterns("drawn_out_level",
                          "successful_apps_at_DOL",
                          "total_apps_at_DOL",
                          "chance_at_DOL",
                          "chance_with_first_choice"),
  variable.name = "Class",
  value.name = c("Drawn_out_level",
                 "Successful_apps_at_DOL",
                 "Total_apps_at_DOL",
                 "Chance_at_DOL",
                 "Chance_with_First_choice")
)

# Now fix the ID column: map 1:6 → actual prefixes
ids <- c("A_R", "A_NR", "Y_R", "Y_NR", "L_U", "L_R")
longDOADeer25[, Class := ids[Class]]

#Remove unneccesary columns
longDOADeer25<-longDOADeer25[,(3:10) :=NULL]


# Split out the tag into "Season, weapon, sex, etc."
longDOADeer25[, `:=`(
  Animal = substr(Tag, 1, 1),   # D, A
  Sex = substr(Tag, 2, 2),   # E, B
  UnitCode = substr(Tag, 3, 5),   # 005, 123
  SeasonCode = substr(Tag, 6, 7),   # O2, X9
  Weapon = substr(Tag, 8, 8),    # R, Z
  SeasonWeapon = substr(Tag, 6, 8 ) #O2R
)]



#round off number

longDOADeer25[,c("Chance_at_DOL","Chance_with_First_choice"):= lapply(.SD, round, 2), .SDcols = c("Chance_at_DOL","Chance_with_First_choice")]

#convert to percentages

longDOADeer25$Chance_at_DOL<-percent(longDOADeer25$Chance_at_DOL)
longDOADeer25$Chance_with_First_choice<-percent(longDOADeer25$Chance_with_First_choice)

#Add column for DOA PP that's only numeric
longDOADeer25[grepl(c("Choice|Leftover"), Drawn_out_level), DOL:=0]

longDOADeer25<-longDOADeer25[is.na(DOL),DOL := as.integer(sub("^(\\d+).*", "\\1", Drawn_out_level))]

longDOADeer25<-longDOADeer25[is.na(DOL),DOL:=0]

#Split out the CPW draw results spreadsheet into long format with the same titles as the LongDOA table
setnames(DOADeer25CPW, "Hunt Code", "Tag")
longDOADeer25[DOADeer25CPW, on="Tag", `:=` (Quota = Quota, `Choice 1 Applicants`= `Choice 1 Applicants`)]


#Deer Harvest and public percentages----
PublicLandGMU[,Acres:=gmu_area_sqmi*640]
PublicLandGMU[,`Acres Public`:=public_sqmi*640]


#Add in Previous year data
DeerHarvest24[, `:=`(
  `TH23`   = NA_real_,
  `PS23` = NA_real_,
  `TH22`   = NA_real_,
  `PS22` = NA_real_
  
)]


setnames(DeerHarvest23, c("Total Hunters","Percent Success"), c("TH23", "PS23"))
setnames(DeerHarvest22, c("Total Hunters","Percent Success"), c("TH22", "PS22"))

# Join and populate
DeerHarvest24[
  DeerHarvest23[, .(
    Unit,
    Category,
    TH23,
    PS23
  )],
  on = .(Unit, Category),
  `:=`(
    TH23   = i.TH23,
    PS23 = i.PS23
  )
]

DeerHarvest24[
  DeerHarvest22[, .(
    Unit,
    Category,
    TH22,
    PS22
  )],
  on = .(Unit, Category),
  `:=`(
    TH22   = i.TH22,
    PS22 = i.PS22
  )
]

#calculate three year average for percent success
DeerHarvest24[, threeyearsuccess :=
                rowMeans(
                  cbind(PS22, PS23, `Percent Success`),
                  na.rm = TRUE
                )
]

DeerHarvest24[, threeyearshunters :=
                rowMeans(
                  cbind(TH22, TH23, `Total Hunters`),
                  na.rm = TRUE
                )
]
DeerHarvest24$threeyearshunters<-round(DeerHarvest24$threeyearshunters,0)
DeerHarvest24$threeyearsuccess<-paste0(round(DeerHarvest24$threeyearsuccess,0), "%")

#Calculate 3 year trend for percent success
yearlist<-c(1,2,3)
DeerHarvest24[, slope :=
                apply(.SD, 1, function(y) {
                  if (sum(!is.na(y)) < 2) return(NA_real_)
                  coef(lm(y ~ yearlist))[2]
                }),
              .SDcols = c("PS22", "PS23", "Percent Success")
]






DeerHarvest24[,UnitList:=Unit]
DeerHarvest24$Unit<-as.numeric(DeerHarvest24$Unit)
#Combine the publiclandGMU data
DeerHarvest24<-merge(DeerHarvest24[],PublicLandGMU,by="Unit",all=TRUE)
#make the harvestunit column
DeerHarvest24[,harvestunit:=paste(Unit,Category)]
DeerHarvest24$percent_public<-paste0(round(DeerHarvest24$percent_public,0), "%")
#calculate hunter density
DeerHarvest24[,`Hunters Density Per Sq. Mile`:=round((`Total Hunters`/gmu_area_sqmi)*1000,1)]
DeerHarvest24[`Hunters Density Per Sq. Mile`==Inf, `Hunters Density Per Sq. Mile`:=NA]


DeerHarvest24[,`Hunters Density Per Public Sq. Mile`:=round((`Total Hunters`/public_sqmi)*1000,1)]
DeerHarvest24[`Hunters Density Per Public Sq. Mile`==Inf, `Hunters Density Per Public Sq. Mile`:=NA]
DeerHarvest24[public_sqmi<1,`Hunters Density Per Public Sq. Mile`:=NA]

DeerHarvest24[, c("Acres", "Acres Public","gmu_area_sqmi","public_sqmi") := lapply(.SD, round, 0), 
              .SDcols = c("Acres", "Acres Public","gmu_area_sqmi","public_sqmi")]

DeerHarvest24[, (c("Acres", "Acres Public","gmu_area_sqmi","public_sqmi")) := lapply(.SD, function(x) 
  format(x, big.mark = ",", scientific = FALSE)), .SDcols = c("Acres", "Acres Public","gmu_area_sqmi","public_sqmi")]

DeerHarvest24[!is.na(Unit),onx:=paste("https://webmap.onxmaps.com/hunt/map#9.45/",latitude,"/",longitude, sep="")]

#Add in the tags associated with each unit




harvestcategories<- unique(DeerHarvest24$Category)





#Big Game Brochure mapping and reordering----
##Go long to wide for any duplicate Hunt Codes, consolidating the dates into one column

#List WTO based on tblsort column
GMUDeer26[, WTO := ifelse(grepl("WTO", tblsort), "White-tailed Deer Only", "")]


#remove unnecessary columns
GMUDeer26[,c('ID','spp','tblsort','row_cnt','MandatoryCWD','nr_lic_available','UNIT'):=NULL]

#remove comments from the list of units

# Extract numbers
GMUDeer26[, All_Units := trimws(regmatches(AUNIT, regexpr("(\\d+\\s*,\\s*)*\\d+", AUNIT)))]

# Extract text (everything NOT part of the number list)
GMUDeer26[, All_Units :=
            str_trim(str_extract(AUNIT, "(\\d+\\s*,\\s*)*\\d+"))
]

#remove all repeats now that the individual columns are gone
GMUDeer26<-unique(GMUDeer26)


#collapse dates from repeated units into one list of dates
GMUDeer26<-GMUDeer26[, .(DATES = paste(DATES, collapse = ",")), 
                     by = setdiff(names(GMUDeer26), "DATES")]
#combine the note columns and ignore NAs
collapse_nonempty <- function(...) {
  x <- unlist(list(...))
  x <- x[!is.na(x) & trimws(x) != ""]
  paste(x, collapse = ",")
}

GMUDeer26[, All_Notes :=
            collapse_nonempty(UnitNote, HuntcodeNote, WTO),
          by = 1:nrow(GMUDeer26)
]

#subset only the columns I want from GMUDeer26
GMUDeer26 <- GMUDeer26[
  , .(
    Tag = HUNT_CODE,
    Valid_GMUs = All_Units,
    Dates      = DATES,
    Notes      = All_Notes,
    List=LIST
  )
]

#Merge the useful columns from the GMU list into the DOA report
FullDeer25 <- longDOADeer25[
  GMUDeer26,
  on = "Tag"
]

# Combine the two list columns
FullDeer25[, List := fcoalesce(i.List, List)]

# Remove the duplicate
FullDeer25[, i.List := NULL]

#Add "New" designation
FullDeer25[is.na(Class)&!grepl("U6",Tag),New:="New"]
FullDeer25[New=="New",Drawn_out_level:="New Code - No historical draw data"]
FullDeer25[New=="New",Chance_at_DOL:="New Code - Unknown Chance"]
FullDeer25[New=="New",Notes:="New 2026 Code"]

#add in a "High-demand" and "Hybrid" column to note the high demand tags
FullDeer25[Tag%in%highdemand$'2025',HD:="High Demand"]
FullDeer25[is.na(HD),HD:="No"]

FullDeer25[Tag%in%hybrid$'CodeND',hybrid:="Hybrid"]
FullDeer25[is.na(hybrid),hybrid:="No"]






#continue filling out draw table
FullDeer25[is.na(Notes)|Notes=="No",Notes:=""]
FullDeer25[grepl("P",SeasonCode),PLO:="Yes"]
FullDeer25[!grepl("P",SeasonCode),PLO:="No"]

FullDeer25[grepl("W",SeasonCode),RFW:="Yes"]
FullDeer25[!grepl("W",SeasonCode),RFW:="No"]
FullDeer25[grepl("U",SeasonCode),OTC:="Yes"]
FullDeer25[!grepl("U",SeasonCode),OTC:="No"]
FullDeer25[Drawn_out_level=="No Apps" |Drawn_out_level== "None Drawn", NoApps:="Yes"]


#split out into long table by units

FullDeer25_Longunits <- as.data.table(FullDeer25 %>%
                                        separate_rows(Valid_GMUs, sep = ",\\s*") %>%   # split "1, 5, 3" into rows
                                        mutate(Valid_GMUs = as.integer(Valid_GMUs)))


setnames(FullDeer25_Longunits,"Valid_GMUs","Unit")

#give designation for tags you don't need to burn points on
FullDeer25_Longunits[grepl(c("Choice|Leftover"),Drawn_out_level), nopoints:="Y"]
FullDeer25_Longunits[!grepl(c("Choice|Leftover"),Drawn_out_level), nopoints:="N"]

#Add in the new format harvest data by hunt code and the DAU data by unit----

#only take the unique values from the harvest csv
DeerHarvest25_unique <- unique(DeerHarvest25, by = "Tag")

#add harvest stats to deer stats by tag
FullDeer25_Longunits_harvest <- DeerHarvest25_unique[FullDeer25_Longunits, on = "Tag"]

#DAU split long and by unit
DeerDAUPopAndMaplong <- DeerDAUPopAndMap24[, .(`GAME MANAGEMENT UNITS INVOLVED IN 2024` = unlist(strsplit(`GAME MANAGEMENT UNITS INVOLVED IN 2024`, ",\\s*"))), 
                                     by = setdiff(names(DeerDAUPopAndMap24), "GAME MANAGEMENT UNITS INVOLVED IN 2024")]
  #removing the unnecessary rows and changing to numeric
DeerDAUPopAndMaplong<-DeerDAUPopAndMaplong[!is.na(`DAU*`)&!is.na(`Herd Name`)]
setnames(DeerDAUPopAndMaplong,"GAME MANAGEMENT UNITS INVOLVED IN 2024","Unit")
DeerDAUPopAndMaplong$Unit<-as.numeric(DeerDAUPopAndMaplong$Unit)

#Mapping the DAU number and population estimates to the DOA report
FullDeer25_Longunits_harvest_dau <- DeerDAUPopAndMaplong[FullDeer25_Longunits_harvest, on = "Unit"]




##Adding in HarvestUnit to map to harvest table in java----
#Start with a single merged table, but no harvestunit yet
FullDeer25_LongunitsMerged <- copy(FullDeer25_Longunits_harvest_dau)
FullDeer25_LongunitsMerged[, harvestunit := NA_character_]

# Condition 1: Bosque del Oso - Archery
FullDeer25_LongunitsMerged[
  grepl("Bosque", Notes) & !grepl("Except", Notes) & Weapon == "A",
  harvestunit := DeerHarvest24[Category == "Bosque del Oso  - Archery"][
    .SD, on = "Unit", x.harvestunit]
]

# Bosque 1st rifle
FullDeer25_LongunitsMerged[
  grepl("Bosque", Notes) & !grepl("Except", Notes) & SeasonWeapon == "O1R",
  harvestunit := DeerHarvest24[Category == "Bosque del Oso - 1st Rifle"][
    .SD, on = "Unit", x.harvestunit]
]

# Bosque 2nd rifle
FullDeer25_LongunitsMerged[
  grepl("Bosque", Notes) & !grepl("Except", Notes) & SeasonWeapon == "O2R",
  harvestunit := DeerHarvest24[Category == "Bosque del Oso - 2nd Rifle"][
    .SD, on = "Unit", x.harvestunit]
]
#Bosque muzz
FullDeer25_LongunitsMerged[
  grepl("Bosque", Notes) & !grepl("Except", Notes) & Weapon == "M",
  harvestunit := DeerHarvest24[Category == "Bosque del Oso - Muzzleloader"][
    .SD, on = "Unit", x.harvestunit]
]


#Whitetail only rifle, archery, Muzz
FullDeer25_LongunitsMerged[grepl("White",Notes)&Weapon=="R",
                           harvestunit := DeerHarvest24[Category == "All Whitetail Only Rifle Seasons"][
                             .SD, on = "Unit", x.harvestunit]
]

FullDeer25_LongunitsMerged[grepl("White",Notes)&Weapon=="M",
                           harvestunit := DeerHarvest24[Category == "All Whitetail Only Muzzleloader Seasons"][
                             .SD, on = "Unit", x.harvestunit]
]
FullDeer25_LongunitsMerged[grepl("White",Notes)&Weapon=="A",
                           harvestunit := DeerHarvest24[Category == "All Whitetail Only Archery Seasons"][
                             .SD, on = "Unit", x.harvestunit]
]
#4th rifle antlered
FullDeer25_LongunitsMerged[is.na(harvestunit)&
                             SeasonWeapon=="O4R" & Sex=="M",
                           harvestunit := DeerHarvest24[Category == "4th season rifle Antlered (Does not include PLO)"][
                             .SD, on = "Unit", x.harvestunit]
]

FullDeer25_LongunitsMerged[is.na(harvestunit)&
                             SeasonWeapon=="O4R" & Sex=="M",
                           harvestunit := DeerHarvest24[Category == "4th Rifle Seasons"][
                             .SD, on = "Unit", x.harvestunit]
]
FullDeer25_LongunitsMerged[is.na(harvestunit)&
                             SeasonWeapon=="O4R" & Sex=="M",
                           harvestunit := DeerHarvest24[Category == "All Rifle"][
                             .SD, on = "Unit", x.harvestunit]
]


#3rd rifle antlered
FullDeer25_LongunitsMerged[is.na(harvestunit)&
                             SeasonWeapon=="O3R" & Sex=="M",
                           harvestunit := DeerHarvest24[Category == "3rd season rifle Antlered (Does not include PLO)"][
                             .SD, on = "Unit", x.harvestunit]
]

FullDeer25_LongunitsMerged[is.na(harvestunit)&
                             SeasonWeapon=="O3R" & Sex=="M",
                           harvestunit := DeerHarvest24[Category == "3rd Rifle Seasons"][
                             .SD, on = "Unit", x.harvestunit]
]
FullDeer25_LongunitsMerged[is.na(harvestunit)&
                             SeasonWeapon=="O3R" & Sex=="M",
                           harvestunit := DeerHarvest24[Category == "All Rifle"][
                             .SD, on = "Unit", x.harvestunit]
]

#2nd rifle antlered
FullDeer25_LongunitsMerged[is.na(harvestunit)&
                             SeasonWeapon=="O2R" & Sex=="M",
                           harvestunit := DeerHarvest24[Category == "2nd season rifle Antlered (Does not include PLO)"][
                             .SD, on = "Unit", x.harvestunit]
]

FullDeer25_LongunitsMerged[is.na(harvestunit)&
                             SeasonWeapon=="O2R" & (Sex=="M"|Sex=="E"),
                           harvestunit := DeerHarvest24[Category == "2nd Rifle Seasons"][
                             .SD, on = "Unit", x.harvestunit]
]
FullDeer25_LongunitsMerged[is.na(harvestunit)&
                             SeasonWeapon=="O2R" & (Sex=="M"|Sex=="E"),
                           harvestunit := DeerHarvest24[Category == "All Rifle"][
                             .SD, on = "Unit", x.harvestunit]
]

#second season rifle antlerless
FullDeer25_LongunitsMerged[is.na(harvestunit)&
                             SeasonWeapon=="O2R" & Sex=="F",
                           harvestunit := DeerHarvest24[Category == "2nd season rifle Antlerless (Does not include PLO)"][
                             .SD, on = "Unit", x.harvestunit]]

FullDeer25_LongunitsMerged[is.na(harvestunit)&
                             SeasonWeapon=="O2R" & Sex=="F",
                           harvestunit := DeerHarvest24[Category == "2nd Rifle Seasons"][
                             .SD, on = "Unit", x.harvestunit]]

FullDeer25_LongunitsMerged[is.na(harvestunit)&
                             SeasonWeapon=="O2R" & Sex=="F",
                           harvestunit := DeerHarvest24[Category == "All Rifle"][
                             .SD, on = "Unit", x.harvestunit]]

#third season rifle antlerless
FullDeer25_LongunitsMerged[is.na(harvestunit)&
                             SeasonWeapon=="O3R" & Sex=="F",
                           harvestunit := DeerHarvest24[Category == "3rd season rifle Antlerless (Does not include PLO)"][
                             .SD, on = "Unit", x.harvestunit]]

FullDeer25_LongunitsMerged[is.na(harvestunit)&
                             SeasonWeapon=="O3R" & Sex=="F",
                           harvestunit := DeerHarvest24[Category == "3rd Rifle Seasons"][
                             .SD, on = "Unit", x.harvestunit]]

FullDeer25_LongunitsMerged[is.na(harvestunit)&
                             SeasonWeapon=="O3R" & Sex=="F",
                           harvestunit := DeerHarvest24[Category == "All Rifle"][
                             .SD, on = "Unit", x.harvestunit]]

#fourth season rifle antlerless
FullDeer25_LongunitsMerged[is.na(harvestunit)&
                             SeasonWeapon=="O4R" & Sex=="F",
                           harvestunit := DeerHarvest24[Category == "4th season rifle Antlerless (Does not include PLO)"][
                             .SD, on = "Unit", x.harvestunit]]

FullDeer25_LongunitsMerged[is.na(harvestunit)&
                             SeasonWeapon=="O4R" & Sex=="F",
                           harvestunit := DeerHarvest24[Category == "4th Rifle Seasons"][
                             .SD, on = "Unit", x.harvestunit]]

FullDeer25_LongunitsMerged[is.na(harvestunit)&
                             SeasonWeapon=="O4R" & Sex=="F",
                           harvestunit := DeerHarvest24[Category == "All Rifle"][
                             .SD, on = "Unit", x.harvestunit]]


#Antlerless muzzleloader
FullDeer25_LongunitsMerged[is.na(harvestunit)&
                             Weapon=="M" & Sex=="F",
                           harvestunit := DeerHarvest24[Category == "Antlerless Muzzleloader"][
                             .SD, on = "Unit", x.harvestunit]]
FullDeer25_LongunitsMerged[is.na(harvestunit)&
                             Weapon=="M" & (Sex=="F"|Sex=="E"),
                           harvestunit := DeerHarvest24[Category == "Either Sex Muzzleloader"][
                             .SD, on = "Unit", x.harvestunit]]
FullDeer25_LongunitsMerged[is.na(harvestunit)&
                             Weapon=="M" & (Sex=="F"|Sex=="E"),
                           harvestunit := DeerHarvest24[Category == "All manners of take"][
                             .SD, on = "Unit", x.harvestunit]]
#Antlered muzzleloader
FullDeer25_LongunitsMerged[is.na(harvestunit)&
                             Weapon=="M" & Sex=="M",
                           harvestunit := DeerHarvest24[Category == "Antlered Muzzleloader"][
                             .SD, on = "Unit", x.harvestunit]]
FullDeer25_LongunitsMerged[is.na(harvestunit)&
                             Weapon=="M" & (Sex=="M"|Sex=="E"),
                           harvestunit := DeerHarvest24[Category == "Either Sex Muzzleloader"][
                             .SD, on = "Unit", x.harvestunit]]
FullDeer25_LongunitsMerged[is.na(harvestunit)&
                             Weapon=="M" & (Sex=="M"|Sex=="E"),
                           harvestunit := DeerHarvest24[Category == "All manners of take"][
                             .SD, on = "Unit", x.harvestunit]]

#RFW
FullDeer25_LongunitsMerged[is.na(harvestunit)&
                             RFW=="Yes",
                           harvestunit := DeerHarvest24[Category == "All Ranching For Wildlife Seasons"][
                             .SD, on = "Unit", x.harvestunit]]

FullDeer25_LongunitsMerged[is.na(harvestunit)&
                             RFW=="Yes",
                           harvestunit := DeerHarvest24[Category == "All manners of take"][
                             .SD, on = "Unit", x.harvestunit]]

#High Country Rifle
FullDeer25_LongunitsMerged[grepl("E",SeasonWeapon)& grepl("R",Weapon),
                           harvestunit := DeerHarvest24[Category == "All High Country Rifle Seasons"][
                             .SD, on = "Unit", x.harvestunit]]
FullDeer25_LongunitsMerged[is.na(harvestunit) & grepl("E",SeasonWeapon)& grepl("R",Weapon),
                           harvestunit := DeerHarvest24[Category == "All Rifle"][
                             .SD, on = "Unit", x.harvestunit]]

#other early tags and all late tags
FullDeer25_LongunitsMerged[is.na(harvestunit)& (grepl("L",SeasonWeapon)|grepl("E",SeasonWeapon)),
                           harvestunit := DeerHarvest24[Category == "Early and Late Seasons (Includes PLOs)"][
                             .SD, on = "Unit", x.harvestunit]]
FullDeer25_LongunitsMerged[is.na(harvestunit)& (grepl("L",SeasonWeapon)|grepl("E",SeasonWeapon)),
                           harvestunit := DeerHarvest24[Category == "All manners of take"][
                             .SD, on = "Unit", x.harvestunit]]
#All archery
FullDeer25_LongunitsMerged[Weapon=="A",
                           harvestunit := DeerHarvest24[Category == "All Archery Seasons"][
                             .SD, on = "Unit", x.harvestunit]]
FullDeer25_LongunitsMerged[is.na(harvestunit) & Weapon=="A",
                           harvestunit := DeerHarvest24[Category == "All manners of take"][
                             .SD, on = "Unit", x.harvestunit]]

#Season Choice
FullDeer25_LongunitsMerged[is.na(harvestunit) & Weapon=="X",
                           harvestunit := DeerHarvest24[Category == "All Season Choice"][
                             .SD, on = "Unit", x.harvestunit]]

#Final safety check to get all

FullDeer25_LongunitsMerged[is.na(harvestunit) & Weapon=="R",
                           harvestunit := DeerHarvest24[Category == "All Rifle"][
                             .SD, on = "Unit", x.harvestunit]]

FullDeer25_LongunitsMerged[is.na(harvestunit),
                           harvestunit := DeerHarvest24[Category == "All manners of take"][
                             .SD, on = "Unit", x.harvestunit]]


#collapse harvestunit into a list of harvest units
collapse_cols <- c("Unit","harvestunit")
group_cols <- setdiff(names(FullDeer25_LongunitsMerged), collapse_cols)

FullDeer25_wideunits <- FullDeer25_LongunitsMerged[, lapply(.SD, function(x) paste(x, collapse=",")),
                                                   by = group_cols, .SDcols = collapse_cols]

setnames(FullDeer25_wideunits,"Unit","Valid GMUs")

##Add list of tags to the harvest stats
tags_by_unit <- FullDeer25_Longunits_harvest_dau[
  , .(Tags = paste(unique(Tag), collapse=",")), 
  by = Unit
]

DeerHarvest24 <- merge(
  DeerHarvest24, 
  tags_by_unit, 
  by = "Unit", 
  all.x = TRUE
)



##Remove NA units
DeerHarvest24<-DeerHarvest24[!is.na(Unit)|!is.na(UnitList)]


#Deer add in public land. Add up the total public land in each unit----
# Step 1: Split Numbers column into separate rows
FullDeer25_Long <- as.data.table(FullDeer25_wideunits %>%
                                   separate_rows(`Valid GMUs`, sep = ",\\s*") %>%   # split "1, 5, 3" into rows
                                   mutate(`Valid GMUs` = as.integer(`Valid GMUs`)))

# Step 2: Join with df2 to get percentages
FullDeer25_Long_joined <- FullDeer25_Long %>%
  left_join(PublicLandGMU, by = c("Valid GMUs" = "Unit"))



# # Step 3: Average percentages back per ID
# FullDeer25Final <- FullDeer25_Long_joined %>%
#   group_by(Tag) %>%
#   summarise(
#     'Valid GMUs' = paste('Valid GMUs', collapse = ", "),
#     Total_Acres = sum(Acres, na.rm = TRUE),
#     Public_Acres = sum(`Acres Public`, na.rm = TRUE),
#     )

# Step 3a: Compute the summaries
summaries <- as.data.table(
  FullDeer25_Long_joined %>%
    group_by(Tag,Class) %>%
    summarise(
      # `Valid GMUs` = paste(`Valid GMUs`, collapse = ", "),
      Total_Acres = sum(Acres, na.rm = TRUE),
      Public_Acres = sum(`Acres Public`, na.rm = TRUE),
      .groups = "drop"
    )
)

# Step 3b: Join back to the original (all columns preserved)
FullDeer25Final <- FullDeer25_wideunits %>%
  left_join(summaries, by = c('Tag','Class'))
#calculate percent public land
FullDeer25Final[,Public_Percent:= paste0(round((Public_Acres / Total_Acres) * 100, 1), "%")]

FullDeer25Final[, c("Total_Acres","Public_Acres") := lapply(.SD, round, 0), .SDcols = c("Total_Acres","Public_Acres")]

#calculate hunter density
FullDeer25Final$`Total Hunters`<-as.numeric(FullDeer25Final$`Total Hunters`)
FullDeer25Final[,Hunters_per_Acre:=`Total Hunters`/Total_Acres]
FullDeer25Final[,Hunters_per_Public_Acre:=`Total Hunters`/Public_Acres]

##Add in 23 and 24 drawn out numbers----
FullDeer25Final[Deerdraw24, `:=`(
  Drawn_out_level24 = i.Drawn_out_level,
  Chance_at_DOL24   = i.Chance_at_DOL,
  DOL24 = i.DOL
), on = c("Tag","Class")]

FullDeer25Final[Deerdraw23, `:=`(
  Drawn_out_level23 = i.Drawn_out_level,
  Chance_at_DOL23   = i.Chance_at_DOL,
  DOL23 = i.DOL
), on = c("Tag","Class")]


##Add in a way to view trend of draw
#switch chance at DOL to a decimal and add it to the actual level so the drawn out level can be entirely numeric
FullDeer25Final[, Chance_at_DOLN := 1-(as.numeric(gsub("%", "", Chance_at_DOL)) / 100)]
FullDeer25Final[Drawn_out_level=="Choice 2",DOL:=-1]
FullDeer25Final[Drawn_out_level=="Choice 3",DOL:=-2]
FullDeer25Final[Drawn_out_level=="Choice 4",DOL:=-3]
FullDeer25Final[Drawn_out_level %in% "Leftover",DOL:=-4]
FullDeer25Final[,DOLE25:=DOL+Chance_at_DOLN]

FullDeer25Final[, Chance_at_DOLN24 := 1-(as.numeric(gsub("%", "", Chance_at_DOL24)) / 100)]
FullDeer25Final[Drawn_out_level24=="Choice 2",DOL24:=-1]
FullDeer25Final[Drawn_out_level24=="Choice 3",DOL24:=-2]
FullDeer25Final[Drawn_out_level24=="Choice 4",DOL24:=-3]
FullDeer25Final[Drawn_out_level24=="Leftover",DOL24:=-4]
FullDeer25Final[,DOLE24:=DOL24+Chance_at_DOLN24]

FullDeer25Final[, Chance_at_DOLN23 := 1-(as.numeric(gsub("%", "", Chance_at_DOL23)) / 100)]
FullDeer25Final[Drawn_out_level23=="Choice 2",DOL23:=-1]
FullDeer25Final[Drawn_out_level23=="Choice 3",DOL23:=-2]
FullDeer25Final[Drawn_out_level23=="Choice 4",DOL23:=-3]
FullDeer25Final[Drawn_out_level23 %in% "Leftover",DOL3:=-4]
FullDeer25Final[,DOLE23:=DOL23+Chance_at_DOLN23]

#calculate slope and predicted value
x <- 1:3

FullDeer25Final[, c("slope", "predictedDrawnOut") := {
  
  y <- c(DOLE23, DOLE24, DOLE25)
  
  if (all(is.na(y))) {
    list(NA_real_, NA_real_)
  } else {
    fit <- lm(y ~ x)
    slope <- coef(fit)[2]
    predictedDrawnOut <- coef(fit)[1] + slope * 4
    list(slope, predictedDrawnOut)
  }
  
}, by = 1:nrow(FullDeer25Final)]

FullDeer25Final[predictedDrawnOut>0, `:=`(
  PredPrefPoints = paste0(floor(predictedDrawnOut), " Preference Points"),   # integer + text
  Predpercent = 1-(predictedDrawnOut - floor(predictedDrawnOut)),                       # decimal only
  PredPrefPercent = paste0(round(((1-(predictedDrawnOut - floor(predictedDrawnOut))) * 100)), "%")  # percent string
)]

FullDeer25Final[predictedDrawnOut<=0 & predictedDrawnOut>-1, `:=`(
  PredPrefPoints = "Choice 2",  
  Predpercent = -(floor(predictedDrawnOut)- predictedDrawnOut),                       # decimal only
  PredPrefPercent = paste0(round(-(floor(predictedDrawnOut)-predictedDrawnOut) * 100), "%")  # percent string
)]

FullDeer25Final[predictedDrawnOut<=-1 & predictedDrawnOut>-2, `:=`(
  PredPrefPoints = "Choice 3",  
  Predpercent = -(floor(predictedDrawnOut)- predictedDrawnOut),                       # decimal only
  PredPrefPercent = paste0(round(-(floor(predictedDrawnOut)-predictedDrawnOut) * 100), "%")  # percent string
)]

FullDeer25Final[predictedDrawnOut<=-2 & predictedDrawnOut>-3, `:=`(
  PredPrefPoints = "Choice 4",  
  Predpercent = -(floor(predictedDrawnOut)- predictedDrawnOut),                       # decimal only
  PredPrefPercent = paste0(round(-(floor(predictedDrawnOut)-predictedDrawnOut) * 100), "%")  # percent string
)]

FullDeer25Final[predictedDrawnOut<=-3, `:=`(
  PredPrefPoints = "Leftover",  
  Predpercent = -(floor(predictedDrawnOut)- predictedDrawnOut),                       # decimal only
  PredPrefPercent = paste0(round(-(floor(predictedDrawnOut)-predictedDrawnOut) * 100), "%")  # percent string
)]

#remove OTC tags
FullDeer25Final<-FullDeer25Final[!grepl("U",SeasonWeapon)]
#remove lines with non-res codes for residents
FullDeer25Final <- FullDeer25Final[
  !(grepl("V", SeasonCode) & Class %in% c("A_R", "Y_R", "L_R", "L_U"))
]

#Create the sub table from the long DOA table----
PublicLandGMU <- unique(PublicLandGMU, by = "Unit")

PublicLandGMU$percent_public<-paste0(round(PublicLandGMU$percent_public,0), "%")


##Clean up DeerDAUPopulation
setnames(DeerDAUPopAndMaplong,"DAU*","DAU")

DeerDAUPopAndMaplong$DAU<-as.integer(DeerDAUPopAndMaplong$DAU)
##bind the PublicLandGMU to DeerDAUPopulation long and create the subtable DT
DeerDraw25Subtable<-PublicLandGMU[DeerDAUPopAndMaplong,on="Unit"]

##Clean up DeerDAUHarvest
DeerDAUHarvest25[, DAU := as.integer(sub("D-", "", DAU))]
###remove any NA columns
DeerDAUHarvest25 <- DeerDAUHarvest25[, names(DeerDAUHarvest25)[colSums(!is.na(DeerDAUHarvest25)) > 0], with = FALSE]
##bind the DAUharvest to the subtable
DeerDraw25Subtable<-DeerDAUHarvest25[DeerDraw25Subtable,on="DAU"]

#merge in the historic harvest data
DeerHarvest24OldHarv<-copy(DeerHarvest24)
setnames(DeerHarvest24OldHarv, paste0(names(DeerHarvest24OldHarv), "OldHarv"))
setnames(DeerHarvest24OldHarv,"UnitOldHarv","Unit")
DeerDraw25SubtableH<-DeerHarvest24OldHarv[DeerDraw25Subtable, on="Unit"]

##calculate hunter density
# DeerDraw25Subtable[,`Hunters Density Per Sq. Mile`:=round((`Total_Hunters_estimate`/gmu_area_sqmi)*1000,1)]
# DeerDraw25Subtable[`Hunters Density Per Sq. Mile`==Inf, `Hunters Density Per Sq. Mile`:=NA]
# 
# 
# DeerDraw25Subtable[,`Hunters Density Per Public Sq. Mile`:=round((`Total_Hunters_estimate`/public_sqmi)*1000,1)]
# DeerDraw25Subtable[`Hunters Density Per Public Sq. Mile`==Inf, `Hunters Density Per Public Sq. Mile`:=NA]
# DeerDraw25Subtable[public_sqmi<1,`Hunters Density Per Public Sq. Mile`:=NA]
# 
# DeerDraw25Subtable[, c("Acres", "Acres Public","gmu_area_sqmi","public_sqmi") := lapply(.SD, round, 0), 
#               .SDcols = c("Acres", "Acres Public","gmu_area_sqmi","public_sqmi")]

DeerDraw25Subtable[, (c("Acres", "Acres Public","gmu_area_sqmi","public_sqmi")) := lapply(.SD, function(x) 
  format(x, big.mark = ",", scientific = FALSE)), .SDcols = c("Acres", "Acres Public","gmu_area_sqmi","public_sqmi")]

DeerDraw25Subtable[!is.na(Unit),onx:=paste("https://webmap.onxmaps.com/hunt/map#9.45/",latitude,"/",longitude, sep="")]






###Split out OTC table (old harvest stats) and add OTC designation for harvest stats and subtable----
#OTC Designation
WTOnlyLateRifleOTC<-c(48, 49, 56, 57, 58, 59, 69, 84, 85, 86, 140,
                      481, 511, 512, 561, 581, 591, 691, 851, 86)

DeerHarvest24$Unit<-as.numeric(DeerHarvest24$Unit)

DeerOTCHarvest24<-copy(DeerHarvest24)

DeerOTCHarvest24[Category=="All Whitetail Only Rifle Seasons" & Unit%in%WTOnlyLateRifleOTC,
              OTCCat:="Whitetail Only Late Rifle Season"]

DeerOTCHarvest24[OTCCat=="Whitetail Only Late Rifle Season",OTC:="WTOLR"]

DeerOTCHarvest24<-DeerOTCHarvest24
#now add the OTC category to the Subtable
DeerDraw25Subtable[Unit%in%WTOnlyLateRifleOTC,OTCCat:="Whitetail Only Late Rifle Season"]

#write CSVs----

write_csv(FullDeer25Final, file='Website/FullDeer26FinalNewHarv.csv')
write_csv(DeerHarvest24,'Website/DeerHarvest24.csv')
write_csv(DeerOTCHarvest24,'Website/DeerOTC24.csv')
write_csv(DeerDraw25Subtable, file='Website/DeerDraw25Subtable.csv')



####Elk Stats----

DOAelk25 <- as.data.table(read_excel("Raw data/Drawn out at reports and exported Excel sheets/elkDrawnOut2025.xlsx"))

DOAelk25CPW<-as.data.table(read_excel("Raw data/CPW/2025 Min Pref Points Extract final.xlsx", 
                                      sheet = "Elk"))

GMUElk26<-as.data.table(read_excel("Raw data/CPW/2026 BG Hunt codes for Brochures_Manual_update.xlsx", 
                                   sheet = "ELK"))

elkHarvest25<-as.data.table(read_excel("Raw data/Harvest_and_public/ElkHarvest24Final.xlsx"))
elkHarvest22<-as.data.table(read_excel("Raw data/Harvest_and_public/ElkHarvest22Final.xlsx"))
elkHarvest23<-as.data.table(read_excel("Raw data/Harvest_and_public/ElkHarvest23Final.xlsx"))


Elkdraw23<-as.data.table(read_excel("Raw data/Drawn out at reports and exported Excel sheets/organizedDOASheets/ElkDraw23.xlsx"))
Elkdraw24<-as.data.table(read_excel("Raw data/Drawn out at reports and exported Excel sheets/organizedDOASheets/ElkDraw24.xlsx"))




##remove any empty rows
DOAelk25<-DOAelk25[!is.na(...3)]
##change names of columns
setnames(DOAelk25,names(DOAelk25),c('Tag','List','title','DOAAdultR','DOAAdultNR','DOAYouthR','DOAYouthNR','DOALandUnRes','DOALandRes'))

##Forward fill the codes into the both rows

DOAelk25[, Tag := na.locf(Tag, na.rm = FALSE)]
DOAelk25[is.na(List),num:=1]
DOAelk25[!is.na(List),num:=2]
nam<-names(DOAelk25)
##remove the names of the variables  we're going to split wide on and any extra unnecessary column
nam<-nam[-1]

nam<-nam[-9]
##Fix the ones where the tag # was listed twice due to falling on page line. This will be specific for each year
DOAelk25<-unique(DOAelk25)
Duplicate2<-DOAelk25[num==2]
Duplicate1<-DOAelk25[num==1]

DOAelkdups2<-unique(Duplicate2[duplicated(Tag) | duplicated(Tag, fromLast=TRUE)]$Tag)
DOAelkdups1<-unique(Duplicate1[duplicated(Tag) | duplicated(Tag, fromLast=TRUE)]$Tag)

DOAelk25<-DOAelk25[(Tag==DOAelkdups1 | Tag==DOAelkdups2) & title=="Drawn Out At", num:=2]
DOAelk25<-DOAelk25[(Tag==DOAelkdups1| Tag==DOAelkdups2) & title=="# Drawn at Final Level", num:=1]

DOAelk25 <- DOAelk25[!duplicated(DOAelk25, by = c("Tag","num"))]



##Split wide using dcast
DOAelk25 <- data.table::dcast(
  DOAelk25,
  Tag ~ num,
  value.var = nam
)
DOAelk25[,List_1:=NULL]
setnames(DOAelk25, "List_2", "List")
##Split and add columns to calculate chances
# NNames<-names(DOAelk25)
# NNames<-NNames[-c(1:7)]
# NNames<-NNames[-c(31:41)]

setnames(DOAelk25, c("DOAAdultR_2",'DOAAdultNR_2',"DOAYouthR_2","DOAYouthNR_2","DOALandUnRes_2","DOALandRes_2"),
         c("A_R_drawn_out_level","A_NR_drawn_out_level","Y_R_drawn_out_level","Y_NR_drawn_out_level","L_U_drawn_out_level","L_R_drawn_out_level"))

##Split the x of y column into x and y columns and ad a ratio.
DOAelk25[, c("A_R_successful_apps_at_DOL", "A_R_total_apps_at_DOL") := tstrsplit(DOAAdultR_1, "of", fixed = TRUE)]
DOAelk25[, c("A_NR_successful_apps_at_DOL", "A_NR_total_apps_at_DOL") := tstrsplit(DOAAdultNR_1, "of", fixed = TRUE)]
DOAelk25[, c("Y_R_successful_apps_at_DOL", "Y_R_total_apps_at_DOL") := tstrsplit(DOAYouthR_1, "of", fixed = TRUE)]
DOAelk25[, c("Y_NR_successful_apps_at_DOL", "Y_NR_total_apps_at_DOL") := tstrsplit(DOAYouthNR_1, "of", fixed = TRUE)]
DOAelk25[, c("L_U_successful_apps_at_DOL", "L_U_total_apps_at_DOL") := tstrsplit(DOALandUnRes_1, "of", fixed = TRUE)]
DOAelk25[, c("L_R_successful_apps_at_DOL", "L_R_total_apps_at_DOL") := tstrsplit(DOALandRes_1, "of", fixed = TRUE)]

# Convert to numeric
DOAelk25$A_R_successful_apps_at_DOL<-as.numeric(DOAelk25$A_R_successful_apps_at_DOL)
DOAelk25$A_R_total_apps_at_DOL<-as.numeric(DOAelk25$A_R_total_apps_at_DOL)

DOAelk25$A_NR_successful_apps_at_DOL<-as.numeric(DOAelk25$A_NR_successful_apps_at_DOL)
DOAelk25$A_NR_total_apps_at_DOL<-as.numeric(DOAelk25$A_NR_total_apps_at_DOL)

DOAelk25$Y_R_successful_apps_at_DOL<-as.numeric(DOAelk25$Y_R_successful_apps_at_DOL)
DOAelk25$Y_R_total_apps_at_DOL<-as.numeric(DOAelk25$Y_R_total_apps_at_DOL)

DOAelk25$Y_NR_successful_apps_at_DOL<-as.numeric(DOAelk25$Y_NR_successful_apps_at_DOL)
DOAelk25$Y_NR_total_apps_at_DOL<-as.numeric(DOAelk25$Y_NR_total_apps_at_DOL)

DOAelk25$L_R_successful_apps_at_DOL<-as.numeric(DOAelk25$L_R_successful_apps_at_DOL)
DOAelk25$L_R_total_apps_at_DOL<-as.numeric(DOAelk25$L_R_total_apps_at_DOL)

DOAelk25$L_U_successful_apps_at_DOL<-as.numeric(DOAelk25$L_U_successful_apps_at_DOL)
DOAelk25$L_U_total_apps_at_DOL<-as.numeric(DOAelk25$L_U_total_apps_at_DOL)

# Add ratio column
DOAelk25[,"A_R_chance_at_DOL"  :=  A_R_successful_apps_at_DOL/ A_R_total_apps_at_DOL]
DOAelk25[,"A_NR_chance_at_DOL"  :=  A_NR_successful_apps_at_DOL/ A_NR_total_apps_at_DOL]
DOAelk25[,"Y_R_chance_at_DOL"  :=  Y_R_successful_apps_at_DOL/ Y_R_total_apps_at_DOL]
DOAelk25[,"Y_NR_chance_at_DOL"  :=  Y_NR_successful_apps_at_DOL/ Y_NR_total_apps_at_DOL]
DOAelk25[,"L_U_chance_at_DOL"  :=  L_U_successful_apps_at_DOL/ L_U_total_apps_at_DOL]
DOAelk25[,"L_R_chance_at_DOL"  :=  L_R_successful_apps_at_DOL/ L_R_total_apps_at_DOL]

#ratio column (Chance at DOL) should equal 100% (1) if the Drawn out level is "Leftover"
DOAelk25[A_R_drawn_out_level=="Leftover",
         A_R_chance_at_DOL:=1]
DOAelk25[A_NR_drawn_out_level=="Leftover",
         A_NR_chance_at_DOL:=1]
DOAelk25[Y_NR_drawn_out_level=="Leftover",
         Y_NR_chance_at_DOL:=1]
DOAelk25[Y_R_drawn_out_level=="Leftover",
         Y_R_chance_at_DOL:=1]
DOAelk25[L_U_drawn_out_level=="Leftover",
         L_U_chance_at_DOL:=1]
DOAelk25[L_R_drawn_out_level=="Leftover",
         L_R_chance_at_DOL:=1]


#Specify chances with first choice
##If leftover or choice 2/3/4 it's 100% chance
DOAelk25[A_R_drawn_out_level=="Leftover"|
           grepl("Choice",A_R_drawn_out_level),
         A_R_chance_with_first_choice:=1]
DOAelk25[A_NR_drawn_out_level=="Leftover"|
           grepl("Choice",A_NR_drawn_out_level),
         A_NR_chance_with_first_choice:=1]
DOAelk25[Y_NR_drawn_out_level=="Leftover"|
           grepl("Choice",Y_NR_drawn_out_level),
         Y_NR_chance_with_first_choice:=1]
DOAelk25[Y_R_drawn_out_level=="Leftover"|
           grepl("Choice",Y_R_drawn_out_level),
         Y_R_chance_with_first_choice:=1]
DOAelk25[L_U_drawn_out_level=="Leftover"|
           grepl("Choice",L_U_drawn_out_level),
         L_U_chance_with_first_choice:=1]
DOAelk25[L_R_drawn_out_level!="Leftover"|
           grepl("Choice",L_R_drawn_out_level),
         L_R_chance_with_first_choice:=1]
#if not leftover or choice 2/3/4, it's the same as the chance at drawn out level
DOAelk25[A_R_drawn_out_level!="Leftover"|
           !grepl("Choice",A_R_drawn_out_level),
         A_R_chance_with_first_choice:=A_R_chance_at_DOL]
DOAelk25[A_NR_drawn_out_level!="Leftover"|
           !grepl("Choice",A_NR_drawn_out_level),
         A_NR_chance_with_first_choice:=A_NR_chance_at_DOL]
DOAelk25[Y_NR_drawn_out_level!="Leftover"|
           !grepl("Choice",Y_NR_drawn_out_level),
         Y_NR_chance_with_first_choice:=Y_NR_chance_at_DOL]
DOAelk25[Y_R_drawn_out_level!="Leftover"|
           !grepl("Choice",Y_R_drawn_out_level),
         Y_R_chance_with_first_choice:=Y_R_chance_at_DOL]
DOAelk25[L_U_drawn_out_level!="Leftover"|
           !grepl("Choice",L_U_drawn_out_level),
         L_U_chance_with_first_choice:=L_U_chance_at_DOL]
DOAelk25[L_R_drawn_out_level!="Leftover"|
           !grepl("Choice",L_R_drawn_out_level),
         L_R_chance_with_first_choice:=L_R_chance_at_DOL]


#Go long and split out each class into rows


# longDOAelk25 <- data.table::melt(
#   DOAelk25,
#   measure.vars = patterns("_drawn_out_level$",
#                           "_successful_apps_at_DOL$"
#   #   total_apps_at_DOL         = "_total_apps_at_DOL$",
#   #   chance_at_DOL             = "_chance_at_DOL$",
#   #   chance_with_first_choice  = "_chance_with_first_choice$"
#   ),
#   value.name = c("Drawn_out_level",
#                 "Successful_apps_at_DOL"
#                  # "Total_apps_at_DOL",
#                  # "Chance_at_DOL",
#                  # "Chance_with_First_choice"
#                  ),
#   variable.name = "Class"
#  )
#
longDOAelk25 <- data.table::melt(
  DOAelk25,
  id.vars = NULL,   # keep any existing ID columns, if you have them
  measure.vars = patterns("drawn_out_level",
                          "successful_apps_at_DOL",
                          "total_apps_at_DOL",
                          "chance_at_DOL",
                          "chance_with_first_choice"),
  variable.name = "Class",
  value.name = c("Drawn_out_level",
                 "Successful_apps_at_DOL",
                 "Total_apps_at_DOL",
                 "Chance_at_DOL",
                 "Chance_with_First_choice")
)

# Now fix the ID column: map 1:6 → actual prefixes
ids <- c("A_R", "A_NR", "Y_R", "Y_NR", "L_U", "L_R")
longDOAelk25[, Class := ids[Class]]

#Remove unneccesary columns
longDOAelk25<-longDOAelk25[,(3:10) :=NULL]


# Split out the tag into "Season, weapon, sex, etc."
longDOAelk25[, `:=`(
  Animal = substr(Tag, 1, 1),   # D, A
  Sex = substr(Tag, 2, 2),   # E, B
  UnitCode = substr(Tag, 3, 5),   # 005, 123
  SeasonCode = substr(Tag, 6, 7),   # O2, X9
  Weapon = substr(Tag, 8, 8),    # R, Z
  SeasonWeapon = substr(Tag, 6, 8 ) #O2R
)]



#round off number

longDOAelk25[,c("Chance_at_DOL","Chance_with_First_choice"):= lapply(.SD, round, 2), .SDcols = c("Chance_at_DOL","Chance_with_First_choice")]

#convert to percentages

longDOAelk25$Chance_at_DOL<-percent(longDOAelk25$Chance_at_DOL)
longDOAelk25$Chance_with_First_choice<-percent(longDOAelk25$Chance_with_First_choice)

#Add column for DOA PP that's only numeric
longDOAelk25[grepl(c("Choice|Leftover"), Drawn_out_level), DOL:=0]

longDOAelk25<-longDOAelk25[is.na(DOL),DOL := as.integer(sub("^(\\d+).*", "\\1", Drawn_out_level))]

longDOAelk25<-longDOAelk25[is.na(DOL),DOL:=0]

#Split out the CPW draw results spreadsheet into long format with the same titles as the LongDOA table
setnames(DOAelk25CPW, "Hunt Code", "Tag")
longDOAelk25[DOAelk25CPW, on="Tag", `:=` (Quota = Quota, `Choice 1 Applicants`= `Choice 1 Applicants`)]


#elk Harvest----
PublicLandGMU[,Acres:=gmu_area_sqmi*640]
PublicLandGMU[,`Acres Public`:=public_sqmi*640]

#Add in Previous year data
elkHarvest25[, `:=`(
  `TH23`   = NA_real_,
  `PS23` = NA_real_,
  `TH22`   = NA_real_,
  `PS22` = NA_real_
  
)]


setnames(elkHarvest23, c("Total Hunters","Percent Success"), c("TH23", "PS23"))
setnames(elkHarvest22, c("Total Hunters","Percent Success"), c("TH22", "PS22"))

# Join and populate
elkHarvest25[
  elkHarvest23[, .(
    Unit,
    Category,
    TH23,
    PS23
  )],
  on = .(Unit, Category),
  `:=`(
    TH23   = i.TH23,
    PS23 = i.PS23
  )
]

elkHarvest25[
  elkHarvest22[, .(
    Unit,
    Category,
    TH22,
    PS22
  )],
  on = .(Unit, Category),
  `:=`(
    TH22   = i.TH22,
    PS22 = i.PS22
  )
]

#calculate three year average for percent success
elkHarvest25[, threeyearsuccess :=
                rowMeans(
                  cbind(PS22, PS23, `Percent Success`),
                  na.rm = TRUE
                )
]

elkHarvest25[, threeyearshunters :=
                rowMeans(
                  cbind(TH22, TH23, `Total Hunters`),
                  na.rm = TRUE
                )
]
elkHarvest25$threeyearshunters<-round(elkHarvest25$threeyearshunters,0)
elkHarvest25$threeyearsuccess<-paste0(round(elkHarvest25$threeyearsuccess,0), "%")
#Calculate 3 year trend for percent success
yearlist<-c(1,2,3)
elkHarvest25[, slope :=
                apply(.SD, 1, function(y) {
                  if (sum(!is.na(y)) < 2) return(NA_real_)
                  coef(lm(y ~ yearlist))[2]
                }),
              .SDcols = c("PS22", "PS23", "Percent Success")
]


#Harvest table updates
elkHarvest25$Unit<-as.numeric(elkHarvest25$Unit)
elkHarvest25<-merge(elkHarvest25,PublicLandGMU,by="Unit",all=TRUE)
elkHarvest25[,harvestunit:=paste(Unit,Category)]
elkHarvest25$percent_public<-paste0(round(elkHarvest25$percent_public,0), "%")



elkHarvest25[,`Hunters Density Per Sq. Mile`:=round((`Total Hunters`/gmu_area_sqmi)*1000,1)]
elkHarvest25[`Hunters Density Per Sq. Mile`==Inf, `Hunters Density Per Sq. Mile`:=NA]


elkHarvest25[,`Hunters Density Per Public Sq. Mile`:=round((`Total Hunters`/public_sqmi)*1000,1)]
elkHarvest25[`Hunters Density Per Public Sq. Mile`==Inf, `Hunters Density Per Public Sq. Mile`:=NA]
elkHarvest25[public_sqmi<1,`Hunters Density Per Public Sq. Mile`:=NA]

elkHarvest25[, c("Acres", "Acres Public","gmu_area_sqmi","public_sqmi") := lapply(.SD, round, 0), 
             .SDcols = c("Acres", "Acres Public","gmu_area_sqmi","public_sqmi")]

elkHarvest25[, (c("Acres", "Acres Public","gmu_area_sqmi","public_sqmi")) := lapply(.SD, function(x) 
  format(x, big.mark = ",", scientific = FALSE)), .SDcols = c("Acres", "Acres Public","gmu_area_sqmi","public_sqmi")]

elkHarvest25[,onx:=paste("https://webmap.onxmaps.com/hunt/map#9.45/",latitude,"/",longitude, sep="")]

#Add in the tags associated with each unit




elkharvestcategories<- unique(elkHarvest25$Category)





#elk Combination----
#Big Game Brochure mapping and reordering----
##Go long to wide for any duplicate Hunt Codes, consolidating the dates into one column

#remove unnecessary columns
GMUElk26[,c('ID','spp','tblsort','row_cnt','MandatoryCWD','nr_lic_available','UNIT','...17'):=NULL]

#remove comments from the list of units

# Extract numbers
GMUElk26[, All_Units := trimws(regmatches(AUNIT, regexpr("(\\d+\\s*,\\s*)*\\d+", AUNIT)))]

# Extract text (everything NOT part of the number list)
GMUElk26[, Text := trimws(gsub("(\\d+\\s*,\\s*)*\\d+", "", AUNIT))]

#remove all repeats now that the individual columns are gone
GMUElk26<-unique(GMUElk26)


#collapse dates from repeated units into one list of dates
GMUElk26<-GMUElk26[, .(DATES = paste(DATES, collapse = ",")), 
                   by = setdiff(names(GMUElk26), "DATES")]
#combine the note columns and ignore NAs
collapse_nonempty <- function(...) {
  x <- unlist(list(...))
  x <- x[!is.na(x) & trimws(x) != ""]
  paste(x, collapse = ",")
}

GMUElk26[, All_Notes :=
           collapse_nonempty(UnitNote, HuntcodeNote),
         by = 1:nrow(GMUElk26)
]

#subset only the columns I want from GMUDeer26
GMUElk26 <- GMUElk26[
  , .(
    Tag = HUNT_CODE,
    Valid_GMUs = All_Units,
    Dates      = DATES,
    Notes      = All_Notes,
    List = LIST
  )
]

#Merge the useful columns from the GMU list into the DOA report
Fullelk25 <- longDOAelk25[
  GMUElk26,
  on = "Tag"
]
# Combine the two list columns
Fullelk25[, List := fcoalesce(i.List, List)]

# Remove the duplicate
Fullelk25[, i.List := NULL]

#Add "New" designation
Fullelk25[is.na(Class),New:="New"]
Fullelk25[New=="New",Drawn_out_level:="New Code - No historical draw data"]
Fullelk25[New=="New",Chance_at_DOL:="New Code - Unknown Chance"]
Fullelk25[New=="New",Notes:="New 2026 Code"]



#add in a "High-demand" and "Hybrid" column to note the high demand tags
Fullelk25[Tag%in%highdemand$'2025',HD:="High Demand"]
Fullelk25[is.na(HD),HD:="No"]

Fullelk25[Tag%in%hybrid$'CodeND',hybrid:="Hybrid"]
Fullelk25[is.na(hybrid),hybrid:="No"]




Fullelk25[is.na(Notes)|Notes=="No",Notes:=""]
Fullelk25[grepl("P",SeasonCode),PLO:="Yes"]
Fullelk25[!grepl("P",SeasonCode),PLO:="No"]

Fullelk25[grepl("W",SeasonCode),RFW:="Yes"]
Fullelk25[!grepl("W",SeasonCode),RFW:="No"]
Fullelk25[Drawn_out_level=="No Apps" |Drawn_out_level== "None Drawn", NoApps:="Yes"]


#split out into long table by units

Fullelk25_Longunits <- as.data.table(Fullelk25 %>%
                                       separate_rows(Valid_GMUs, sep = ",\\s*") %>%   # split "1, 5, 3" into rows
                                       mutate(Valid_GMUs = as.integer(Valid_GMUs)))


setnames(Fullelk25_Longunits,"Valid_GMUs","Unit")


#give designation for tags you don't need to burn points on
Fullelk25_Longunits[grepl(c("Choice|Leftover"),Drawn_out_level), nopoints:="Y"]
Fullelk25_Longunits[!grepl(c("Choice|Leftover"),Drawn_out_level), nopoints:="N"]


##Adding in HarvestUnit to map to harvest table in java----
# Start with a single merged table, but no harvestunit yet
Fullelk25_LongunitsMerged <- copy(Fullelk25_Longunits)
Fullelk25_LongunitsMerged[, harvestunit := NA_character_]

# Condition 1: Bosque del Oso - Archery
Fullelk25_LongunitsMerged[
  grepl("Bosque", Notes) & !grepl("Except", Notes) & Weapon == "A",
  harvestunit := elkHarvest25[Category == "Bosque del Oso - Archery"][
    .SD, on = "Unit", x.harvestunit]
]

# Bosque 1st rifle
Fullelk25_LongunitsMerged[
  grepl("Bosque", Notes) & !grepl("Except", Notes) & SeasonWeapon == "O1R",
  harvestunit := elkHarvest25[Category == "Bosque del Oso - 1st Rifle"][
    .SD, on = "Unit", x.harvestunit]
]

# Bosque 2nd rifle
Fullelk25_LongunitsMerged[
  grepl("Bosque", Notes) & !grepl("Except", Notes) & SeasonWeapon == "O2R",
  harvestunit := elkHarvest25[Category == "Bosque del Oso - 2nd Rifle"][
    .SD, on = "Unit", x.harvestunit]
]
#Bosque muzz
Fullelk25_LongunitsMerged[
  grepl("Bosque", Notes) & !grepl("Except", Notes) & Weapon == "M",
  harvestunit := elkHarvest25[Category == "Bosque del Oso - Muzzleloader"][
    .SD, on = "Unit", x.harvestunit]
]



#4th rifle antlered
Fullelk25_LongunitsMerged[is.na(harvestunit)&
                            SeasonWeapon=="O4R" & Sex=="M",
                          harvestunit := elkHarvest25[Category == "Antlered Fourth Rifle Seasons (No PLO or Either-sex tags included"][
                            .SD, on = "Unit", x.harvestunit]
]

Fullelk25_LongunitsMerged[is.na(harvestunit)&
                            SeasonWeapon=="O4R" & Sex=="M",
                          harvestunit := elkHarvest25[Category == "Fourth Season Limited Antlered"][
                            .SD, on = "Unit", x.harvestunit]
]
Fullelk25_LongunitsMerged[is.na(harvestunit)&
                            SeasonWeapon=="O4R" & Sex=="E",
                          harvestunit := elkHarvest25[Category == "Fourth Season Limited Either-sex Seasons"][
                            .SD, on = "Unit", x.harvestunit]
]

Fullelk25_LongunitsMerged[is.na(harvestunit)&
                            SeasonWeapon=="O4R" & (Sex=="M"|Sex=="E"),
                          harvestunit := elkHarvest25[Category == "All Antlered Rifle Seasons"][
                            .SD, on = "Unit", x.harvestunit]
]


#3rd rifle antlered
Fullelk25_LongunitsMerged[is.na(harvestunit)&
                            SeasonWeapon=="O3R" & Sex=="M",
                          harvestunit := elkHarvest25[Category == "Antlered Third Rifle Seasons (No PLO or Either-sex tags included)"][
                            .SD, on = "Unit", x.harvestunit]
]

Fullelk25_LongunitsMerged[is.na(harvestunit)&
                            SeasonWeapon=="O3R" & Sex=="M",
                          harvestunit := elkHarvest25[Category == "Third Season Limited Antlered"][
                            .SD, on = "Unit", x.harvestunit]
]

Fullelk25_LongunitsMerged[is.na(harvestunit)&
                            SeasonWeapon=="O3R" & Sex=="E",
                          harvestunit := elkHarvest25[Category == "Third Season Limited Either-sex Seasons"][
                            .SD, on = "Unit", x.harvestunit]
]
Fullelk25_LongunitsMerged[is.na(harvestunit)&
                            SeasonWeapon=="O3R" & (Sex=="M"|Sex=="E"),
                          harvestunit := elkHarvest25[Category == "All Antlered Rifle Seasons"][
                            .SD, on = "Unit", x.harvestunit]
]

#2nd rifle antlered
Fullelk25_LongunitsMerged[is.na(harvestunit)&
                            SeasonWeapon=="O2R" & Sex=="M",
                          harvestunit := elkHarvest25[Category == "Antlered Second Rifle Seasons (No PLO or Either-sex tags included)"][
                            .SD, on = "Unit", x.harvestunit]
]

Fullelk25_LongunitsMerged[is.na(harvestunit)&
                            SeasonWeapon=="O2R" & Sex=="M",
                          harvestunit := elkHarvest25[Category == "Second Season Limited Antlered"][
                            .SD, on = "Unit", x.harvestunit]
]

Fullelk25_LongunitsMerged[is.na(harvestunit)&
                            SeasonWeapon=="O2R" & Sex=="E",
                          harvestunit := elkHarvest25[Category == "Second Season Limited Either-sex Seasons"][
                            .SD, on = "Unit", x.harvestunit]
]
Fullelk25_LongunitsMerged[is.na(harvestunit)&
                            SeasonWeapon=="O2R" & (Sex=="M"|Sex=="E"),
                          harvestunit := elkHarvest25[Category == "All Antlered Rifle Seasons"][
                            .SD, on = "Unit", x.harvestunit]
]


#1st rifle antlered
Fullelk25_LongunitsMerged[is.na(harvestunit)&
                            SeasonWeapon=="O1R" & Sex=="M",
                          harvestunit := elkHarvest25[Category == "Antlered First Rifle Seasons (No PLO or Either-sex tags included)"][
                            .SD, on = "Unit", x.harvestunit]
]

Fullelk25_LongunitsMerged[is.na(harvestunit)&
                            SeasonWeapon=="O1R" & Sex=="M",
                          harvestunit := elkHarvest25[Category == "First Season Limited Antlered"][
                            .SD, on = "Unit", x.harvestunit]
]

Fullelk25_LongunitsMerged[is.na(harvestunit)&
                            SeasonWeapon=="O1R" & Sex=="E",
                          harvestunit := elkHarvest25[Category == "First Season Limited Either-sex Seasons"][
                            .SD, on = "Unit", x.harvestunit]
]

Fullelk25_LongunitsMerged[is.na(harvestunit)&
                            SeasonWeapon=="O1R" & (Sex=="M"|Sex=="E"),
                          harvestunit := elkHarvest25[Category == "All Antlered Rifle Seasons"][
                            .SD, on = "Unit", x.harvestunit]
]


#First season rifle antlerless
Fullelk25_LongunitsMerged[is.na(harvestunit)&
                            SeasonWeapon=="O1R" & Sex=="F",
                          harvestunit := elkHarvest25[Category == "Antlerless First Rifle Seasons (No PLO or Either-sex tags included)"][
                            .SD, on = "Unit", x.harvestunit]]

Fullelk25_LongunitsMerged[is.na(harvestunit)&
                            SeasonWeapon=="O1R" & Sex=="F",
                          harvestunit := elkHarvest25[Category == "First Season Limited Antlerless"][
                            .SD, on = "Unit", x.harvestunit]]

Fullelk25_LongunitsMerged[is.na(harvestunit)&
                            SeasonWeapon=="O1R" & Sex=="F",
                          harvestunit := elkHarvest25[Category == "All Antlerless Rifle Seasons"][
                            .SD, on = "Unit", x.harvestunit]]


#second season rifle antlerless
Fullelk25_LongunitsMerged[is.na(harvestunit)&
                            SeasonWeapon=="O2R" & Sex=="F",
                          harvestunit := elkHarvest25[Category == "Antlerless Second Rifle Seasons (No PLO or Either-sex tags included)"][
                            .SD, on = "Unit", x.harvestunit]]

Fullelk25_LongunitsMerged[is.na(harvestunit)&
                            SeasonWeapon=="O2R" & Sex=="F",
                          harvestunit := elkHarvest25[Category == "Second Season Limited Antlerless"][
                            .SD, on = "Unit", x.harvestunit]]

Fullelk25_LongunitsMerged[is.na(harvestunit)&
                            SeasonWeapon=="O2R" & Sex=="F",
                          harvestunit := elkHarvest25[Category == "All Antlerless Rifle Seasons"][
                            .SD, on = "Unit", x.harvestunit]]

#Third season rifle antlerless
Fullelk25_LongunitsMerged[is.na(harvestunit)&
                            SeasonWeapon=="O3R" & Sex=="F",
                          harvestunit := elkHarvest25[Category == "Antlerless Third Rifle Seasons (No PLO or Either-sex tags included)"][
                            .SD, on = "Unit", x.harvestunit]]

Fullelk25_LongunitsMerged[is.na(harvestunit)&
                            SeasonWeapon=="O3R" & Sex=="F",
                          harvestunit := elkHarvest25[Category == "Third Season Limited Antlerless"][
                            .SD, on = "Unit", x.harvestunit]]

Fullelk25_LongunitsMerged[is.na(harvestunit)&
                            SeasonWeapon=="O3R" & Sex=="F",
                          harvestunit := elkHarvest25[Category == "All Antlerless Rifle Seasons"][
                            .SD, on = "Unit", x.harvestunit]]
#fourth season rifle antlerless
Fullelk25_LongunitsMerged[is.na(harvestunit)&
                            SeasonWeapon=="O4R" & Sex=="F",
                          harvestunit := elkHarvest25[Category == "Antlerless Fourth Rifle Seasons (No PLO or Either-sex tags included)"][
                            .SD, on = "Unit", x.harvestunit]]

Fullelk25_LongunitsMerged[is.na(harvestunit)&
                            SeasonWeapon=="O4R" & Sex=="F",
                          harvestunit := elkHarvest25[Category == "Fourth Season Limited Antlerless"][
                            .SD, on = "Unit", x.harvestunit]]

Fullelk25_LongunitsMerged[is.na(harvestunit)&
                            SeasonWeapon=="O4R" & Sex=="F",
                          harvestunit := elkHarvest25[Category == "All Antlerless Rifle Seasons"][
                            .SD, on = "Unit", x.harvestunit]]

#Antlerless muzzleloader
Fullelk25_LongunitsMerged[is.na(harvestunit)&
                            Weapon=="M" & Sex=="F",
                          harvestunit := elkHarvest25[Category == "Antlerless Muzzleloader"][
                            .SD, on = "Unit", x.harvestunit]]
Fullelk25_LongunitsMerged[is.na(harvestunit)&
                            Weapon=="M" & (Sex=="F"|Sex=="E"),
                          harvestunit := elkHarvest25[Category == "All Muzzleloader Seasons"][
                            .SD, on = "Unit", x.harvestunit]]
Fullelk25_LongunitsMerged[is.na(harvestunit)&
                            Weapon=="M" & (Sex=="F"|Sex=="E"),
                          harvestunit := elkHarvest25[Category == "All Manners of Take"][
                            .SD, on = "Unit", x.harvestunit]]
#Antlered muzzleloader
Fullelk25_LongunitsMerged[is.na(harvestunit)&
                            Weapon=="M" & Sex=="M",
                          harvestunit := elkHarvest25[Category == "Antlered Muzzleloader"][
                            .SD, on = "Unit", x.harvestunit]]
Fullelk25_LongunitsMerged[is.na(harvestunit)&
                            Weapon=="M" & (Sex=="M"|Sex=="E"),
                          harvestunit := elkHarvest25[Category == "All Muzzleloader Seasons"][
                            .SD, on = "Unit", x.harvestunit]]
Fullelk25_LongunitsMerged[is.na(harvestunit)&
                            Weapon=="M" & (Sex=="M"|Sex=="E"),
                          harvestunit := elkHarvest25[Category == "All Manners of Take"][
                            .SD, on = "Unit", x.harvestunit]]

#All archery
Fullelk25_LongunitsMerged[Weapon=="A",
                          harvestunit := elkHarvest25[Category == "All Archery Seasons"][
                            .SD, on = "Unit", x.harvestunit]]
Fullelk25_LongunitsMerged[is.na(harvestunit) & Weapon=="A",
                          harvestunit := elkHarvest25[Category == "All Manners of Take"][
                            .SD, on = "Unit", x.harvestunit]]


#RFW
Fullelk25_LongunitsMerged[is.na(harvestunit)&
                            RFW=="Yes",
                          harvestunit := elkHarvest25[Category == "All Ranching For Wildlife Seasons"][
                            .SD, on = "Unit", x.harvestunit]]

Fullelk25_LongunitsMerged[is.na(harvestunit)&
                            RFW=="Yes",
                          harvestunit := elkHarvest25[Category == "All Manners of Take"][
                            .SD, on = "Unit", x.harvestunit]]



#late tags
Fullelk25_LongunitsMerged[is.na(harvestunit)& grepl("L",SeasonWeapon),
                          harvestunit := elkHarvest25[Category == "All Late Seasons (Not including PLOs)"][
                            .SD, on = "Unit", x.harvestunit]]
Fullelk25_LongunitsMerged[is.na(harvestunit)& grepl("L",SeasonWeapon),
                          harvestunit := elkHarvest25[Category == "All Manners of Take"][
                            .SD, on = "Unit", x.harvestunit]]
#early tags

Fullelk25_LongunitsMerged[is.na(harvestunit)& grepl("E",SeasonWeapon),
                          harvestunit := elkHarvest25[Category == "All Early Seasons"][
                            .SD, on = "Unit", x.harvestunit]]
Fullelk25_LongunitsMerged[is.na(harvestunit)& grepl("E",SeasonWeapon),
                          harvestunit := elkHarvest25[Category == "All Manners of Take"][
                            .SD, on = "Unit", x.harvestunit]]
#PLO

Fullelk25_LongunitsMerged[is.na(harvestunit)& grepl("P5",SeasonWeapon),
                          harvestunit := elkHarvest25[Category == "Late PLO Seasons"][
                            .SD, on = "Unit", x.harvestunit]]
Fullelk25_LongunitsMerged[is.na(harvestunit)& grepl("P5",SeasonWeapon),
                          harvestunit := elkHarvest25[Category == "Early PLO Seasons"][
                            .SD, on = "Unit", x.harvestunit]]
Fullelk25_LongunitsMerged[is.na(harvestunit)& grepl("P1",SeasonWeapon),
                          harvestunit := elkHarvest25[Category == "First Rifle Seasons (All Harvest - Includes PLOs)"][
                            .SD, on = "Unit", x.harvestunit]]

Fullelk25_LongunitsMerged[is.na(harvestunit)& grepl("P2",SeasonWeapon),
                          harvestunit := elkHarvest25[Category == "Second Rifle Seasons (All Harvest - Includes PLOs)"][
                            .SD, on = "Unit", x.harvestunit]]
Fullelk25_LongunitsMerged[is.na(harvestunit)& grepl("P3",SeasonWeapon),
                          harvestunit := elkHarvest25[Category == "Third Rifle Seasons (All Harvest - Includes PLOs)"][
                            .SD, on = "Unit", x.harvestunit]]
Fullelk25_LongunitsMerged[is.na(harvestunit)& grepl("P4",SeasonWeapon),
                          harvestunit := elkHarvest25[Category == "Fourth Rifle Seasons (All Harvest - Includes PLOs)"][
                            .SD, on = "Unit", x.harvestunit]]

Fullelk25_LongunitsMerged[is.na(harvestunit)& grepl("P",SeasonWeapon),
                          harvestunit := elkHarvest25[Category == "Private Land Only Seasons"][
                            .SD, on = "Unit", x.harvestunit]]
#OTC
Fullelk25_LongunitsMerged[is.na(harvestunit)& (SeasonWeapon=="U2R"|SeasonWeapon=="U5R"),
                          harvestunit := elkHarvest25[Category == "All Rifle Seasons"][
                            .SD, on = "Unit", x.harvestunit]]

#Final safety check to get all

Fullelk25_LongunitsMerged[is.na(harvestunit) & Weapon=="R",
                          harvestunit := elkHarvest25[Category == "All Rifle Seasons"][
                            .SD, on = "Unit", x.harvestunit]]

Fullelk25_LongunitsMerged[is.na(harvestunit),
                          harvestunit := elkHarvest25[Category == "All Manners of Take"][
                            .SD, on = "Unit", x.harvestunit]]


#collapse harvestunit into a list of harvest units
collapse_cols <- c("Unit","harvestunit")
group_cols <- setdiff(names(Fullelk25_LongunitsMerged), collapse_cols)

Fullelk25_wideunits <- Fullelk25_LongunitsMerged[, lapply(.SD, function(x) paste(x, collapse=",")),
                                                 by = group_cols, .SDcols = collapse_cols]

setnames(Fullelk25_wideunits,"Unit","Valid GMUs")
#----##Add list of tags to the harvest stats
tags_by_unit <- Fullelk25_Longunits[
  , .(Tags = paste(unique(Tag), collapse=",")), 
  by = Unit
]

elkHarvest25 <- merge(
  elkHarvest25, 
  tags_by_unit, 
  by = "Unit", 
  all.x = TRUE
)
##Remove NA units
elkHarvest25<-elkHarvest25[!is.na(Unit)]


#elk add in public land. Add up the total public land in each unit
# Step 1: Split Numbers column into separate rows
Fullelk25_Long <- as.data.table(Fullelk25_wideunits %>%
                                  separate_rows(`Valid GMUs`, sep = ",\\s*") %>%   # split "1, 5, 3" into rows
                                  mutate(`Valid GMUs` = as.integer(`Valid GMUs`)))

# Step 2: Join with df2 to get percentages
Fullelk25_Long_joined <- Fullelk25_Long %>%
  left_join(PublicLandGMU, by = c("Valid GMUs" = "Unit"))



# # Step 3: Average percentages back per ID
# Fullelk25Final <- Fullelk25_Long_joined %>%
#   group_by(Tag) %>%
#   summarise(
#     'Valid GMUs' = paste('Valid GMUs', collapse = ", "),
#     Total_Acres = sum(Acres, na.rm = TRUE),
#     Public_Acres = sum(`Acres Public`, na.rm = TRUE),
#     )

# Step 3a: Compute the summaries
summaries <- as.data.table(
  Fullelk25_Long_joined %>%
    group_by(Tag,Class) %>%
    summarise(
      # `Valid GMUs` = paste(`Valid GMUs`, collapse = ", "),
      Total_Acres = sum(Acres, na.rm = TRUE),
      Public_Acres = sum(`Acres Public`, na.rm = TRUE),
      .groups = "drop"
    )
)

# Step 3b: Join back to the original (all columns preserved)
Fullelk25Final <- Fullelk25_wideunits %>%
  left_join(summaries, by = c('Tag','Class'))




#calculate percent public land
Fullelk25Final[,Public_Percent:= paste0(round((Public_Acres / Total_Acres) * 100, 1), "%")]

Fullelk25Final[, c("Total_Acres","Public_Acres") := lapply(.SD, round, 0), .SDcols = c("Total_Acres","Public_Acres")]

##Combine old drawn out numbers with new
Fullelk25Final[Elkdraw24, `:=`(
  Drawn_out_level24 = i.Drawn_out_level,
  Chance_at_DOL24   = i.Chance_at_DOL
), on = c("Tag","Class")]

Fullelk25Final[Elkdraw23, `:=`(
  Drawn_out_level23 = i.Drawn_out_level,
  Chance_at_DOL23   = i.Chance_at_DOL
), on = c("Tag","Class")]


##Add in a way to view trend of draw----
#switch chance at DOL to a decimal and add it to the actual level so the drawn out level can be entirely numeric
Fullelk25Final[, Chance_at_DOLN := 1-(as.numeric(gsub("%", "", Chance_at_DOL)) / 100)]
Fullelk25Final[Drawn_out_level=="Choice 2",DOL:=-1]
Fullelk25Final[Drawn_out_level=="Choice 3",DOL:=-2]
Fullelk25Final[Drawn_out_level=="Choice 4",DOL:=-3]
Fullelk25Final[Drawn_out_level %in% "Leftover",DOL:=-4]
Fullelk25Final[,DOLE25:=DOL+Chance_at_DOLN]

Fullelk25Final[, Chance_at_DOLN24 := 1-(as.numeric(gsub("%", "", Chance_at_DOL24)) / 100)]
Fullelk25Final[Drawn_out_level24=="Choice 2",DOL24:=-1]
Fullelk25Final[Drawn_out_level24=="Choice 3",DOL24:=-2]
Fullelk25Final[Drawn_out_level24=="Choice 4",DOL24:=-3]
Fullelk25Final[Drawn_out_level24=="Leftover",DOL24:=-4]
Fullelk25Final[,DOLE24:=DOL24+Chance_at_DOLN24]

Fullelk25Final[, Chance_at_DOLN23 := 1-(as.numeric(gsub("%", "", Chance_at_DOL23)) / 100)]
Fullelk25Final[Drawn_out_level23=="Choice 2",DOL23:=-1]
Fullelk25Final[Drawn_out_level23=="Choice 3",DOL23:=-2]
Fullelk25Final[Drawn_out_level23=="Choice 4",DOL23:=-3]
Fullelk25Final[Drawn_out_level23 %in% "Leftover",DOL23:=-4]
Fullelk25Final[,DOLE23:=DOL23+Chance_at_DOLN23]

#calculate slope and predicted value
x <- 1:3

Fullelk25Final[, c("slope", "predictedDrawnOut") := {
  
  y <- c(DOLE23, DOLE24, DOLE25)
  
  if (all(is.na(y))) {
    list(NA_real_, NA_real_)
  } else {
    fit <- lm(y ~ x)
    slope <- coef(fit)[2]
    predictedDrawnOut <- coef(fit)[1] + slope * 4
    list(slope, predictedDrawnOut)
  }
  
}, by = 1:nrow(Fullelk25Final)]

Fullelk25Final[predictedDrawnOut>0, `:=`(
  PredPrefPoints = paste0(floor(predictedDrawnOut), " Preference Points"),   # integer + text
  Predpercent = 1-(predictedDrawnOut - floor(predictedDrawnOut)),                       # decimal only
  PredPrefPercent = paste0(round(((1-(predictedDrawnOut - floor(predictedDrawnOut))) * 100)), "%")  # percent string
)]

Fullelk25Final[predictedDrawnOut<=0 & predictedDrawnOut>-1, `:=`(
  PredPrefPoints = "Choice 2",  
  Predpercent = -(floor(predictedDrawnOut)- predictedDrawnOut),                       # decimal only
  PredPrefPercent = paste0(round(-(floor(predictedDrawnOut)-predictedDrawnOut) * 100), "%")  # percent string
)]

Fullelk25Final[predictedDrawnOut<=-1 & predictedDrawnOut>-2, `:=`(
  PredPrefPoints = "Choice 3",  
  Predpercent = -(floor(predictedDrawnOut)- predictedDrawnOut),                       # decimal only
  PredPrefPercent = paste0(round(-(floor(predictedDrawnOut)-predictedDrawnOut) * 100), "%")  # percent string
)]

Fullelk25Final[predictedDrawnOut<=-2 & predictedDrawnOut>-3, `:=`(
  PredPrefPoints = "Choice 4",  
  Predpercent = -(floor(predictedDrawnOut)- predictedDrawnOut),                       # decimal only
  PredPrefPercent = paste0(round(-(floor(predictedDrawnOut)-predictedDrawnOut) * 100), "%")  # percent string
)]

Fullelk25Final[predictedDrawnOut<=-3, `:=`(
  PredPrefPoints = "Leftover",  
  Predpercent = -(floor(predictedDrawnOut)- predictedDrawnOut),                       # decimal only
  PredPrefPercent = paste0(round(-(floor(predictedDrawnOut)-predictedDrawnOut) * 100), "%")  # percent string
)]

#remove OTC tags
Fullelk25Final<-Fullelk25Final[!grepl("U",SeasonWeapon)]
#remove lines with non-res codes for residents
Fullelk25Final <- Fullelk25Final[
  !(grepl("V", SeasonCode) & Class %in% c("A_R", "Y_R", "L_R", "L_U"))
]



###Split out OTC table and add OTC designationAdd OTC designation for harvest stats----
ArcheryAntlerlessOTC<-c(6, 16, 17, 18, 21, 22, 30, 31, 32,
                        38, 43, 59, 82, 85, 86, 133, 134,
                        140, 141, 142, 161, 171, 181,
                        431, 471, 511, 581, 591,
                        682, 691, 791, 851, 861)
ArcheryAntlerlessOTCEastNonres<-c(133, 134, 141, 142)
ArcheryEitherSexOTC<-c(3, 6, 11, 13, 14, 15, 16, 17, 18, 21, 22,
                       25, 26, 27, 28, 30, 31, 32, 34, 35, 36,
                       37, 38, 43, 53, 59, 60, 62, 63, 64, 65,
                       68, 79, 82, 83, 85,
                       86, 87, 88, 89, 90, 91, 92, 93, 94, 95,
                       96, 97, 98, 99, 100, 101, 102, 103, 105,
                       106, 107, 109, 110, 111, 112, 113, 114,
                       115, 116, 117, 118, 119, 120, 121, 122,
                       123, 124, 125, 126, 127, 128, 129, 130,
                       131, 132, 133, 134, 135, 136, 137, 138,
                       139, 140, 141, 142, 143, 144, 145, 146,
                       147, 161, 171, 181, 211, 214, 231, 301,
                       361, 371, 431, 471, 511, 581, 591, 681,
                       691, 851,
                       861, 951)
ArcheryEitherSexOTCEastNonRes<-c(87, 88, 89, 90, 91, 92, 93, 94, 95, 96,
                                 97, 98, 99, 100, 101, 102, 103, 105,
                                 106, 107, 109, 110, 111, 112, 113,
                                 114, 115, 116, 117, 118, 119, 120,
                                 121, 122, 123, 124, 125, 126, 127,
                                 128, 129, 130, 132, 133, 134, 135,
                                 136, 137, 138, 139, 141, 142, 143,
                                 144, 145, 146, 147, 951)

RifleOTC<-c(6, 11, 12, 13, 14, 15,
            16, 17, 18, 21, 22, 23,
            24, 25, 26, 27, 28, 30, 31,
            32, 33, 34, 35, 36, 37, 38,
            41, 42, 43, 44, 45, 47, 52, 53,
            59, 60, 62, 63, 64, 65, 68, 70,
            71, 72, 73, 74, 75, 77, 78, 80,
            81, 82,
            85, 86,131, 133, 134,
            140, 141, 142, 161, 171, 181,
            211, 214, 231, 301,
            361, 371, 411, 421, 431,
            441, 444, 471, 511,
            521, 581, 591, 681, 691, 711,
            741, 751, 771, 851)

RifleOTCPlains<-c(87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 105, 106,
                  107, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126,
                  127, 129, 130, 132, 135, 136, 137, 138, 139, 143, 144, 145, 146, 147, 951)


elkHarvest25$Unit<-as.numeric(elkHarvest25$Unit)

elkOTCHarvest24<-copy(elkHarvest25)

elkOTCHarvest24[Category=="All Archery Seasons" & Unit%in%ArcheryAntlerlessOTC,
             OTCAA:="Archery Antlerless"]
elkOTCHarvest24[Category=="All Archery Seasons" & Unit%in%ArcheryEitherSexOTC,
             OTCAE:="Archery Either Sex"]
elkOTCHarvest24[Category=="Antlered Second Rifle Seasons (No PLO or Either-sex tags included)" & Unit%in%RifleOTC,
             OTC2R:="Second Season Antlered"]
elkOTCHarvest24[Category=="Antlered Third Rifle Seasons (No PLO or Either-sex tags included)" & Unit%in%RifleOTC,
             OTC3R:="Third Season Antlered"]
elkOTCHarvest24[Category=="All Rifle Seasons" & Unit%in%RifleOTCPlains,
             OTCplainsR:="Plains OTC Rifle Either Sex"]



elkOTCHarvest24[Category=="All Archery Seasons" & Unit%in%ArcheryEitherSexOTCEastNonRes,
             OTCAplains:="Plains Either Sex OTC"]

elkOTCHarvest24[Category=="All Archery Seasons" & Unit%in%ArcheryAntlerlessOTCEastNonres,
             OTCAplainsF:="Plains Antlerless OTC"]

elkOTCHarvest24<-data.table::melt(elkOTCHarvest24,
     measure.vars=c('OTCAA','OTCAE','OTC2R','OTC3R','OTCplainsR','OTCAplains','OTCAplainsF'),
     variable.name = "OTC",
     value.name = "OTCCat"
     )
elkOTCHarvest24<-elkOTCHarvest24[!is.na(OTCCat)]



#write CSV Elk----
write_csv(Fullelk25Final, file='Website/Fullelk26Final.csv')
write_csv(elkHarvest25,'Website/elkHarvest25.csv')
write_csv(elkOTCHarvest24,'Website/elkOTC24.csv')




####ant Stats----

DOAant25 <- as.data.table(read_excel("Raw data/Drawn out at reports and exported Excel sheets/2025AntDrawnOut.xlsx"))
DOAant25CPW<-as.data.table(read_excel("Raw data/CPW/2025 Min Pref Points Extract final.xlsx", 
                                      sheet = "Pronghorn"))

GMUant26<-as.data.table(read_excel("Raw data/CPW/2026 BG Hunt codes for Brochures_Manual_update.xlsx", 
                                   sheet = "Pronghorn"))



antHarvest25<-as.data.table(read_excel("Raw data/Harvest_and_public/AntHarvest24Final.xlsx"))
antHarvest23<-as.data.table(read_excel("Raw data/Harvest_and_public/AntHarvest23Final.xlsx"))
antHarvest22<-as.data.table(read_excel("Raw data/Harvest_and_public/AntHarvest22Final.xlsx"))


Antdraw23<-as.data.table(read_excel("Raw data/Drawn out at reports and exported Excel sheets/organizedDOASheets/AntDraw23.xlsx"))
Antdraw24<-as.data.table(read_excel("Raw data/Drawn out at reports and exported Excel sheets/organizedDOASheets/AntDraw24.xlsx"))




##remove any empty rows
DOAant25<-DOAant25[!is.na(...3)]
##change names of columns
setnames(DOAant25,names(DOAant25),c('Tag','List','title','DOAAdultR','DOAAdultNR','DOAYouthR','DOAYouthNR','DOALandUnRes','DOALandRes'))

##Forward fill the codes into the both rows

DOAant25[, Tag := na.locf(Tag, na.rm = FALSE)]
DOAant25[is.na(List),num:=1]
DOAant25[!is.na(List),num:=2]
nam<-names(DOAant25)
##remove the names of the variables  we're going to split wide on and any extra unnecessary column
nam<-nam[-1]
nam<-nam[-9]
##Fix the ones where the tag # was listed twice due to falling on page line. This will be specific for each year
DOAant25<-unique(DOAant25)
Duplicate2<-DOAant25[num==2]
Duplicate1<-DOAant25[num==1]

DOAantdups2<-unique(Duplicate2[duplicated(Tag) | duplicated(Tag, fromLast=TRUE)]$Tag)
DOAantdups1<-unique(Duplicate1[duplicated(Tag) | duplicated(Tag, fromLast=TRUE)]$Tag)

DOAant25<-DOAant25[(Tag==DOAantdups1 | Tag==DOAantdups2) & title=="Drawn Out At", num:=2]
DOAant25<-DOAant25[(Tag==DOAantdups1| Tag==DOAantdups2) & title=="# Drawn at Final Level", num:=1]

DOAant25 <- DOAant25[!duplicated(DOAant25, by = c("Tag","num"))]



##Split wide using dcast
DOAant25 <- data.table::dcast(
  DOAant25,
  Tag ~ num,
  value.var = nam
)
DOAant25[,List_1:=NULL]
##Split and add columns to calculate chances
# NNames<-names(DOAant25)
# NNames<-NNames[-c(1:7)]
# NNames<-NNames[-c(31:41)]

setnames(DOAant25, c("List_2","DOAAdultR_2",'DOAAdultNR_2',"DOAYouthR_2","DOAYouthNR_2","DOALandUnRes_2","DOALandRes_2"),
         c("List","A_R_drawn_out_level","A_NR_drawn_out_level","Y_R_drawn_out_level","Y_NR_drawn_out_level","L_U_drawn_out_level","L_R_drawn_out_level"))

##Split the x of y column into x and y columns and ad a ratio.
DOAant25[, c("A_R_successful_apps_at_DOL", "A_R_total_apps_at_DOL") := tstrsplit(DOAAdultR_1, "of", fixed = TRUE)]
DOAant25[, c("A_NR_successful_apps_at_DOL", "A_NR_total_apps_at_DOL") := tstrsplit(DOAAdultNR_1, "of", fixed = TRUE)]
DOAant25[, c("Y_R_successful_apps_at_DOL", "Y_R_total_apps_at_DOL") := tstrsplit(DOAYouthR_1, "of", fixed = TRUE)]
DOAant25[, c("Y_NR_successful_apps_at_DOL", "Y_NR_total_apps_at_DOL") := tstrsplit(DOAYouthNR_1, "of", fixed = TRUE)]
DOAant25[, c("L_U_successful_apps_at_DOL", "L_U_total_apps_at_DOL") := tstrsplit(DOALandUnRes_1, "of", fixed = TRUE)]
DOAant25[, c("L_R_successful_apps_at_DOL", "L_R_total_apps_at_DOL") := tstrsplit(DOALandRes_1, "of", fixed = TRUE)]

# Convert to numeric
DOAant25$A_R_successful_apps_at_DOL<-as.numeric(DOAant25$A_R_successful_apps_at_DOL)
DOAant25$A_R_total_apps_at_DOL<-as.numeric(DOAant25$A_R_total_apps_at_DOL)

DOAant25$A_NR_successful_apps_at_DOL<-as.numeric(DOAant25$A_NR_successful_apps_at_DOL)
DOAant25$A_NR_total_apps_at_DOL<-as.numeric(DOAant25$A_NR_total_apps_at_DOL)

DOAant25$Y_R_successful_apps_at_DOL<-as.numeric(DOAant25$Y_R_successful_apps_at_DOL)
DOAant25$Y_R_total_apps_at_DOL<-as.numeric(DOAant25$Y_R_total_apps_at_DOL)

DOAant25$Y_NR_successful_apps_at_DOL<-as.numeric(DOAant25$Y_NR_successful_apps_at_DOL)
DOAant25$Y_NR_total_apps_at_DOL<-as.numeric(DOAant25$Y_NR_total_apps_at_DOL)

DOAant25$L_R_successful_apps_at_DOL<-as.numeric(DOAant25$L_R_successful_apps_at_DOL)
DOAant25$L_R_total_apps_at_DOL<-as.numeric(DOAant25$L_R_total_apps_at_DOL)

DOAant25$L_U_successful_apps_at_DOL<-as.numeric(DOAant25$L_U_successful_apps_at_DOL)
DOAant25$L_U_total_apps_at_DOL<-as.numeric(DOAant25$L_U_total_apps_at_DOL)

# Add ratio column
DOAant25[,"A_R_chance_at_DOL"  :=  A_R_successful_apps_at_DOL/ A_R_total_apps_at_DOL]
DOAant25[,"A_NR_chance_at_DOL"  :=  A_NR_successful_apps_at_DOL/ A_NR_total_apps_at_DOL]
DOAant25[,"Y_R_chance_at_DOL"  :=  Y_R_successful_apps_at_DOL/ Y_R_total_apps_at_DOL]
DOAant25[,"Y_NR_chance_at_DOL"  :=  Y_NR_successful_apps_at_DOL/ Y_NR_total_apps_at_DOL]
DOAant25[,"L_U_chance_at_DOL"  :=  L_U_successful_apps_at_DOL/ L_U_total_apps_at_DOL]
DOAant25[,"L_R_chance_at_DOL"  :=  L_R_successful_apps_at_DOL/ L_R_total_apps_at_DOL]

#ratio column (Chance at DOL) should equal 100% (1) if the Drawn out level is "Leftover"
DOAant25[A_R_drawn_out_level=="Leftover",
         A_R_chance_at_DOL:=1]
DOAant25[A_NR_drawn_out_level=="Leftover",
         A_NR_chance_at_DOL:=1]
DOAant25[Y_NR_drawn_out_level=="Leftover",
         Y_NR_chance_at_DOL:=1]
DOAant25[Y_R_drawn_out_level=="Leftover",
         Y_R_chance_at_DOL:=1]
DOAant25[L_U_drawn_out_level=="Leftover",
         L_U_chance_at_DOL:=1]
DOAant25[L_R_drawn_out_level=="Leftover",
         L_R_chance_at_DOL:=1]


#Specify chances with first choice
##If leftover or choice 2/3/4 it's 100% chance
DOAant25[A_R_drawn_out_level=="Leftover"|
           grepl("Choice",A_R_drawn_out_level),
         A_R_chance_with_first_choice:=1]
DOAant25[A_NR_drawn_out_level=="Leftover"|
           grepl("Choice",A_NR_drawn_out_level),
         A_NR_chance_with_first_choice:=1]
DOAant25[Y_NR_drawn_out_level=="Leftover"|
           grepl("Choice",Y_NR_drawn_out_level),
         Y_NR_chance_with_first_choice:=1]
DOAant25[Y_R_drawn_out_level=="Leftover"|
           grepl("Choice",Y_R_drawn_out_level),
         Y_R_chance_with_first_choice:=1]
DOAant25[L_U_drawn_out_level=="Leftover"|
           grepl("Choice",L_U_drawn_out_level),
         L_U_chance_with_first_choice:=1]
DOAant25[L_R_drawn_out_level!="Leftover"|
           grepl("Choice",L_R_drawn_out_level),
         L_R_chance_with_first_choice:=1]
#if not leftover or choice 2/3/4, it's the same as the chance at drawn out level
DOAant25[A_R_drawn_out_level!="Leftover"|
           !grepl("Choice",A_R_drawn_out_level),
         A_R_chance_with_first_choice:=A_R_chance_at_DOL]
DOAant25[A_NR_drawn_out_level!="Leftover"|
           !grepl("Choice",A_NR_drawn_out_level),
         A_NR_chance_with_first_choice:=A_NR_chance_at_DOL]
DOAant25[Y_NR_drawn_out_level!="Leftover"|
           !grepl("Choice",Y_NR_drawn_out_level),
         Y_NR_chance_with_first_choice:=Y_NR_chance_at_DOL]
DOAant25[Y_R_drawn_out_level!="Leftover"|
           !grepl("Choice",Y_R_drawn_out_level),
         Y_R_chance_with_first_choice:=Y_R_chance_at_DOL]
DOAant25[L_U_drawn_out_level!="Leftover"|
           !grepl("Choice",L_U_drawn_out_level),
         L_U_chance_with_first_choice:=L_U_chance_at_DOL]
DOAant25[L_R_drawn_out_level!="Leftover"|
           !grepl("Choice",L_R_drawn_out_level),
         L_R_chance_with_first_choice:=L_R_chance_at_DOL]


#Go long and split out each class into rows


# longDOAant25 <- data.table::melt(
#   DOAant25,
#   measure.vars = patterns("_drawn_out_level$",
#                           "_successful_apps_at_DOL$"
#   #   total_apps_at_DOL         = "_total_apps_at_DOL$",
#   #   chance_at_DOL             = "_chance_at_DOL$",
#   #   chance_with_first_choice  = "_chance_with_first_choice$"
#   ),
#   value.name = c("Drawn_out_level",
#                 "Successful_apps_at_DOL"
#                  # "Total_apps_at_DOL",
#                  # "Chance_at_DOL",
#                  # "Chance_with_First_choice"
#                  ),
#   variable.name = "Class"
#  )
#
longDOAant25 <- data.table::melt(
  DOAant25,
  id.vars = NULL,   # keep any existing ID columns, if you have them
  measure.vars = patterns("drawn_out_level",
                          "successful_apps_at_DOL",
                          "total_apps_at_DOL",
                          "chance_at_DOL",
                          "chance_with_first_choice"),
  variable.name = "Class",
  value.name = c("Drawn_out_level",
                 "Successful_apps_at_DOL",
                 "Total_apps_at_DOL",
                 "Chance_at_DOL",
                 "Chance_with_First_choice")
)

# Now fix the ID column: map 1:6 → actual prefixes
ids <- c("A_R", "A_NR", "Y_R", "Y_NR", "L_U", "L_R")
longDOAant25[, Class := ids[Class]]

#Remove unneccesary columns
longDOAant25<-longDOAant25[,(3:10) :=NULL]


# Split out the tag into "Season, weapon, sex, etc."
longDOAant25[, `:=`(
  Animal = substr(Tag, 1, 1),   # D, A
  Sex = substr(Tag, 2, 2),   # E, B
  UnitCode = substr(Tag, 3, 5),   # 005, 123
  SeasonCode = substr(Tag, 6, 7),   # O2, X9
  Weapon = substr(Tag, 8, 8),    # R, Z
  SeasonWeapon = substr(Tag, 6, 8 ) #O2R
)]



#round off number

longDOAant25[,c("Chance_at_DOL","Chance_with_First_choice"):= lapply(.SD, round, 2), .SDcols = c("Chance_at_DOL","Chance_with_First_choice")]

#convert to percentages

longDOAant25$Chance_at_DOL<-percent(longDOAant25$Chance_at_DOL)
longDOAant25$Chance_with_First_choice<-percent(longDOAant25$Chance_with_First_choice)

#Add column for DOA PP that's only numeric
longDOAant25[grepl(c("Choice|Leftover"), Drawn_out_level), DOL:=0]

longDOAant25<-longDOAant25[is.na(DOL),DOL := as.integer(sub("^(\\d+).*", "\\1", Drawn_out_level))]

longDOAant25<-longDOAant25[is.na(DOL),DOL:=0]

#Integrate the useful columns from the CPW DOA workbook
setnames(DOAant25CPW, "Hunt Code", "Tag")
longDOAant25[DOAant25CPW, on="Tag", `:=` (Quota = Quota, `Choice 1 Applicants`= `Choice 1 Applicants`)]



#ant Harvest----
PublicLandGMU[,Acres:=gmu_area_sqmi*640]
PublicLandGMU[,`Acres Public`:=public_sqmi*640]




#Add in Previous year data
antHarvest25$`Percent Success`<-as.numeric(antHarvest25$`Percent Success`)
antHarvest25$`Total Hunters`<-as.numeric(antHarvest25$`Total Hunters`)

antHarvest25[, `:=`(
  `TH23`   = NA_real_,
  `PS23` = NA_real_,
  `TH22`   = NA_real_,
  `PS22` = NA_real_
  
)]


setnames(antHarvest23, c("Total Hunters","Percent Success"), c("TH23", "PS23"))
setnames(antHarvest22, c("Total Hunters","Percent Success"), c("TH22", "PS22"))

# Join and populate
antHarvest25[
  antHarvest23[, .(
    Unit,
    Category,
    TH23,
    PS23
  )],
  on = .(Unit, Category),
  `:=`(
    TH23   = i.TH23,
    PS23 = i.PS23
  )
]

antHarvest25[
  antHarvest22[, .(
    Unit,
    Category,
    TH22,
    PS22
  )],
  on = .(Unit, Category),
  `:=`(
    TH22   = i.TH22,
    PS22 = i.PS22
  )
]

#calculate three year average for percent success
antHarvest25[, threeyearsuccess :=
               rowMeans(
                 cbind(PS22, PS23, `Percent Success`),
                 na.rm = TRUE
               )
]

antHarvest25[, threeyearshunters :=
               rowMeans(
                 cbind(TH22, TH23, `Total Hunters`),
                 na.rm = TRUE
               )
]
antHarvest25$threeyearshunters<-round(antHarvest25$threeyearshunters,0)
antHarvest25$threeyearsuccess<-paste0(round(antHarvest25$threeyearsuccess,0), "%")

#Calculate 3 year trend for percent success
yearlist<-c(1,2,3)
antHarvest25[, slope :=
               apply(.SD, 1, function(y) {
                 if (sum(!is.na(y)) < 2) return(NA_real_)
                 coef(lm(y ~ yearlist))[2]
               }),
             .SDcols = c("PS22", "PS23", "Percent Success")
]


#Harvest table updates



antHarvest25$Unit<-as.numeric(antHarvest25$Unit)
antHarvest25$`Total Hunters`<-as.numeric(antHarvest25$`Total Hunters`)

antHarvest25<-merge(antHarvest25,PublicLandGMU,by="Unit",all=TRUE)

antHarvest25[,harvestunit:=paste(Unit,Category)]
antHarvest25$percent_public<-paste0(round(antHarvest25$percent_public,0), "%")





antHarvest25[,`Hunters Density Per Sq. Mile`:=round((`Total Hunters`/gmu_area_sqmi)*1000,1)]
antHarvest25[`Hunters Density Per Sq. Mile`==Inf, `Hunters Density Per Sq. Mile`:=NA]

antHarvest25[,`Hunters Density Per Public Sq. Mile`:=round((`Total Hunters`/public_sqmi)*1000,1)]
antHarvest25[`Hunters Density Per Public Sq. Mile`==Inf, `Hunters Density Per Public Sq. Mile`:=NA]
antHarvest25[public_sqmi<1,`Hunters Density Per Public Sq. Mile`:=NA]


antHarvest25[, c("Acres", "Acres Public","gmu_area_sqmi","public_sqmi") := lapply(.SD, round, 0), 
             .SDcols = c("Acres", "Acres Public","gmu_area_sqmi","public_sqmi")]

antHarvest25[, (c("Acres", "Acres Public","gmu_area_sqmi","public_sqmi")) := lapply(.SD, function(x) 
  format(x, big.mark = ",", scientific = FALSE)), .SDcols = c("Acres", "Acres Public","gmu_area_sqmi","public_sqmi")]

antHarvest25[,onx:=paste("https://webmap.onxmaps.com/hunt/map#9.45/",latitude,"/",longitude, sep="")]

#Add in the tags associated with each unit




antharvestcategories<- unique(antHarvest25$Category)





#ant Combination----
#Big Game Brochure mapping and reordering----
##Go long to wide for any duplicate Hunt Codes, consolidating the dates into one column

#remove unnecessary columns
GMUant26<-GMUant26[spp=="Pronghorn"]
GMUant26[,c('ID','spp','tblsort','row_cnt','MandatoryCWD','nr_lic_available','UNIT','...17','...22'):=NULL]

#remove comments from the list of units

# Extract numbers
GMUant26[, All_Units := trimws(regmatches(AUNIT, regexpr("(\\d+\\s*,\\s*)*\\d+", AUNIT)))]

# Extract text (everything NOT part of the number list)
GMUant26[, Text := trimws(gsub("(\\d+\\s*,\\s*)*\\d+", "", AUNIT))]

#remove all repeats now that the individual columns are gone
GMUant26<-unique(GMUant26)


#collapse dates from repeated units into one list of dates
GMUant26<-GMUant26[, .(DATES = paste(DATES, collapse = ",")), 
                   by = setdiff(names(GMUant26), "DATES")]
#combine the note columns and ignore NAs
collapse_nonempty <- function(...) {
  x <- unlist(list(...))
  x <- x[!is.na(x) & trimws(x) != ""]
  paste(x, collapse = ",")
}

GMUant26[, All_Notes :=
           collapse_nonempty(UnitNote, HuntcodeNote),
         by = 1:nrow(GMUant26)
]
#subset only the columns I want from GMUDeer26
GMUant26 <- GMUant26[
  , .(
    Tag = HUNT_CODE,
    Valid_GMUs = All_Units,
    Dates      = DATES,
    Notes      = All_Notes,
    List = LIST
  )
]


#Merge the useful columns from the GMU list into the DOA report
Fullant25 <- longDOAant25[
  GMUant26,
  on = "Tag"
]
# Combine the two list columns
Fullant25[, List := fcoalesce(i.List, List)]

# Remove the duplicate
Fullant25[, i.List := NULL]

#Add "New" designation
Fullant25[is.na(Class),New:="New"]
Fullant25[New=="New",Drawn_out_level:="New Code - No historical draw data"]
Fullant25[New=="New",Chance_at_DOL:="New Code - Unknown Chance"]
Fullant25[New=="New",Notes:="New 2026 Code"]

#add in a "High-demand" and "Hybrid" column to note the high demand tags
Fullant25[Tag%in%highdemand$'2025',HD:="High Demand"]
Fullant25[is.na(HD),HD:="No"]

Fullant25[Tag%in%hybrid$'CodeND',hybrid:="Hybrid"]
Fullant25[is.na(hybrid),hybrid:="No"]




Fullant25[grepl("P",SeasonCode),PLO:="Yes"]
Fullant25[!grepl("P",SeasonCode),PLO:="No"]

Fullant25[grepl("W",SeasonCode),RFW:="Yes"]
Fullant25[!grepl("W",SeasonCode),RFW:="No"]
Fullant25[Drawn_out_level=="No Apps" |Drawn_out_level== "None Drawn", NoApps:="Yes"]


#split out into long table by units

Fullant25_Longunits <- as.data.table(Fullant25 %>%
                                       separate_rows(Valid_GMUs, sep = ",\\s*") %>%   # split "1, 5, 3" into rows
                                       mutate(Valid_GMUs = as.integer(Valid_GMUs)))


setnames(Fullant25_Longunits,"Valid_GMUs","Unit")

#give designation for tags you don't need to burn points on
Fullant25_Longunits[grepl(c("Choice|Leftover"),Drawn_out_level), nopoints:="Y"]
Fullant25_Longunits[!grepl(c("Choice|Leftover"),Drawn_out_level), nopoints:="N"]




##Adding in HarvestUnit to map to harvest table in java----
# Start with a single merged table, but no harvestunit yet
Fullant25_LongunitsMerged <- copy(Fullant25_Longunits)
Fullant25_LongunitsMerged[, harvestunit := NA_character_]

# PLO
Fullant25_LongunitsMerged[
  grepl("P", SeasonWeapon),
  harvestunit := antHarvest25[Category == "All Private Land Only Seasons"][
    .SD, on = "Unit", x.harvestunit]
]

Fullant25_LongunitsMerged[is.na(harvestunit)&
                            grepl("P", SeasonWeapon) & grepl("A", Weapon),
                          harvestunit := antHarvest25[Category == "All Archery Seasons"][
                            .SD, on = "Unit", x.harvestunit]
]

Fullant25_LongunitsMerged[is.na(harvestunit)&
                            grepl("P5", SeasonWeapon) & grepl("R", Weapon),
                          harvestunit := antHarvest25[Category == "All Late Rifle Seasons"][
                            .SD, on = "Unit", x.harvestunit]
]

Fullant25_LongunitsMerged[is.na(harvestunit)&
                            grepl("P", SeasonWeapon) & grepl("R", Weapon),
                          harvestunit := antHarvest25[Category == "All Rifle Seasons"][
                            .SD, on = "Unit", x.harvestunit]
]

Fullant25_LongunitsMerged[is.na(harvestunit)&
                            grepl("P", SeasonWeapon) & grepl("R", Weapon),
                          harvestunit := antHarvest25[Category == "All Regular Rifle Seasons"][
                            .SD, on = "Unit", x.harvestunit]
]

Fullant25_LongunitsMerged[is.na(harvestunit)&
                            grepl("P", SeasonWeapon) & grepl("M", Weapon),
                          harvestunit := antHarvest25[Category == "All Muzzleloader Seasons"][
                            .SD, on = "Unit", x.harvestunit]
]
Fullant25_LongunitsMerged[is.na(harvestunit)&
                            grepl("P", SeasonWeapon),
                          harvestunit := antHarvest25[Category == "All Manners of Take"][
                            .SD, on = "Unit", x.harvestunit]
]

# RFW
Fullant25_LongunitsMerged[is.na(harvestunit)&
                            grepl("W", SeasonWeapon),
                          harvestunit := antHarvest25[Category == "All Ranching for Wildlife Seasons"][
                            .SD, on = "Unit", x.harvestunit]
]

Fullant25_LongunitsMerged[is.na(harvestunit)&
                            grepl("W", SeasonWeapon),
                          harvestunit := antHarvest25[Category == "All Rifle Seasons"][
                            .SD, on = "Unit", x.harvestunit]
]

# Archery
Fullant25_LongunitsMerged[is.na(harvestunit)&
                            Weapon == "A",
                          harvestunit := antHarvest25[Category == "All Archery Seasons"][
                            .SD, on = "Unit", x.harvestunit]
]

Fullant25_LongunitsMerged[is.na(harvestunit)&
                            Weapon == "A",
                          harvestunit := antHarvest25[Category == "All Manners of Take"][
                            .SD, on = "Unit", x.harvestunit]
]


#Muzzleloader
Fullant25_LongunitsMerged[is.na(harvestunit)&
                            Weapon == "M",
                          harvestunit := antHarvest25[Category == "All Muzzleloader Seasons"][
                            .SD, on = "Unit", x.harvestunit]
]

Fullant25_LongunitsMerged[is.na(harvestunit)&
                            Weapon == "M",
                          harvestunit := antHarvest25[Category == "All Manners of Take"][
                            .SD, on = "Unit", x.harvestunit]
]



#Rifle
Fullant25_LongunitsMerged[is.na(harvestunit)&
                            Weapon == "R" & grepl("L",SeasonWeapon),
                          harvestunit := antHarvest25[Category == "All Late Rifle Seasons"][
                            .SD, on = "Unit", x.harvestunit]
]

Fullant25_LongunitsMerged[is.na(harvestunit)&
                            Weapon == "R",
                          harvestunit := antHarvest25[Category == "All Regular Rifle Seasons"][
                            .SD, on = "Unit", x.harvestunit]
]

Fullant25_LongunitsMerged[is.na(harvestunit)&
                            Weapon == "R",
                          harvestunit := antHarvest25[Category == "All Rifle Seasons"][
                            .SD, on = "Unit", x.harvestunit]
]

#Final safety check to get all

Fullant25_LongunitsMerged[is.na(harvestunit),
                          harvestunit := antHarvest25[Category == "All Manners of Take"][
                            .SD, on = "Unit", x.harvestunit]]
Fullant25_LongunitsMerged[is.na(harvestunit),
                          harvestunit := "No Harvest Stats Available"]


#collapse harvestunit into a list of harvest units
collapse_cols <- c("Unit","harvestunit")
group_cols <- setdiff(names(Fullant25_LongunitsMerged), collapse_cols)

Fullant25_wideunits <- Fullant25_LongunitsMerged[, lapply(.SD, function(x) paste(x, collapse=",")),
                                                 by = group_cols, .SDcols = collapse_cols]

setnames(Fullant25_wideunits,"Unit","Valid GMUs")
###Add list of tags to the harvest stats----
tags_by_unit <- Fullant25_Longunits[
  , .(Tags = paste(unique(Tag), collapse=",")), 
  by = Unit
]

antHarvest25 <- merge(
  antHarvest25, 
  tags_by_unit, 
  by = "Unit", 
  all.x = TRUE
)
##Remove NA units
antHarvest25<-antHarvest25[!is.na(Unit)]


#ant add in public land. Add up the total public land in each unit
# Step 1: Split Numbers column into separate rows
Fullant25_Long <- as.data.table(Fullant25_wideunits %>%
                                  separate_rows(`Valid GMUs`, sep = ",\\s*") %>%   # split "1, 5, 3" into rows
                                  mutate(`Valid GMUs` = as.integer(`Valid GMUs`)))

# Step 2: Join with df2 to get percentages
Fullant25_Long_joined <- Fullant25_Long %>%
  left_join(PublicLandGMU, by = c("Valid GMUs" = "Unit"))



# # Step 3: Average percentages back per ID
# Fullant25Final <- Fullant25_Long_joined %>%
#   group_by(Tag) %>%
#   summarise(
#     'Valid GMUs' = paste('Valid GMUs', collapse = ", "),
#     Total_Acres = sum(Acres, na.rm = TRUE),
#     Public_Acres = sum(`Acres Public`, na.rm = TRUE),
#     )

# Step 3a: Compute the summaries
summaries <- as.data.table(
  Fullant25_Long_joined %>%
    group_by(Tag,Class) %>%
    summarise(
      # `Valid GMUs` = paste(`Valid GMUs`, collapse = ", "),
      Total_Acres = sum(Acres, na.rm = TRUE),
      Public_Acres = sum(`Acres Public`, na.rm = TRUE),
      .groups = "drop"
    )
)

# Step 3b: Join back to the original (all columns preserved)
Fullant25Final <- Fullant25_wideunits %>%
  left_join(summaries, by = c('Tag','Class'))
#calculate percent public land
Fullant25Final[,Public_Percent:= paste0(round((Public_Acres / Total_Acres) * 100, 1), "%")]

Fullant25Final[, c("Total_Acres","Public_Acres") := lapply(.SD, round, 0), .SDcols = c("Total_Acres","Public_Acres")]


##Add in drawn out data from previous years
Fullant25Final[Antdraw24, `:=`(
  Drawn_out_level24 = i.Drawn_out_level,
  Chance_at_DOL24   = i.Chance_at_DOL
), on = c("Tag","Class")]

Fullant25Final[Antdraw23, `:=`(
  Drawn_out_level23 = i.Drawn_out_level,
  Chance_at_DOL23   = i.Chance_at_DOL
), on = c("Tag","Class")]



##Add in a way to view trend of draw----
#switch chance at DOL to a decimal and add it to the actual level so the drawn out level can be entirely numeric
Fullant25Final[, Chance_at_DOLN := 1-(as.numeric(gsub("%", "", Chance_at_DOL)) / 100)]
Fullant25Final[Drawn_out_level=="Choice 2",DOL:=-1]
Fullant25Final[Drawn_out_level=="Choice 3",DOL:=-2]
Fullant25Final[Drawn_out_level=="Choice 4",DOL:=-3]
Fullant25Final[Drawn_out_level %in% "Leftover",DOL:=-4]
Fullant25Final[,DOLE25:=DOL+Chance_at_DOLN]

Fullant25Final[, Chance_at_DOLN24 := 1-(as.numeric(gsub("%", "", Chance_at_DOL24)) / 100)]
Fullant25Final[Drawn_out_level24=="Choice 2",DOL24:=-1]
Fullant25Final[Drawn_out_level24=="Choice 3",DOL24:=-2]
Fullant25Final[Drawn_out_level24=="Choice 4",DOL24:=-3]
Fullant25Final[Drawn_out_level24=="Leftover",DOL24:=-4]
Fullant25Final[,DOLE24:=DOL24+Chance_at_DOLN24]

Fullant25Final[, Chance_at_DOLN23 := 1-(as.numeric(gsub("%", "", Chance_at_DOL23)) / 100)]
Fullant25Final[Drawn_out_level23=="Choice 2",DOL23:=-1]
Fullant25Final[Drawn_out_level23=="Choice 3",DOL23:=-2]
Fullant25Final[Drawn_out_level23=="Choice 4",DOL23:=-3]
Fullant25Final[Drawn_out_level23 %in% "Leftover",DOL3:=-4]
Fullant25Final[,DOLE23:=DOL23+Chance_at_DOLN23]

#calculate slope and predicted value
x <- 1:3

Fullant25Final[, c("slope", "predictedDrawnOut") := {
  
  y <- c(DOLE23, DOLE24, DOLE25)
  
  if (all(is.na(y))) {
    list(NA_real_, NA_real_)
  } else {
    fit <- lm(y ~ x)
    slope <- coef(fit)[2]
    predictedDrawnOut <- coef(fit)[1] + slope * 4
    list(slope, predictedDrawnOut)
  }
  
}, by = 1:nrow(Fullant25Final)]

Fullant25Final[predictedDrawnOut>0, `:=`(
  PredPrefPoints = paste0(floor(predictedDrawnOut), " Preference Points"),   # integer + text
  Predpercent = 1-(predictedDrawnOut - floor(predictedDrawnOut)),                       # decimal only
  PredPrefPercent = paste0(round(((1-(predictedDrawnOut - floor(predictedDrawnOut))) * 100)), "%")  # percent string
)]

Fullant25Final[predictedDrawnOut<=0 & predictedDrawnOut>-1, `:=`(
  PredPrefPoints = "Choice 2",  
  Predpercent = -(floor(predictedDrawnOut)- predictedDrawnOut),                       # decimal only
  PredPrefPercent = paste0(round(-(floor(predictedDrawnOut)-predictedDrawnOut) * 100), "%")  # percent string
)]

Fullant25Final[predictedDrawnOut<=-1 & predictedDrawnOut>-2, `:=`(
  PredPrefPoints = "Choice 3",  
  Predpercent = -(floor(predictedDrawnOut)- predictedDrawnOut),                       # decimal only
  PredPrefPercent = paste0(round(-(floor(predictedDrawnOut)-predictedDrawnOut) * 100), "%")  # percent string
)]

Fullant25Final[predictedDrawnOut<=-2 & predictedDrawnOut>-3, `:=`(
  PredPrefPoints = "Choice 4",  
  Predpercent = -(floor(predictedDrawnOut)- predictedDrawnOut),                       # decimal only
  PredPrefPercent = paste0(round(-(floor(predictedDrawnOut)-predictedDrawnOut) * 100), "%")  # percent string
)]

Fullant25Final[predictedDrawnOut<=-3, `:=`(
  PredPrefPoints = "Leftover",  
  Predpercent = -(floor(predictedDrawnOut)- predictedDrawnOut),                       # decimal only
  PredPrefPercent = paste0(round(-(floor(predictedDrawnOut)-predictedDrawnOut) * 100), "%")  # percent string
)]
#remove OTC tags
Fullant25Final<-Fullant25Final[!grepl("U",SeasonWeapon)]
#remove lines with non-res codes for residents
Fullant25Final <- Fullant25Final[
  !(grepl("V", SeasonCode) & Class %in% c("A_R", "Y_R", "L_R", "L_U"))
]


##Add OTC designation for harvest stats and split out OTC table----
ArcheryEitherSexAntOTC<-c(7, 8, 9, 14, 15, 19, 20, 22,
                          23, 24, 25, 26, 29, 31, 32, 33,
                          34, 35, 36, 38, 39, 40, 42,
                          43, 44, 45, 46, 47, 51, 52,
                          53, 54, 59, 60, 61, 63, 64,
                          65, 69, 71, 72, 73, 74, 75, 76,
                          77, 78, 84, 85, 86, 89, 90, 91,
                          92, 93, 94, 95, 96, 97, 98, 99,
                          100, 101, 102, 104, 105, 106,
                          107, 109, 110, 111, 112, 113,
                          114, 115, 116, 117, 118, 119,
                          120, 121, 122, 123, 124, 125,
                          126, 127, 128, 129, 130, 132,
                          133, 134, 135, 136, 137, 138,
                          139, 140, 141, 142, 143, 144,
                          145, 146, 147, 191, 231, 361,
                          371, 421, 431, 444, 461,
                          471, 511, 521, 591, 691, 711,
                          741, 751, 771, 851, 861, 951)

antOTCHarvest24<-copy(antHarvest25)
antOTCHarvest24[Category=="All Archery Seasons" & Unit%in%ArcheryEitherSexAntOTC,
             OTCCat:="Either Sex OTC Archery"]
antOTCHarvest24[OTCCat=="Either Sex OTC Archery",OTC:="ESA"]
antOTCHarvest24[!is.na(OTCCat)]

#writeCSVs Antelope----
write_csv(Fullant25Final, file='Website/Fullant26FinalTest.csv')
write_csv(antHarvest25,'Website/antHarvest25Test.csv')
write_csv(antOTCHarvest24,'Website/antOTC24Test.csv')











##HuntCodeDrawRecap-Pages----

drawrecapdeer25<-"Raw data/draw_recap_reports/2025_Deer_DR.pdf"
drawrecapelk25<-"Raw data/draw_recap_reports/2025_Elk_DR.pdf"
drawrecapant25<-"Raw data/draw_recap_reports/2025_Ant_DR.pdf"
brochure2025<-pdf_text("Colorado Big Game Hunting Brochure.pdf")
text<-paste(brochure2025, collapse = "\n")
writeLines(text, "Colorado_Big_Game_Brochure_FullText.txt")

#draw recap mapping page numbers

drawrecapdeer25_text<-pdf_text(drawrecapdeer25)
drawrecapelk25_text<-pdf_text(drawrecapelk25)
drawrecapant25_text<-pdf_text(drawrecapant25)


# Create data frame with page numbers
#for deer
hunt_codes <- lapply(seq_along(drawrecapdeer25_text), 
                     function(i) {
                       page_text <- drawrecapdeer25_text[[i]]
                       
                       codes <- str_extract_all(page_text, "[A-Z]{2}[0-9]{3}[A-Z][0-9][A-Z]")[[1]]
                       
                       if (length(codes) > 0) {
                         data.frame(
                           HuntCode = codes,
                           Page = i,
                           stringsAsFactors = FALSE
                         )
                       } else {
                         NULL
                       }
                     })

deer25code_pages <- as.data.table(bind_rows(hunt_codes)) %>%
  distinct() %>%   # remove duplicates
  arrange(HuntCode, Page)
write_csv(deer25code_pages, "deer25code_pages.csv")

#for elk
elkhunt_codes <- lapply(seq_along(drawrecapelk25_text), 
                        function(i) {
                          page_text <- drawrecapelk25_text[[i]]
                          
                          codes <- str_extract_all(page_text, "[A-Z]{2}[0-9]{3}[A-Z][0-9][A-Z]")[[1]]
                          
                          if (length(codes) > 0) {
                            data.frame(
                              HuntCode = codes,
                              Page = i,
                              stringsAsFactors = FALSE
                            )
                          } else {
                            NULL
                          }
                        })

elk25code_pages <- as.data.table(bind_rows(elkhunt_codes)) %>%
  distinct() %>%   # remove duplicates
  arrange(HuntCode, Page)
write_csv(elk25code_pages, "elk25code_pages.csv")

#for antelope
anthunt_codes <- lapply(seq_along(drawrecapant25_text), 
                        function(i) {
                          page_text <- drawrecapant25_text[[i]]
                          
                          codes <- str_extract_all(page_text, "[A-Z]{2}[0-9]{3}[A-Z][0-9][A-Z]")[[1]]
                          
                          if (length(codes) > 0) {
                            data.frame(
                              HuntCode = codes,
                              Page = i,
                              stringsAsFactors = FALSE
                            )
                          } else {
                            NULL
                          }
                        })

ant25code_pages <- as.data.table(bind_rows(anthunt_codes)) %>%
  distinct() %>%   # remove duplicates
  arrange(HuntCode, Page)
write_csv(ant25code_pages, "ant25code_pages.csv")


