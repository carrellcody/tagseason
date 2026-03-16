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

#Drawn out for Deer 2 ----
## Load spreadsheets
DOADeer24 <- as.data.table(read_excel("Raw data/Drawn out at reports and exported Excel sheets/DeerDrawnOut2024.xlsx"))
DOADeer24CPW<-as.data.table(read_excel("Raw data/CPW/2025 Min Pref Points Extract final.xlsx", 
                                       sheet = "Deer"))


##remove any empty rows
DOADeer24<-DOADeer24[!is.na(...3)]
##change names of columns
setnames(DOADeer24,names(DOADeer24),c('Tag','List','title','DOAAdultR','DOAAdultNR','DOAYouthR','DOAYouthNR','DOALandUnRes','DOALandRes'))
##Forward fill the codes into the both rows

DOADeer24[, Tag := na.locf(Tag, na.rm = FALSE)]
DOADeer24[is.na(List),num:=1]
DOADeer24[!is.na(List),num:=2]
nam<-names(DOADeer24)
##remove the names of the variables  we're going to split wide on and any extra unnecessary column
nam<-nam[-1]
nam<-nam[-9]
##Fix the ones where the tag # was listed twice due to falling on page line. This will be specific for each year
DOADeer24<-unique(DOADeer24)
Duplicate2<-DOADeer24[num==2]
Duplicate1<-DOADeer24[num==1]

DOADeerdups2<-unique(Duplicate2[duplicated(Tag) | duplicated(Tag, fromLast=TRUE)]$Tag)
DOADeerdups1<-unique(Duplicate1[duplicated(Tag) | duplicated(Tag, fromLast=TRUE)]$Tag)

DOADeer24<-DOADeer24[(Tag==DOADeerdups1 | Tag==DOADeerdups2) & title=="Drawn Out At", num:=2]
DOADeer24<-DOADeer24[(Tag==DOADeerdups1| Tag==DOADeerdups2) & title=="# Drawn at Final Level", num:=1]

DOADeer24 <- DOADeer24[!duplicated(DOADeer24, by = c("Tag","num"))]



##Split wide using dcast
DOADeer24 <- data.table::dcast(
  DOADeer24,
  Tag ~ num,
  value.var = nam
)

DOADeer24[,List_1:=NULL]
setnames(DOADeer24, "List_2", "List")
##Split and add columns to calculate chances
# NNames<-names(DOADeer24)
# NNames<-NNames[-c(1:7)]
# NNames<-NNames[-c(31:41)]

setnames(DOADeer24, c("DOAAdultR_2",'DOAAdultNR_2',"DOAYouthR_2","DOAYouthNR_2","DOALandUnRes_2","DOALandRes_2"),
         c("A_R_drawn_out_level","A_NR_drawn_out_level","Y_R_drawn_out_level","Y_NR_drawn_out_level","L_U_drawn_out_level","L_R_drawn_out_level"))

##Split the x of y column into x and y columns and ad a ratio.
DOADeer24[, c("A_R_successful_apps_at_DOL", "A_R_total_apps_at_DOL") := tstrsplit(DOAAdultR_1, "of", fixed = TRUE)]
DOADeer24[, c("A_NR_successful_apps_at_DOL", "A_NR_total_apps_at_DOL") := tstrsplit(DOAAdultNR_1, "of", fixed = TRUE)]
DOADeer24[, c("Y_R_successful_apps_at_DOL", "Y_R_total_apps_at_DOL") := tstrsplit(DOAYouthR_1, "of", fixed = TRUE)]
DOADeer24[, c("Y_NR_successful_apps_at_DOL", "Y_NR_total_apps_at_DOL") := tstrsplit(DOAYouthNR_1, "of", fixed = TRUE)]
DOADeer24[, c("L_U_successful_apps_at_DOL", "L_U_total_apps_at_DOL") := tstrsplit(DOALandUnRes_1, "of", fixed = TRUE)]
DOADeer24[, c("L_R_successful_apps_at_DOL", "L_R_total_apps_at_DOL") := tstrsplit(DOALandRes_1, "of", fixed = TRUE)]

# Convert to numeric
DOADeer24$A_R_successful_apps_at_DOL<-as.numeric(DOADeer24$A_R_successful_apps_at_DOL)
DOADeer24$A_R_total_apps_at_DOL<-as.numeric(DOADeer24$A_R_total_apps_at_DOL)

DOADeer24$A_NR_successful_apps_at_DOL<-as.numeric(DOADeer24$A_NR_successful_apps_at_DOL)
DOADeer24$A_NR_total_apps_at_DOL<-as.numeric(DOADeer24$A_NR_total_apps_at_DOL)

DOADeer24$Y_R_successful_apps_at_DOL<-as.numeric(DOADeer24$Y_R_successful_apps_at_DOL)
DOADeer24$Y_R_total_apps_at_DOL<-as.numeric(DOADeer24$Y_R_total_apps_at_DOL)

DOADeer24$Y_NR_successful_apps_at_DOL<-as.numeric(DOADeer24$Y_NR_successful_apps_at_DOL)
DOADeer24$Y_NR_total_apps_at_DOL<-as.numeric(DOADeer24$Y_NR_total_apps_at_DOL)

DOADeer24$L_R_successful_apps_at_DOL<-as.numeric(DOADeer24$L_R_successful_apps_at_DOL)
DOADeer24$L_R_total_apps_at_DOL<-as.numeric(DOADeer24$L_R_total_apps_at_DOL)

DOADeer24$L_U_successful_apps_at_DOL<-as.numeric(DOADeer24$L_U_successful_apps_at_DOL)
DOADeer24$L_U_total_apps_at_DOL<-as.numeric(DOADeer24$L_U_total_apps_at_DOL)

# Add ratio column
DOADeer24[,"A_R_chance_at_DOL"  :=  A_R_successful_apps_at_DOL/ A_R_total_apps_at_DOL]
DOADeer24[,"A_NR_chance_at_DOL"  :=  A_NR_successful_apps_at_DOL/ A_NR_total_apps_at_DOL]
DOADeer24[,"Y_R_chance_at_DOL"  :=  Y_R_successful_apps_at_DOL/ Y_R_total_apps_at_DOL]
DOADeer24[,"Y_NR_chance_at_DOL"  :=  Y_NR_successful_apps_at_DOL/ Y_NR_total_apps_at_DOL]
DOADeer24[,"L_U_chance_at_DOL"  :=  L_U_successful_apps_at_DOL/ L_U_total_apps_at_DOL]
DOADeer24[,"L_R_chance_at_DOL"  :=  L_R_successful_apps_at_DOL/ L_R_total_apps_at_DOL]

#ratio column (Chance at DOL) should equal 100% (1) if the Drawn out level is "Leftover"
DOADeer24[A_R_drawn_out_level=="Leftover",
          A_R_chance_at_DOL:=1]
DOADeer24[A_NR_drawn_out_level=="Leftover",
          A_NR_chance_at_DOL:=1]
DOADeer24[Y_NR_drawn_out_level=="Leftover",
          Y_NR_chance_at_DOL:=1]
DOADeer24[Y_R_drawn_out_level=="Leftover",
          Y_R_chance_at_DOL:=1]
DOADeer24[L_U_drawn_out_level=="Leftover",
          L_U_chance_at_DOL:=1]
DOADeer24[L_R_drawn_out_level=="Leftover",
          L_R_chance_at_DOL:=1]


#Specify chances with first choice
##If leftover or choice 2/3/4 it's 100% chance
DOADeer24[A_R_drawn_out_level=="Leftover"|
            grepl("Choice",A_R_drawn_out_level),
          A_R_chance_with_first_choice:=1]
DOADeer24[A_NR_drawn_out_level=="Leftover"|
            grepl("Choice",A_NR_drawn_out_level),
          A_NR_chance_with_first_choice:=1]
DOADeer24[Y_NR_drawn_out_level=="Leftover"|
            grepl("Choice",Y_NR_drawn_out_level),
          Y_NR_chance_with_first_choice:=1]
DOADeer24[Y_R_drawn_out_level=="Leftover"|
            grepl("Choice",Y_R_drawn_out_level),
          Y_R_chance_with_first_choice:=1]
DOADeer24[L_U_drawn_out_level=="Leftover"|
            grepl("Choice",L_U_drawn_out_level),
          L_U_chance_with_first_choice:=1]
DOADeer24[L_R_drawn_out_level!="Leftover"|
            grepl("Choice",L_R_drawn_out_level),
          L_R_chance_with_first_choice:=1]
#if not leftover or choice 2/3/4, it's the same as the chance at drawn out level
DOADeer24[A_R_drawn_out_level!="Leftover"|
            grepl("Choice",A_R_drawn_out_level),
          A_R_chance_with_first_choice:=A_R_chance_at_DOL]
DOADeer24[A_NR_drawn_out_level!="Leftover"|
            A_NR_drawn_out_level%!in%"Choice",
          A_NR_chance_with_first_choice:=A_NR_chance_at_DOL]
DOADeer24[Y_NR_drawn_out_level!="Leftover"|
            Y_NR_drawn_out_level%!in%"Choice",
          Y_NR_chance_with_first_choice:=Y_NR_chance_at_DOL]
DOADeer24[Y_R_drawn_out_level!="Leftover"|
            Y_R_drawn_out_level%!in%"Choice",
          Y_R_chance_with_first_choice:=Y_R_chance_at_DOL]
DOADeer24[L_U_drawn_out_level!="Leftover"|
            L_U_drawn_out_level%!in%"Choice",
          L_U_chance_with_first_choice:=L_U_chance_at_DOL]
DOADeer24[L_R_drawn_out_level!="Leftover"|
            L_R_drawn_out_level%!in%"Choice",
          L_R_chance_with_first_choice:=L_R_chance_at_DOL]
longDOADeer24 <- data.table::melt(
  DOADeer24,
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
longDOADeer24[, Class := ids[Class]]

#Remove unneccesary columns
longDOADeer24<-longDOADeer24[,(3:10) :=NULL]


# Split out the tag into "Season, weapon, sex, etc."
longDOADeer24[, `:=`(
  Animal = substr(Tag, 1, 1),   # D, A
  Sex = substr(Tag, 2, 2),   # E, B
  UnitCode = substr(Tag, 3, 5),   # 005, 123
  SeasonCode = substr(Tag, 6, 7),   # O2, X9
  Weapon = substr(Tag, 8, 8),    # R, Z
  SeasonWeapon = substr(Tag, 6, 8 ) #O2R
)]



#round off number

longDOADeer24[,c("Chance_at_DOL","Chance_with_First_choice"):= lapply(.SD, round, 2), .SDcols = c("Chance_at_DOL","Chance_with_First_choice")]

#convert to percentages

longDOADeer24$Chance_at_DOL<-percent(longDOADeer24$Chance_at_DOL)
longDOADeer24$Chance_with_First_choice<-percent(longDOADeer24$Chance_with_First_choice)

#Add column for DOA PP that's only numeric
longDOADeer24[grepl(c("Choice|Leftover"), Drawn_out_level), DOL:=0]

longDOADeer24<-longDOADeer24[is.na(DOL),DOL := as.integer(sub("^(\\d+).*", "\\1", Drawn_out_level))]

longDOADeer24<-longDOADeer24[is.na(DOL),DOL:=0]

write_xlsx(longDOADeer24,'Raw data/Drawn out at reports and exported Excel sheets/organizedDOASheets/DeerDraw24.xlsx')
