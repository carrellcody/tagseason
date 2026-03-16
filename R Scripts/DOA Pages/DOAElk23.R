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
source('~/github/R-General/.Rprofile')



DOAelk23 <- as.data.table(read_excel("Raw data/Drawn out at reports and exported Excel sheets/elkDrawnOut2023.xlsx"))



##remove any empty rows
DOAelk23<-DOAelk23[!is.na(...3)]
##change names of columns
setnames(DOAelk23,names(DOAelk23),c('Tag','List','title','DOAAdultR','DOAAdultNR','DOAYouthR','DOAYouthNR','DOALandUnRes','DOALandRes'))

##Forward fill the codes into the both rows

DOAelk23[, Tag := na.locf(Tag, na.rm = FALSE)]
DOAelk23[is.na(List),num:=1]
DOAelk23[!is.na(List),num:=2]
nam<-names(DOAelk23)
##remove the names of the variables  we're going to split wide on and any extra unnecessary column
nam<-nam[-1]

nam<-nam[-9]
##Fix the ones where the tag # was listed twice due to falling on page line. This will be specific for each year
DOAelk23<-unique(DOAelk23)
Duplicate2<-DOAelk23[num==2]
Duplicate1<-DOAelk23[num==1]

DOAelkdups2<-unique(Duplicate2[duplicated(Tag) | duplicated(Tag, fromLast=TRUE)]$Tag)
DOAelkdups1<-unique(Duplicate1[duplicated(Tag) | duplicated(Tag, fromLast=TRUE)]$Tag)

DOAelk23<-DOAelk23[(Tag==DOAelkdups1 | Tag==DOAelkdups2) & title=="Drawn Out At", num:=2]
DOAelk23<-DOAelk23[(Tag==DOAelkdups1| Tag==DOAelkdups2) & title=="# Drawn at Final Level", num:=1]

DOAelk23 <- DOAelk23[!duplicated(DOAelk23, by = c("Tag","num"))]



##Split wide using dcast
DOAelk23 <- data.table::dcast(
  DOAelk23,
  Tag ~ num,
  value.var = nam
)
DOAelk23[,List_1:=NULL]
setnames(DOAelk23, "List_2", "List")
##Split and add columns to calculate chances
# NNames<-names(DOAelk23)
# NNames<-NNames[-c(1:7)]
# NNames<-NNames[-c(31:41)]

setnames(DOAelk23, c("DOAAdultR_2",'DOAAdultNR_2',"DOAYouthR_2","DOAYouthNR_2","DOALandUnRes_2","DOALandRes_2"),
         c("A_R_drawn_out_level","A_NR_drawn_out_level","Y_R_drawn_out_level","Y_NR_drawn_out_level","L_U_drawn_out_level","L_R_drawn_out_level"))

##Split the x of y column into x and y columns and ad a ratio.
DOAelk23[, c("A_R_successful_apps_at_DOL", "A_R_total_apps_at_DOL") := tstrsplit(DOAAdultR_1, "of", fixed = TRUE)]
DOAelk23[, c("A_NR_successful_apps_at_DOL", "A_NR_total_apps_at_DOL") := tstrsplit(DOAAdultNR_1, "of", fixed = TRUE)]
DOAelk23[, c("Y_R_successful_apps_at_DOL", "Y_R_total_apps_at_DOL") := tstrsplit(DOAYouthR_1, "of", fixed = TRUE)]
DOAelk23[, c("Y_NR_successful_apps_at_DOL", "Y_NR_total_apps_at_DOL") := tstrsplit(DOAYouthNR_1, "of", fixed = TRUE)]
DOAelk23[, c("L_U_successful_apps_at_DOL", "L_U_total_apps_at_DOL") := tstrsplit(DOALandUnRes_1, "of", fixed = TRUE)]
DOAelk23[, c("L_R_successful_apps_at_DOL", "L_R_total_apps_at_DOL") := tstrsplit(DOALandRes_1, "of", fixed = TRUE)]

# Convert to numeric
DOAelk23$A_R_successful_apps_at_DOL<-as.numeric(DOAelk23$A_R_successful_apps_at_DOL)
DOAelk23$A_R_total_apps_at_DOL<-as.numeric(DOAelk23$A_R_total_apps_at_DOL)

DOAelk23$A_NR_successful_apps_at_DOL<-as.numeric(DOAelk23$A_NR_successful_apps_at_DOL)
DOAelk23$A_NR_total_apps_at_DOL<-as.numeric(DOAelk23$A_NR_total_apps_at_DOL)

DOAelk23$Y_R_successful_apps_at_DOL<-as.numeric(DOAelk23$Y_R_successful_apps_at_DOL)
DOAelk23$Y_R_total_apps_at_DOL<-as.numeric(DOAelk23$Y_R_total_apps_at_DOL)

DOAelk23$Y_NR_successful_apps_at_DOL<-as.numeric(DOAelk23$Y_NR_successful_apps_at_DOL)
DOAelk23$Y_NR_total_apps_at_DOL<-as.numeric(DOAelk23$Y_NR_total_apps_at_DOL)

DOAelk23$L_R_successful_apps_at_DOL<-as.numeric(DOAelk23$L_R_successful_apps_at_DOL)
DOAelk23$L_R_total_apps_at_DOL<-as.numeric(DOAelk23$L_R_total_apps_at_DOL)

DOAelk23$L_U_successful_apps_at_DOL<-as.numeric(DOAelk23$L_U_successful_apps_at_DOL)
DOAelk23$L_U_total_apps_at_DOL<-as.numeric(DOAelk23$L_U_total_apps_at_DOL)

# Add ratio column
DOAelk23[,"A_R_chance_at_DOL"  :=  A_R_successful_apps_at_DOL/ A_R_total_apps_at_DOL]
DOAelk23[,"A_NR_chance_at_DOL"  :=  A_NR_successful_apps_at_DOL/ A_NR_total_apps_at_DOL]
DOAelk23[,"Y_R_chance_at_DOL"  :=  Y_R_successful_apps_at_DOL/ Y_R_total_apps_at_DOL]
DOAelk23[,"Y_NR_chance_at_DOL"  :=  Y_NR_successful_apps_at_DOL/ Y_NR_total_apps_at_DOL]
DOAelk23[,"L_U_chance_at_DOL"  :=  L_U_successful_apps_at_DOL/ L_U_total_apps_at_DOL]
DOAelk23[,"L_R_chance_at_DOL"  :=  L_R_successful_apps_at_DOL/ L_R_total_apps_at_DOL]

#ratio column (Chance at DOL) should equal 100% (1) if the Drawn out level is "Leftover"
DOAelk23[A_R_drawn_out_level=="Leftover",
         A_R_chance_at_DOL:=1]
DOAelk23[A_NR_drawn_out_level=="Leftover",
         A_NR_chance_at_DOL:=1]
DOAelk23[Y_NR_drawn_out_level=="Leftover",
         Y_NR_chance_at_DOL:=1]
DOAelk23[Y_R_drawn_out_level=="Leftover",
         Y_R_chance_at_DOL:=1]
DOAelk23[L_U_drawn_out_level=="Leftover",
         L_U_chance_at_DOL:=1]
DOAelk23[L_R_drawn_out_level=="Leftover",
         L_R_chance_at_DOL:=1]


#Specify chances with first choice
##If leftover or choice 2/3/4 it's 100% chance
DOAelk23[A_R_drawn_out_level=="Leftover"|
           grepl("Choice",A_R_drawn_out_level),
         A_R_chance_with_first_choice:=1]
DOAelk23[A_NR_drawn_out_level=="Leftover"|
           grepl("Choice",A_NR_drawn_out_level),
         A_NR_chance_with_first_choice:=1]
DOAelk23[Y_NR_drawn_out_level=="Leftover"|
           grepl("Choice",Y_NR_drawn_out_level),
         Y_NR_chance_with_first_choice:=1]
DOAelk23[Y_R_drawn_out_level=="Leftover"|
           grepl("Choice",Y_R_drawn_out_level),
         Y_R_chance_with_first_choice:=1]
DOAelk23[L_U_drawn_out_level=="Leftover"|
           grepl("Choice",L_U_drawn_out_level),
         L_U_chance_with_first_choice:=1]
DOAelk23[L_R_drawn_out_level!="Leftover"|
           grepl("Choice",L_R_drawn_out_level),
         L_R_chance_with_first_choice:=1]
#if not leftover or choice 2/3/4, it's the same as the chance at drawn out level
DOAelk23[A_R_drawn_out_level!="Leftover"|
           !grepl("Choice",A_R_drawn_out_level),
         A_R_chance_with_first_choice:=A_R_chance_at_DOL]
DOAelk23[A_NR_drawn_out_level!="Leftover"|
           !grepl("Choice",A_NR_drawn_out_level),
         A_NR_chance_with_first_choice:=A_NR_chance_at_DOL]
DOAelk23[Y_NR_drawn_out_level!="Leftover"|
           !grepl("Choice",Y_NR_drawn_out_level),
         Y_NR_chance_with_first_choice:=Y_NR_chance_at_DOL]
DOAelk23[Y_R_drawn_out_level!="Leftover"|
           !grepl("Choice",Y_R_drawn_out_level),
         Y_R_chance_with_first_choice:=Y_R_chance_at_DOL]
DOAelk23[L_U_drawn_out_level!="Leftover"|
           !grepl("Choice",L_U_drawn_out_level),
         L_U_chance_with_first_choice:=L_U_chance_at_DOL]
DOAelk23[L_R_drawn_out_level!="Leftover"|
           !grepl("Choice",L_R_drawn_out_level),
         L_R_chance_with_first_choice:=L_R_chance_at_DOL]


#Go long and split out each class into rows


# longDOAelk23 <- data.table::melt(
#   DOAelk23,
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
longDOAelk23 <- data.table::melt(
  DOAelk23,
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
longDOAelk23[, Class := ids[Class]]

#Remove unneccesary columns
longDOAelk23<-longDOAelk23[,(3:10) :=NULL]


# Split out the tag into "Season, weapon, sex, etc."
longDOAelk23[, `:=`(
  Animal = substr(Tag, 1, 1),   # D, A
  Sex = substr(Tag, 2, 2),   # E, B
  UnitCode = substr(Tag, 3, 5),   # 005, 123
  SeasonCode = substr(Tag, 6, 7),   # O2, X9
  Weapon = substr(Tag, 8, 8),    # R, Z
  SeasonWeapon = substr(Tag, 6, 8 ) #O2R
)]



#round off number

longDOAelk23[,c("Chance_at_DOL","Chance_with_First_choice"):= lapply(.SD, round, 2), .SDcols = c("Chance_at_DOL","Chance_with_First_choice")]

#convert to percentages

longDOAelk23$Chance_at_DOL<-percent(longDOAelk23$Chance_at_DOL)
longDOAelk23$Chance_with_First_choice<-percent(longDOAelk23$Chance_with_First_choice)

#Add column for DOA PP that's only numeric
longDOAelk23[grepl(c("Choice|Leftover"), Drawn_out_level), DOL:=0]

longDOAelk23<-longDOAelk23[is.na(DOL),DOL := as.integer(sub("^(\\d+).*", "\\1", Drawn_out_level))]

longDOAelk23<-longDOAelk23[is.na(DOL),DOL:=0]

write_xlsx(longDOAelk23,'Raw data/Drawn out at reports and exported Excel sheets/organizedDOASheets/ElkDraw23.xlsx')

