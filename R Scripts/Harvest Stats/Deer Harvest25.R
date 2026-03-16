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
DHarvestRaw <- as.data.table(read_excel("Raw data/Harvest_and_public/NewFormat/StartingData/2025 Deer Statewide Harvest Final.xlsx"))

##Remove all the empty NA columns
DHarvestRaw[, names(DHarvestRaw)[
  sapply(DHarvestRaw, function(x) all(is.na(x) | x == FALSE | x== TRUE))
] := NULL]

#Remove the rows with headers
DHarvestRaw<-DHarvestRaw[!grepl("Harvest|Hunt Code|Total",Tag)|!is.na(Tag)]

#Remove the * and put in a second column to indicate a low response
DHarvestRaw[nchar(Tag) == 9, `:=`(
  LowResponse = substr(Tag, 9, 9),
  Tag = substr(Tag, 1, 8)
)]

DHarvestRaw[LowResponse=="*",LowResponse:="Yes"]
DHarvestRaw[is.na(LowResponse),LowResponse:="No"]



write_xlsx(DHarvestRaw,'Raw data/Harvest_and_public/NewFormat/DeerHarvest25Final.xlsx')

