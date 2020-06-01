## ---------------------------
##
## Script name: NAICS filter
##
## Purpose of script: Downloads all NAICS codes for 2017 and filters them according to 
##    the data we already have
##
## Author: George A. Maynard
##
## Date Created: 2020-06-01
##
## Copyright (c) George A. Maynard, 2020
## Email: galphonsemaynard@gmail.com
##
## ---------------------------
##
## Notes:
##   
##
## ---------------------------

## set working directory
## should already be set to the github repository
## ---------------------------

## set options
options(scipen = 6, digits = 4)
## ---------------------------

## load up the packages we will need:  
library(XLConnect)
library(dplyr)
## ---------------------------

## load up our functions into memory

## ---------------------------

## Read in the 2017 NAICS codes downloaded from
## https://www.census.gov/eos/www/naics/2017NAICS/6-digit_2017_Codes.xlsx
all=readWorksheetFromFile(
  file="6-digit_2017_Codes.xlsx",
  sheet=1,
  startRow=1,
  endCol=2
)
## remove blank rows
all$blank=TRUE
for(i in 1:nrow(all)){
  if(prod(is.na(all$X2017.NAICS.Code[i]),is.na(all$X2017.NAICS.Title[i]))==1){
    all$blank[i]=TRUE
  } else {
    all$blank[i]=FALSE
  }
}
all=subset(all,all$blank==FALSE)
all$blank=NULL
all$NAICS=all$X2017.NAICS.Code

## Read in the previously downloaded list of "blue" NAICS codes
blue=readWorksheetFromFile(
  file="/home/george/Documents/Independent_Analytics/BlueEconomy2020/bertdayabase6262019 appended.xlsx",
  sheet=1,
  startRow=1,
  endCol=50
)
blue$NAICS=blue$NAICS_C1

## Filter out the blues from the all list to get the need list
need=subset(all,all$NAICS%in%blue$NAICS==FALSE)
need$NAICSTitle=need$X2017.NAICS.Title
need=select(need,NAICS,NAICSTitle)

## Write out a worksheet in a new workbook with the needed values in it
## Write everything out to an Excel workbook called NAICSneeds.xlsx
wb=loadWorkbook(
  "/home/george/Documents/Independent_Analytics/BlueEconomy2020/2020 Analysis/NAICSneeds.xlsx", 
  create = TRUE
)
createSheet(
  wb, 
  name="missingNAICS"
)
writeWorksheet(
  wb, 
  as.data.frame(need), 
  sheet="missingNAICS", 
  startRow=1, 
  startCol=1
)
saveWorkbook(wb)
