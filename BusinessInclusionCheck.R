## ---------------------------
##
## Script name: BusinessInclusionCheck
##
## Purpose of script: Checks to see which businesses were dropped from or added to the dataset
##    between the two reports
##
## Author: George A. Maynard
##
## Date Created: 2020-05-28
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

## ---------------------------

## set options
options(scipen = 6, digits = 4) 
## ---------------------------

## load up the packages we will need:  
library(XLConnect)
library(stringdist)
## ---------------------------

## load up our functions into memory

## ---------------------------

## Read in the 2020 NAICS.com data for a list of blue businesses included in the 2020 dataset
naics2020=read.csv("/home/george/Documents/Independent_Analytics/BlueEconomy2020/2020 Analysis/bertdayabase6262019 appended.csv")

## Read in the modified 2017 data NAICS data from the previous report
## The first sheet of the file is the provider data
naics2017=readWorksheetFromFile(
  file="/home/george/Documents/Independent_Analytics/BlueEconomy2020/Blue Economy_oldreport/MASTER File to be Pivoted.xlsx",
  sheet=1,
  startRow=1,
  endCol=22
)

## New companies are those that exist in 2020 but not 2017
new=subset(naics2020,toupper(naics2020$COMPANY)%in%toupper(naics2017$COMPANY)==FALSE)$COMPANY

## "gone" refers  to companies that existed in 2017 but no longer exist in 2020
gone=subset(naics2017,toupper(naics2017$COMPANY)%in%toupper(naics2020$COMPANY)==FALSE)$COMPANY

## Use fuzzy matching to come up with a list of possible matches between new and gone
## Create an empty matrix to store all results
matches=matrix(nrow=length(new),ncol=7)
## Create an empty matrix to store most likely results (>90%)
likely=matrix(nrow=length(new),ncol=3)
## For each new company, find the closest match(es) in the gone companies
for(i in 1:length(new)){
  matches[i,1]=new[i]
  d=stringsim(
    a=toupper(new[i]),
    b=toupper(gone)
  )
  p=gone[which(d==max(d))]
  if(length(p)<5){
    matches[i,seq(2,2+length(p)-1,1)]=p
    matches[i,7]=FALSE
  } else {
    matches[i,seq(2,6,1)]=p[1:5]
    matches[i,7]=TRUE
  }
  likely[i,1]=new[i]
  if(max(d)>0.9){
    likely[i,2]=gone[which(d==max(d))][1]
    likely[i,3]=max(d)*100
  }
}
colnames(matches)=c("NEW","PossibleMatch1","PossibleMatch2","PossibleMatch3","PossibleMatch4",
  "PossibleMatch5","AdditionalMatchesFound")
colnames(likely)=c("NEW","PossibleMatch","PercentMatch")

## Likely matches are only those with a 90% or greater match
likely=subset(likely,likely[,3]>=90)

## Write everything out to an Excel workbook with multiple spreadsheets
wb=loadWorkbook(
  "/home/george/Documents/Independent_Analytics/BlueEconomy2020/2020 Analysis/Matches.xlsx", 
  create = TRUE
  )

## Create a worksheet in the workbook for each result object and write out the results
createSheet(
  wb, 
  name="newBusinesses"
)
writeWorksheet(
  wb, 
  new, 
  sheet="newBusinesses", 
  startRow=1, 
  startCol=1
)
createSheet(
  wb,
  name="gone"
)
writeWorksheet(
  wb,
  gone,
  sheet="gone",
  startRow=1,
  startCol=1
)
createSheet(
  wb,
  name="LikelyMatches"
)
writeWorksheet(
  wb,
  likely,
  sheet="LikelyMatches",
  startRow=1,
  startCol=1
)
createSheet(
  wb,
  name="PossibleMatches"
)
writeWorksheet(
  wb,
  matches,
  sheet="PossibleMatches",
  startRow=1,
  startCol=1
)
## Saves the workbook to the corresponding Excel file and write the file to disk
saveWorkbook(wb)

