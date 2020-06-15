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
naics2020 = read.csv(
  "/home/george/Documents/Independent_Analytics/BlueEconomy2020/2020 Analysis/bertdayabase6262019 appended.csv"
)

## Read in the modified 2017 data NAICS data from the previous report
## The first sheet of the file is the provider data
naics2017 = readWorksheetFromFile(
  file = "/home/george/Documents/Independent_Analytics/BlueEconomy2020/Blue Economy_oldreport/MASTER File to be Pivoted.xlsx",
  sheet = 1,
  startRow = 1,
  endCol = 22
)

## New companies are those that exist in 2020 but not 2017
new = subset(naics2020,
             toupper(naics2020$COMPANY) %in% toupper(naics2017$COMPANY) == FALSE)$COMPANY

## "gone" refers  to companies that existed in 2017 but no longer exist in 2020
gone = subset(naics2017,
              toupper(naics2017$COMPANY) %in% toupper(naics2020$COMPANY) == FALSE)$COMPANY

## Use fuzzy matching to come up with a list of possible matches between new and gone
## Create an empty matrix to store all results
matches = matrix(nrow = length(new), ncol = 7)
## Create an empty matrix to store most likely results (>90%)
likely = matrix(nrow = length(new), ncol = 3)
## For each new company, find the closest match(es) in the gone companies
for (i in 1:length(new)) {
  matches[i, 1] = new[i]
  d = stringsim(a = toupper(new[i]),
                b = toupper(gone))
  p = gone[which(d == max(d))]
  if (length(p) < 5) {
    matches[i, seq(2, 2 + length(p) - 1, 1)] = p
    matches[i, 7] = FALSE
  } else {
    matches[i, seq(2, 6, 1)] = p[1:5]
    matches[i, 7] = TRUE
  }
  likely[i, 1] = new[i]
  if (max(d) > 0.9) {
    likely[i, 2] = gone[which(d == max(d))][1]
    likely[i, 3] = max(d) * 100
  }
}
colnames(matches) = c(
  "NEW",
  "PossibleMatch1",
  "PossibleMatch2",
  "PossibleMatch3",
  "PossibleMatch4",
  "PossibleMatch5",
  "AdditionalMatchesFound"
)
colnames(likely) = c("NEW", "PossibleMatch", "PercentMatch")

## Likely matches are only those with a 90% or greater match
likely = subset(likely, likely[, 3] >= 90)

## Write everything out to an Excel workbook with multiple spreadsheets
wb = loadWorkbook(
  "/home/george/Documents/Independent_Analytics/BlueEconomy2020/2020 Analysis/BUS_Matches.xlsx",
  create = TRUE
)

## Create a worksheet in the workbook for each result object and write out the results
createSheet(wb,
            name = "newBusinesses")
writeWorksheet(
  wb,
  as.data.frame(new),
  sheet = "newBusinesses",
  startRow = 1,
  startCol = 1
)
createSheet(wb,
            name = "gone")
writeWorksheet(
  wb,
  as.data.frame(gone),
  sheet = "gone",
  startRow = 1,
  startCol = 1
)
createSheet(wb,
            name = "LikelyMatches")
writeWorksheet(
  wb,
  as.data.frame(likely),
  sheet = "LikelyMatches",
  startRow = 1,
  startCol = 1
)
createSheet(wb,
            name = "PossibleMatches")
writeWorksheet(
  wb,
  as.data.frame(matches),
  sheet = "PossibleMatches",
  startRow = 1,
  startCol = 1
)
## Saves the workbook to the corresponding Excel file and write the file to disk
saveWorkbook(wb)

## Read in the non-profit list identified in the previous report
np2017 = read.csv(
  "/home/george/Documents/Independent_Analytics/BlueEconomy2020/2020 Analysis/blue_eo.csv"
)
## Subset out only the blue non-profits
np2017 = subset(np2017, np2017$BLUE == "Y")
## Check to see if those non-profits exist in the 2020 NAICS data or the
present = subset(np2017, toupper(np2017$NAME) %in% toupper(naics2020$COMPANY))$NAME
missing = subset(np2017, np2017$NAME %in% present == FALSE)$NAME
## Use fuzzy matching to check for other potentials following the same procedure as above
## Create an empty matrix to store all results
matches = matrix(nrow = length(missing), ncol = 7)
## Create an empty matrix to store most likely results (>90%)
likely = matrix(nrow = length(missing), ncol = 3)
## For each missing non-profit, find the closest match(es) in the business data
for (i in 1:length(missing)) {
  matches[i, 1] = missing[i]
  d = stringsim(a = toupper(missing[i]),
                b = toupper(naics2020$COMPANY))
  p = naics2020$COMPANY[which(d == max(d))]
  if (length(p) < 5) {
    matches[i, seq(2, 2 + length(p) - 1, 1)] = p
    matches[i, 7] = FALSE
  } else {
    matches[i, seq(2, 6, 1)] = p[1:5]
    matches[i, 7] = TRUE
  }
  likely[i, 1] = missing[i]
  if (max(d) > 0.9) {
    likely[i, 2] = naics2020$COMPANY[which(d == max(d))][1]
    likely[i, 3] = max(d) * 100
  }
}
colnames(matches) = c(
  "Missing",
  "PossibleMatch1",
  "PossibleMatch2",
  "PossibleMatch3",
  "PossibleMatch4",
  "PossibleMatch5",
  "AdditionalMatchesFound"
)
colnames(likely) = c(
  "Missing", 
  "PossibleMatch", 
  "PercentMatch"
)

## Likely matches are only those with a 90% or greater match
likely = subset(likely, likely[, 3] >= 90)

## Write everything out to an Excel workbook with multiple spreadsheets
wb = loadWorkbook(
  "/home/george/Documents/Independent_Analytics/BlueEconomy2020/2020 Analysis/NP_Matches.xlsx",
  create = TRUE
)

## Create a worksheet in the workbook for each result object and write out the results
createSheet(
  wb,
  name = "missingNPs"
)
writeWorksheet(
  wb,
  as.data.frame(missing),
  sheet = "missingNPs",
  startRow = 1,
  startCol = 1
)
createSheet(
  wb,
  name = "LikelyMatches"
  )
writeWorksheet(
  wb,
  as.data.frame(likely),
  sheet = "LikelyMatches",
  startRow = 1,
  startCol = 1
)
createSheet(
  wb,
  name = "PossibleMatches"
)
writeWorksheet(
  wb,
  as.data.frame(matches),
  sheet = "PossibleMatches",
  startRow = 1,
  startCol = 1
)
## Saves the workbook to the corresponding Excel file and write the file to disk
saveWorkbook(wb)

colnames(matches)=c(
  "NEW",
  "PossibleMatch1",
  "PossibleMatch2",
  "PossibleMatch3",
  "PossibleMatch4",
  "PossibleMatch5",
  "AdditionalMatchesFound"
)
colnames(likely)=c(
  "NEW",
  "PossibleMatch",
  "PercentMatch"
)

## Likely matches are only those with a 90% or greater match
likely=subset(likely,likely[,3]>=90)

## Write everything out to an Excel workbook with multiple spreadsheets
wb=loadWorkbook(
  "/home/george/Documents/Independent_Analytics/BlueEconomy2020/2020 Analysis/BUS_Matches.xlsx", 
  create = TRUE
)

## Create a worksheet in the workbook for each result object and write out the results
createSheet(
  wb, 
  name="newBusinesses"
)
writeWorksheet(
  wb, 
  as.data.frame(new), 
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
  as.data.frame(gone),
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
  as.data.frame(likely),
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
  as.data.frame(matches),
  sheet="PossibleMatches",
  startRow=1,
  startCol=1
)
## Saves the workbook to the corresponding Excel file and write the file to disk
saveWorkbook(wb)

## Read in the non-profit list identified in the previous report
np2017=read.csv("/home/george/Documents/Independent_Analytics/BlueEconomy2020/2020 Analysis/blue_eo.csv")
## Subset out only the blue non-profits
np2017=subset(np2017,np2017$BLUE=="Y")
## Check to see if those non-profits exist in the 2020 NAICS data or the
present=subset(np2017,toupper(np2017$NAME)%in%toupper(naics2020$COMPANY))$NAME
missing=subset(np2017,np2017$NAME%in%present==FALSE)$NAME
## Use fuzzy matching to check for other potentials following the same procedure as above
## Create an empty matrix to store all results
matches=matrix(nrow=length(missing),ncol=7)
## Create an empty matrix to store most likely results (>90%)
likely=matrix(nrow=length(missing),ncol=3)
## For each missing non-profit, find the closest match(es) in the business data
for(i in 1:length(missing)){
  matches[i,1]=missing[i]
  d=stringsim(
    a=toupper(missing[i]),
    b=toupper(naics2020$COMPANY)
  )
  p=naics2020$COMPANY[which(d==max(d))]
  if(length(p)<5){
    matches[i,seq(2,2+length(p)-1,1)]=p
    matches[i,7]=FALSE
  } else {
    matches[i,seq(2,6,1)]=p[1:5]
    matches[i,7]=TRUE
  }
  likely[i,1]=missing[i]
  if(max(d)>0.9){
    likely[i,2]=naics2020$COMPANY[which(d==max(d))][1]
    likely[i,3]=max(d)*100
  }
}
colnames(matches)=c(
  "Missing",
  "PossibleMatch1",
  "PossibleMatch2",
  "PossibleMatch3",
  "PossibleMatch4",
  "PossibleMatch5",
  "AdditionalMatchesFound"
)
colnames(likely)=c(
  "Missing",
  "PossibleMatch",
  "PercentMatch"
)

## Likely matches are only those with a 90% or greater match
likely=subset(likely,likely[,3]>=90)

## Write everything out to an Excel workbook with multiple spreadsheets
wb=loadWorkbook(
  "/home/george/Documents/Independent_Analytics/BlueEconomy2020/2020 Analysis/NP_Matches.xlsx", 
  create = TRUE
)

## Create a worksheet in the workbook for each result object and write out the results
createSheet(
  wb, 
  name="missingNPs"
)
writeWorksheet(
  wb, 
  as.data.frame(missing), 
  sheet="missingNPs", 
  startRow=1, 
  startCol=1
)
createSheet(
  wb,
  name="LikelyMatches"
)
writeWorksheet(
  wb,
  as.data.frame(likely),
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
  as.data.frame(matches),
  sheet="PossibleMatches",
  startRow=1,
  startCol=1
)
## Saves the workbook to the corresponding Excel file and write the file to disk
saveWorkbook(wb)

## Create a new workbook with NAICS matches between the two data sets
old_NAICS=select(naics2017,NAICS_C1,NAICS_D1)
old_NAICS$dup=duplicated(old_NAICS)
old_NAICS=subset(old_NAICS,old_NAICS$dup==FALSE)
old_NAICS$dup=NULL

new_NAICS=select(naics2020,NAICS_C1,NAICS_D1)
new_NAICS$dup=duplicated(new_NAICS)
new_NAICS=subset(new_NAICS,new_NAICS$dup==FALSE)
new_NAICS$dup=NULL

both_NAICS=subset(old_NAICS,old_NAICS$NAICS_C1%in%new_NAICS$NAICS_C1)
dropped_NAICS=subset(old_NAICS,old_NAICS$NAICS_C1%in%new_NAICS$NAICS_C1==FALSE)
added_NAICS=subset(new_NAICS,new_NAICS$NAICS_C1%in%old_NAICS$NAICS_C1==FALSE)

wb=loadWorkbook(
  "/home/george/Documents/Independent_Analytics/BlueEconomy2020/2020 Analysis/NAICS_Matches.xlsx", 
  create = TRUE
)

## Create a worksheet in the workbook for each result object and write out the results
createSheet(
  wb, 
  name="bothNAICS"
)
writeWorksheet(
  wb, 
  as.data.frame(both_NAICS), 
  sheet="bothNAICS", 
  startRow=1, 
  startCol=1
)
createSheet(
  wb, 
  name="droppedNAICS"
)
writeWorksheet(
  wb, 
  as.data.frame(dropped_NAICS), 
  sheet="droppedNAICS", 
  startRow=1, 
  startCol=1
)
createSheet(
  wb, 
  name="addedNAICS"
)
writeWorksheet(
  wb, 
  as.data.frame(added_NAICS), 
  sheet="addedNAICS", 
  startRow=1, 
  startCol=1
)
saveWorkbook(wb)
