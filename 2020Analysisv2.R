################################################################################
## Blue Economy Analysis
## May 2020
## George A. Maynard
## R v3.5.2
################################################################################
###################  SET UP THE WORKSPACE  #####################################
################################################################################
## Clear the environment
rm(list=ls())
## Load all necessary libraries
library(dplyr)
library(glue)
library(lambda.tools)
library(tcltk)
## Set working directory
setwd(tk_choose.dir())
################################################################################
################### READ IN THE DATA ###########################################
################################################################################
## The blueNaics object contains the list of 121 classifications of interet that
## relate to the Blue Economy. This dataset was provided by the Blue Economy 
## Foundation and should have three columns:
## NAICS = the NAICS code (6 to 9 digits)
## NAICS_DESC = a character string describing what the NAICS code in the first 
##              column means
## SIC_DESC = a character string describing the "Standard Industrial 
##            Classification" (SIC) equivalent of the NAICS code. The SIC was 
##            developed in the United States and has been replaced by the NAICS.
blueNaics=read.csv(
  file="combined_naics_list_request.csv",
  header=TRUE
)

## The blues object contains all known blue businesses in the geographic area of
## interest. It was purchased from NAICS.com by the Blue Economy Foundation and
## should have 36 columns:
## BUSDNSNO = 9 digit Duns and Bradstreet Number (DUNS), a unique business
##            identifier
## COMPANY = company name
## ADDRESS = mailing address
## CITY = mailing city
## STATE = mailing state
## ZIP = mailing zip code
## PADDRESS = physical address
## PCITY = physical city
## PSTATE = physical state
## PREFIX = prefix of contact person
## FIRST = first name of contact person
## MIDINIT = middle initial of contact person
## LAST = last name of contact person
## CONTACTNAMECOMP = contact person
## TITLE = title of contact person
## PROFESSIONALCONTACT = professional contact name
## PROFESSIONALCONTACTTITLE = professional contact title
## EMAIL = professional contact email
## PHONE = phone contact
## LINE_OF_BUSINESS = descriptive business category
## ACTUALSALES = annual sales
## URL = website
## SIC = Standard Industrial Classification (outdated)
## SICDESC = description of SIC class
## NAICS_C1-6 = NAICS classes that the business falls in
## NAICS_D1-6 = description of NAICS classes
blues=read.csv(
  file="bertdayabase6262019 appended.csv",
  header=TRUE,
  colClasses=c("BUSDNSNO"="character","ZIP"="character")
)
## The BUSDNSNO and ZIP columns may have leading zeros that do not load
## correctly. These lines of code fix that. 
blues$BUSDNSNO=ifelse(
  nchar(blues$BUSDNSNO)==7,
  paste("00",blues$BUSDNSNO,sep=""),
  ifelse(
    nchar(blues$BUSDNSNO)==8,
    paste("0",blues$BUSDNSNO,sep=""),
    as.character(blues$BUSDNSNO)
  )
)
blues$ZIP=ifelse(
  nchar(blues$ZIP)==4,
  paste("0",blues$ZIP,sep=""),
  as.character(blues$ZIP)
)

## Read in the Industry Sector classification for each NAICS code from the 
## previous Blue Economy report. This file was manually generated.
lob=read.csv(
  "/home/george/Documents/AUsefulCog/BlueEconomy/2020 Analysis/LOB.csv",
  header=TRUE
  )
lob$NAICS=substr(lob$NAICS_C1,1,4)
lob$dup=duplicated(paste(lob$IndustrySector,lob$BlueLevel,lob$NAICS))
lob=subset(lob,lob$dup==FALSE)
lob$dup=NULL
lob=select(lob,IndustrySector,BlueLevel,NAICS)

## Read in the NAICS conversion tables downloaded from
## https://www.census.gov/eos/www/naics/concordances/concordances.html
transform1=read.csv(
  file="2002_to_2007_NAICS.csv"
  )
transform2=read.csv(
  file="2007_to_2012_NAICS.csv"
)
transform3=read.csv(
  file="2017_to_2012_NAICS.csv"
)
## Add conversion columns to blueNaics to 2012, 2007, and 2002 NAICS
blueNaics$NAICS=as.character(round(blueNaics$NAICS,0))
blueNaics$NAICS12=0
blueNaics$NAICS07=0
blueNaics$NAICS02=0
blueNaics$NAICSshort=0
for(i in 1:nrow(blueNaics)){
  if(blueNaics$NAICS[i]%in%transform3$NAICS2017){
    blueNaics$NAICS12[i]=transform3$NAICS2012[
      which(transform3$NAICS2017==blueNaics$NAICS[i])]
    blueNaics$NAICS07[i]=transform2$NAICS2007[
      which(transform2$NAICS2012==blueNaics$NAICS12[i])]
    blueNaics$NAICS02[i]=transform1$NAICS2002[
      which(transform1$NAICS2007==blueNaics$NAICS07[i])]
    blueNaics$NAICSshort[i]=as.numeric(substr(blueNaics$NAICS[i],1,4))
  }
}
## Assign each census-designated place / village to a town
barnstable=c("Barnstable","Centerville","Cotuit","Cummaquid","Hyannis",
  "Hyannis Port","Marstons Mills","Osterville","West Barnstable")
bourne=c("Bourne","Buzzards Bay","Cataumet","Sagamore","Sagamore Beach",
  "Pocasset","Monument Beach")
brewster=c("Brewster")
chatham=c("West Chatham","South Chatham","North Chatham","Chatham")
chilmark=c("Chilmark")
dennis=c("Bass River","Dennis","Dennis Port","East Dennis","South Dennis",
  "West Dennis")
eastham=c("North Eastham","Eastham")
edgartown=c("Edgartown")
falmouth=c("East Falmouth","Falmouth","North Falmouth","Teaticket","Waquoit",
  "Woods Hole")
harwich=c("Harwich","Harwich Port","East Harwich","South Harwich",
  "West Harwich")
mashpee=c("Mashpee")
mattapoisett=c("Mattapoisett")
menemsha=c("Menemsha")
nantucket=c("Nantucket","Siasconset")
oakbluffs=c("Oak Bluffs")
orleans=c("East Orleans","Orleans","South Orleans")
plymouth=c("Manomet","Plymouth","White Horse Beach")
provincetown=c("Provincetown")
sandwich=c("East Sandwich","Forestdale","Sandwich")
truro=c("North Truro","Truro")
vineyardhaven=c("Vineyard Haven")
wareham=c("East Wareham","Wareham","West Wareham")
wellfleet=c("South Wellfleet","Wellfleet")
yarmouth=c("South Yarmouth","West Yarmouth","Yarmouth Port")
aquinnah=c("Aquinnah")
wtisbury=c("West Tisbury")
other=subset(unique(blues$CITY),
  unique(blues$CITY)%in%c(barnstable,bourne,brewster,chatham,chilmark,dennis,
    eastham,edgartown,falmouth,harwich,mashpee,mattapoisett,menemsha,nantucket,
    oakbluffs,orleans,plymouth,provincetown,sandwich,truro,vineyardhaven,
    wareham,wellfleet,yarmouth,aquinnah,wtisbury)==FALSE)
blues$sumCity=ifelse(blues$CITY%in%barnstable,"Barnstable",
  ifelse(blues$CITY%in%bourne,"Bourne",
    ifelse(blues$CITY%in%brewster,"Brewster",
      ifelse(blues$CITY%in%chatham,"Chatham",
        ifelse(blues$CITY%in%chilmark,"Chilmark",
          ifelse(blues$CITY%in%dennis,"Dennis",
            ifelse(blues$CITY%in%eastham,"Eastham",
              ifelse(blues$CITY%in%edgartown,"Edgartown",
                ifelse(blues$CITY%in%falmouth,"Falmouth",
                  ifelse(blues$CITY%in%harwich,"Harwich",
                    ifelse(blues$CITY%in%mashpee,"Mashpee",
                      ifelse(blues$CITY%in%mattapoisett,"Mattapoisett",
                        ifelse(blues$CITY%in%menemsha,"Menemsha",
                          ifelse(blues$CITY%in%nantucket,"Nantucket",
                            ifelse(blues$CITY%in%oakbluffs,"Oak Bluffs",
                              ifelse(blues$CITY%in%orleans,"Orleans",
                                ifelse(blues$CITY%in%plymouth,"Plymouth",
                                  ifelse(blues$CITY%in%provincetown,"Provincetown",
                                    ifelse(blues$CITY%in%sandwich,"Sandwich",
                                      ifelse(blues$CITY%in%truro,"Truro",
                                        ifelse(blues$CITY%in%vineyardhaven,"Vineyard Haven",
                                          ifelse(blues$CITY%in%wareham,"Wareham",
                                            ifelse(blues$CITY%in%wellfleet,"Wellfleet",
                                              ifelse(blues$CITY%in%yarmouth,"Yarmouth",
                                                ifelse(blues$CITY%in%aquinnah,"Aquinnah",
                                                  ifelse(blues$CITY%in%wtisbury,"West Tisbury",
                                                    "other"
  ))))))))))))))))))))))))))

## Select a business type ("Private" or "Aggregate")
busType="Private"
## Reset the working directory to point to the WageReports folder
setwd(paste(getwd(),"/WageReports/",busType,sep=""))
## Create an empty results dataframe
results=as.data.frame(matrix(nrow=0,ncol=7))
colnames(results)=c("NAICS","Description","Wages","NumBus","NumEmp","Year","Blue")
## For each file in the directory
for(i in dir()){
  ## Read in the file
  x=read.csv(i)
  ## Eliminate dollar signs from the Total.Wages column
  x$wages=gsub("\\$","",x$Total.Wages)
  ## Eliminate thousands separators
  x$wages=gsub(",","",x$wages)
  x$num=as.numeric(gsub(",","",x$No..of.Establishments))
  x$emp=as.numeric(gsub(",","",x$Average.Monthly.Employment))
  ## Replace spaces with no character
  x$NAICS=gsub(" ","",x$NAICS)
  ## Subset out only the 4 digit NAICS code lines (aggregated values)
  x=subset(x,nchar(x$NAICS)==4)
  x$year=substr(colnames(x)[4],(nchar(colnames(x)[4])-3),(nchar(colnames(x)[4])))
  x$blue=NA
  x=x[,c(1,2,19:22)]
  ## Add the new data to the results frame
  results=rbind(results,x)
}
## Ensure that column types are set appropriately
results$wages=as.numeric(as.character(results$wages))
results$year=as.numeric(as.character(results$year))
## Check to see if the 4 digit NAICS is blue or not
blueNaics$dup=duplicated(blueNaics$NAICSshort)
blueNaics=subset(blueNaics,blueNaics$dup==FALSE)
results$blue=ifelse(results$NAICS%in%blueNaics$NAICSshort,TRUE,FALSE)
## Set the working directory back two folders
setwd("..")
setwd("..")
## Read in the inflation corrections
inflation=read.csv(
  file="inflation.csv",
  header=TRUE
  )
## For each row in the results, apply inflation corrections
for(i in 1:nrow(results)){
  results$wages[i]=
    results$wages[i]*
      inflation$Conversion[which(inflation$Year==results$year[i])]
}

## Read in the Massachusetts Wage Report 
MA=read.csv("MA_WageReport.csv")
## Eliminate dollar signs from the Total.Wages column
MA$wages=gsub("\\$","",MA$Total.Wages)
## Eliminate thousands separators
MA$wages=gsub(",","",MA$wages)
MA$num=as.numeric(gsub(",","",MA$No..of.Establishments))
MA$emp=as.numeric(gsub(",","",MA$Average.Monthly.Employment))
MA$NAICS=gsub(" ","",MA$NAICS)
## Subset out only the 4 digit NAICS code lines (aggregated values)
MA=subset(MA,nchar(MA$NAICS)==4)
MA$year=substr(colnames(MA)[4],(nchar(colnames(MA)[4])-3),(nchar(colnames(MA)[4])))
MA=MA[,c(1,2,19:22)]
## Check whether each industry is blue or not
MA$blue=MA$NAICS%in%blueNaics$NAICSshort

## Read in the National Wage Report
nat=read.csv("NationalWageReport2018.csv")
## Subset out only the 4 digit NAICS code lines (aggregated values)
nat=subset(nat,nchar(as.character(nat$NAICS))==4)

################################################################################
################### Analyses ###################################################
################################################################################
## subset out the dark blue industries to generate a  list of values for the darkLQ loop
dark=subset(lob,lob$BlueLevel=="Dark")
## Calculating the location quotients for dark blue industries
## Create an empty matrix to store results
darkLQ=matrix(nrow=length(unique(dark$NAICS)),ncol=8)
colnames(darkLQ)=c(
  "IndustrySector","Local","State","National","StatewideLQ","NationalLQ",
  "StateVNationLQ","Description"
  )
for(i in 1:nrow(darkLQ)){
  ## Record the NAICS value
  darkLQ[i,1]=as.character(unique(dark$NAICS)[i])
  ## Record the proportion of local employment in that category
  darkLQ[i,2]=sum(subset(dark,dark$NAICS==darkLQ[i,1])$emp)/
    sum(subset(results,results$year==2018)$emp)
  ## Record the proportion of state employment in that category
  darkLQ[i,3]=sum(subset(MA,MA$NAICS==darkLQ[i,1])$emp)/sum(MA$emp)
  ## Record the proportion of national employment in that category
  darkLQ[i,4]=sum(subset(nat,nat$NAICS==darkLQ[i,1])$emp)/sum(nat$emp)
  ## Calculate a location quotient for Cape and Islands v MA
  darkLQ[i,5]=as.numeric(darkLQ[i,2])/as.numeric(darkLQ[i,3])
  ## Calculate a location quotient for Cape and Islands v USA
  darkLQ[i,6]=as.numeric(darkLQ[i,2])/as.numeric(darkLQ[i,4])
  ## Calculate a location quotient for MA v USA
  darkLQ[i,7]=as.numeric(darkLQ[i,3])/as.numeric(darkLQ[i,4])
  ## Record the industry description
  darkLQ[i,8]=as.character(subset(results,results$NAICS==darkLQ[i,1])$Description)[1]
}
## Reformat the matrix to a dataframe and round decimals down to 3 places
darkLQ=as.data.frame(darkLQ)
darkLQ$Local=as.numeric(as.character(darkLQ$Local))
darkLQ$State=as.numeric(as.character(darkLQ$State))
darkLQ$StatewideLQ=as.numeric(as.character(darkLQ$StatewideLQ))
darkLQ$StatewideLQ=round(darkLQ$StatewideLQ,3)
darkLQ$NationalLQ=as.numeric(as.character(darkLQ$NationalLQ))
darkLQ$NationalLQ=round(darkLQ$NationalLQ,3)
darkLQ$StateVNationLQ=as.numeric(as.character(darkLQ$StateVNationLQ))
darkLQ$StateVNationLQ=round(darkLQ$StateVNationLQ,3)
darkLQ=darkLQ[order(darkLQ$NationalLQ,decreasing=TRUE),]
darkLQ$dup=duplicated(darkLQ$IndustrySector)
darkLQ=subset(darkLQ,darkLQ$dup==FALSE)
darkLQ$dup=NULL
## Write out the new dataframe to a file called "darkLQ.csv"
write.csv(
  darkLQ,
  file="darkLQ.csv",
  row.names=FALSE
  )

## Repeat the same process for medium blue industries using "medLQ.csv"
medium=subset(subset(lob,lob$BlueLevel=="Medium"))
medLQ=matrix(nrow=length(unique(medium$NAICS)),ncol=8)
colnames(medLQ)=c("IndustrySector","Local","State","National","StatewideLQ",
  "NationalLQ","StateVNationLQ","Description")
for(i in 1:nrow(medLQ)){
  medLQ[i,1]=as.character(unique(medium$NAICS)[i])
  medLQ[i,2]=sum(subset(medium,medium$NAICS==medLQ[i,1])$emp)/
    sum(subset(results,results$year==2018)$emp)
  medLQ[i,3]=sum(subset(MA,MA$NAICS==medLQ[i,1])$emp)/sum(MA$emp)
  medLQ[i,4]=sum(subset(nat,nat$NAICS==medLQ[i,1])$emp)/sum(nat$emp)
  medLQ[i,5]=as.numeric(medLQ[i,2])/as.numeric(medLQ[i,3])
  medLQ[i,6]=as.numeric(medLQ[i,2])/as.numeric(medLQ[i,4])
  medLQ[i,7]=as.numeric(medLQ[i,3])/as.numeric(medLQ[i,4])
  medLQ[i,8]=as.character(subset(results,results$NAICS==medLQ[i,1])$Description)[1]
}
medLQ=as.data.frame(medLQ)
medLQ$Local=as.numeric(as.character(medLQ$Local))
medLQ$State=as.numeric(as.character(medLQ$State))
medLQ$StatewideLQ=as.numeric(as.character(medLQ$StatewideLQ))
medLQ$StatewideLQ=round(medLQ$StatewideLQ,3)
medLQ$NationalLQ=as.numeric(as.character(medLQ$NationalLQ))
medLQ$NationalLQ=round(medLQ$NationalLQ,3)
medLQ$StateVNationLQ=as.numeric(as.character(medLQ$StateVNationLQ))
medLQ$StateVNationLQ=round(medLQ$StateVNationLQ,3)
medLQ=medLQ[order(medLQ$NationalLQ,decreasing=TRUE),]
medLQ$dup=duplicated(medLQ$IndustrySector)
medLQ=subset(medLQ,medLQ$dup==FALSE)
medLQ$dup=NULL
write.csv(
  medLQ,
  file="medLQ.csv",
  row.names=FALSE
  )

## Calculate proportion of wages from dark industries
all=subset(results,results$year==2018)

aBlue=subset(results,results$blue==TRUE)
aBlue=merge(subset(aBlue,aBlue$year==2018),lob)


aggregate(dark$EMP~dark$sumCity,FUN="sum")
aggregate(medium$EMP~medium$sumCity,FUN="sum")