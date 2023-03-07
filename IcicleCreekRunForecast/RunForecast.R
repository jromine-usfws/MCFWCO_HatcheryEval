#####################################
#
#  Code developed by Jakub Bednarek: 
#  https://github.com/likeasturgeon/Icicle-Creek-Spring-Chinook-Forecast
#
#  Edited by J Romine 2023-03-07
#  jason_romine@fws.gov
#
####################################


# == functions === 
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}



#==== end functions ======

libs <- c('dplyr', 'readr', 'tidyr', 'tibble', 'lubridate','curl', 'stringi')
ipak(libs)
ipak('rJava')
#sooo there is a problem with the java; java is not installed on gov machines

#Import PTAGIS Dataset; Data was queried on PTAGIS for LNFH tagged fish
# migrating up the mainstem, including overshoots at TUF and RRF.
# The raw data is a ftp connection updated daily.


#dataURL <- "https://api.ptagis.org/reporting/reports/Bednarekuba/file/LNFH_PIT_IS.csv"
LNFH_Returns <- httr::GET("https://api.ptagis.org/reporting/reports/Bednarekuba/file/LNFH_PIT_IS.csv")

#Connect Web API PTAGIS
res<-GET("https://api.ptagis.org/reporting/reports/Flora1792/file/Icicle_Creek_Forecast.csv",accept('text/plain'))#gets the API connection
res #shows you what the details of the file are
stringi::stri_enc_detect(httr::content(dat, "raw"))#tells you what content is encoded as#Read in PTAGIS returns
LNFH_Returns <- httr::content(LNFH_Returns, encoding = "UTF-16LE")#gets you the return data in table format
head(LNFH_Returns)
dat<-data.frame(LNFH_Returns)

LNFH_Returns <- read.csv(dataURL, 
                         fileEncoding = "UTF-16LE",
                         stringsAsFactors = FALSE)

LNFH_Returns$Release.Date.MMDDYYYY <- as.Date(
  LNFH_Returns$Release.Date.MMDDYYYY, format = "%M/%d/%Y")

LNFH_Returns$First.Obs.Date.Max <- as.Date(
  LNFH_Returns$First.Obs.Date.Max, format = "%M/%d/%Y")

LNFH_Returns$Last.Obs.Date.Min <- as.Date(
  LNFH_Returns$Last.Obs.Date.Min, format = "%M/%d/%Y")


#Import Run Reconstruction for past years' comparison
filepathRun <- "Icicle R  Spring Chinook Run Reconstruction_01092018.xlsx"
RunReconTble <- readWorksheetFromFile(
  file = filepathRun,
  object = "Icicle R  Spring Chinook Run Reconstruction_01092018",
  sheet = "Icicle Run ReconstructionUpdate",
  region = 'B4:M23',
  keep = c("Year", "Run") 
)
RunReconTble <- as.tibble(RunReconTble)

PITratios <- read.csv("PITratios.csv")
PITratios <- PITratios %>%
  select(Release.Year, Release.Number, X..PIT, PIT.Ratio.Non.Tag.Tag)%>%
  rename(RelYear = Release.Year, Ratio = PIT.Ratio.Non.Tag.Tag)

# Select  most useful fields in PTAGIS query, and add Age column using mutate()
myReturns <- select(
  LNFH_Returns, 
  'Tag.Code', 'Site.Name', 'First.Obs.Date.Max', 'Brood.Year.YYYY', 'Release.Date.MMDDYYYY'
) %>% mutate(
  Age = as.integer(format(LNFH_Returns$First.Obs.Date.Max,"%Y")) - `Brood.Year.YYYY` 
)

#CLEANUP
### Cleanup colnames
colnames(myReturns) <- c("Tagcode", "SiteName", "FirstDate", "BroodYear", "RelYear", "Age")
### Abbreviate Site
myReturns$SiteName = substr(myReturns$SiteName, 1,3) 

# Filter on age 3,4,5
Adult_migrants <- myReturns %>%
  filter( Age > 2 & Age < 6)

# Convert detection date to migration year
Adult_migrants$FirstDate <- year(Adult_migrants$FirstDate)
Adult_migrants$RelYear <- year(Adult_migrants$RelYear)

# Pivot Adult_migrants and get counts
pivot <- Adult_migrants %>%
  filter(duplicated(paste(Adult_migrants$Tagcode, Adult_migrants$SiteName) != TRUE)) %>% # remove dupes
  count(FirstDate, SiteName) %>% 
  spread(SiteName, n, fill=0) %>% 
  arrange(FirstDate) %>% 
  mutate(GrandTotal=BON+ICL) %>%
  rename(Year = FirstDate)

#Calculate Conversion and view key sites
ConversionTbl <- pivot %>%
  select(Year,BON, MCN, ICL) %>%
  mutate("Conversion" = round( ( (ICL / ICLefficiency) / BON),3)) %>%
  filter( Year > 2011)


# This is the ICL array efficiency 
# used to correct the ICL detection rate in next step
ICLefficiency <- 0.87

#Find Average and correct it by factor of ICLefficiency
conversionRate <- round(mean(ConversionTbl$Conversion), 3)

#Run Script
