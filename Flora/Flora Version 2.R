library(httr)
library(readr)
library(tidyverse)
library(tibble)
library(readxl)
library(writexl)
library(here)
library(lubridate)

#Connect Web API PTAGIS
res<-GET("https://api.ptagis.org/reporting/reports/Flora1792/file/Icicle_Creek_Forecast.csv",accept('text/plain'))#gets the API connection
res #shows you what the details of the file are
stringi::stri_enc_detect(content(res, "raw"))#tells you what content is encoded as

#Read in PTAGIS returns
returns<-content(res, encoding = "UTF-16LE")#gets you the return data in table format

#Reformat date columns as dates
returns$`Release Date` <- as.Date(
  returns$`Release Date`, format = "%M/%d/%Y")
returns$`First Obs Date Max` <- as.Date(
  returns$`First Obs Date Max`, format = "%M/%d/%Y")
returns$`Last Obs Date Min` <- as.Date(
  returns$`Last Obs Date Min`, format = "%M/%d/%Y")

#Make a new table with Age added 
my_returns <- select(
  returns, 
  'Tag', 'Site', 'First Obs Date Max', 'Brood Year', 'Release Date'
) %>% mutate(
  Age = as.integer(format(returns$`First Obs Date Max`,"%Y")) - `Brood Year` 
)

#Clean up column names
colnames(my_returns) <- c("Tagcode", "SiteName", "MigYear", "BroodYear", "RelYear", "Age")

#Abbreviate array name
my_returns$SiteName = substr(my_returns$SiteName, 1,3)

#Create new adult table by filtering fish from age 2-6
adult_migrants <- my_returns %>%
  filter( Age > 2 & Age < 6)

#Convert detection date to release year in first date and rel year columns
adult_migrants$MigYear <- year(adult_migrants$MigYear)
adult_migrants$RelYear <- year(adult_migrants$RelYear)

#Remove duplicate tag codes at same site, count how many tags/year at each site, 
#reformat table to show counts at each array for each year, arrange by year, and add a grand total tags 
pivot <- adult_migrants %>%
  filter(duplicated(paste(adult_migrants$Tagcode, adult_migrants$SiteName) != TRUE)) %>% # remove dupes
  count(MigYear, SiteName) %>% 
  spread(SiteName, n, fill=0) %>% 
  arrange(MigYear) %>% 
  filter(MigYear > 2011 & MigYear < year(Sys.Date())) %>%
  mutate(GrandTotal = BON + ICL) %>%
  mutate(ConversionRate = (ICL / 0.87) / BON) #%>%
  #rename(Year = MigYear)

#Icicle array efficiency, previously calculated
ICLefficiency <- 0.87

#Calculate conversion rate for BON, MCN, and ICL for years past 2011(i.e., how many fish make it past each array)
ConversionTbl <- pivot %>%
  select(MigYear,BON, MCN, ICL) %>%
  mutate("Conversion" = round( ( (ICL / ICLefficiency) / BON),3)) %>%
  filter( MigYear > 2011)

#Find average conversion rate over the years, round to 3 dec places
conversion_rate <- round(mean(ConversionTbl$Conversion), 3)

#Get current fish counts over BON 
current_BONcounts <- adult_migrants %>% 
  count(MigYear, BroodYear, Age, SiteName) %>% 
  spread(SiteName, n, fill=0) %>% 
  select(MigYear, BroodYear, Age, BON) %>%
  filter(MigYear == year(Sys.Date())) ## This years fish

#Read in PIT tag ratio data and rename columns
PITratios <- read.csv("PITratios.csv")
pit_ratios <- PITratios %>%
  mutate(BroodYear = Release.Year-2) %>%
  select(Release.Year, Release.Number, X..PIT, PIT.Ratio.Non.Tag.Tag, BroodYear)%>%
  rename(RelYear = Release.Year, RelNumber = Release.Number, NumberTagged = X..PIT, Ratio = PIT.Ratio.Non.Tag.Tag)

#Redefine column types, remove ,'s in number columns
pit_ratios$BroodYear <- as.integer(pit_ratios$BroodYear)
pit_ratios$RelNumber <- as.integer( gsub(",", "", pit_ratios$RelNumber))
pit_ratios$NumberTagged <- as.integer( gsub(",","", pit_ratios$NumberTagged))

#Make a data frame of ages we are interested in
current_year <- data.frame(Age = as.integer(c(3,4,5)))

#Make a new table
current_year <- current_year %>% #Use data frame of ages
  mutate(BroodYear = as.integer(year(Sys.Date()))-Age) %>% #Add a column for Brood Year
  left_join(select(current_BONcounts, BroodYear, BON), by = "BroodYear" ) #Join the current BON counts table
current_year_BON_counts<-current_year %>%
  left_join(pit_ratios, by = "BroodYear") %>% #Join the PIT tag table
  mutate("Expanded Migration Size" = BON * Ratio) %>% #Expand the migration size by PIT tag ratio
  rename("Brood Year" = BroodYear, "Bonneville Count" = BON, "Number Released" = RelNumber, "Number Tagged" = NumberTagged, "Ratio Released:Tagged" = Ratio)%>% #Clean up col names
  replace_na(list("Bonneville Count" = 0, "Expanded Migration Size" = 0)) #Replace NA values with 0

#Read in previous return data
dart<-read.csv("hrt_pitadult.csv",header = TRUE)
dart <- dart[,1:10] #keeps columns 1-10

#Read in previous run reconstruction data
icicle_recon<-read.csv("Ici_Run_Recon.csv",header = TRUE)

#Clean up data
ici_run <- icicle_recon %>%
  select(Year, Run) %>%
  filter(!is.na(Year)) %>%
  rename(MigrationYear = Year)
ici_run$Run<- as.integer( gsub(",","",ici_run$Run, ici_run$Run))

#Make sure runs are in whole fish
ici_run$Run <- round(ici_run$Run, 0)

predicted <- adult_migrants %>% 
  count(MigYear, BroodYear,SiteName) %>% 
  spread(SiteName, n, fill=0) %>% 
  arrange(MigYear) %>% 
  filter(MigYear > 2011 & MigYear < year(Sys.Date())) %>%
  inner_join(pit_ratios, by = "BroodYear") %>%
  mutate(Forecast = round(BON*Ratio*conversion_rate, 0)) %>%
  group_by(MigYear) %>%
  summarize(Forecast = sum(Forecast)) %>%
  rename(MigrationYear = MigYear) %>%
  left_join(ici_run, by = "MigrationYear") %>%
  mutate(Difference = Forecast - Run, "Percent Diff" = round((100 * (Run-Forecast) / Run), 0) )
