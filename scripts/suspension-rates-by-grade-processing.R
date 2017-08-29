library(dplyr)
library(datapkg)

##################################################################
#
# Processing Script for Suspension Rates by Grade
# Created by Jenna Daly
# On 08/23/2017
#
##################################################################

#Setup environment
sub_folders <- list.files()
raw_location <- grep("raw", sub_folders, value=T)
path_to_raw_data <- (paste0(getwd(), "/", raw_location))
all_csvs <- dir(path_to_raw_data, recursive=T, pattern = ".csv") 
all_state_csvs <- grep("ct", all_csvs, value=T) 
all_dist_csvs <- all_csvs[!all_csvs %in% all_state_csvs]

#Bring in district level suspension rates
susp_rates_dist <- data.frame(stringsAsFactors = F)
for (i in 1:length(all_dist_csvs)) {
  current_file <- read.csv(paste0(path_to_raw_data, "/", all_dist_csvs[i]), stringsAsFactors=F, header=T, check.names=F )
  current_file <- current_file[, !(names(current_file) == "District Code")]
  get_year <- as.numeric(substr(unique(unlist(gsub("[^0-9]", "", unlist(all_dist_csvs[i])), "")), 1, 4))
  get_year <- paste0(get_year, "-", get_year + 1) 
  current_file$Year <- get_year
  susp_rates_dist <- rbind(susp_rates_dist, current_file)
}

#Add statewide data...
susp_rates_state <- data.frame(stringsAsFactors = F)
for (i in 1:length(all_state_csvs)) {
  current_file <- read.csv(paste0(path_to_raw_data, "/", all_state_csvs[i]), stringsAsFactors=F, header=T, check.names=F )
  current_file <- current_file[, !(names(current_file) == "Organization Code")]
  get_year <- as.numeric(substr(unique(unlist(gsub("[^0-9]", "", unlist(all_state_csvs[i])), "")), 1, 4))
  get_year <- paste0(get_year, "-", get_year + 1) 
  current_file$Year <- get_year
  susp_rates_state <- rbind(susp_rates_state, current_file)
}

#Combine district and state

#set District column to CT
susp_rates_state$District <- "Connecticut"

#bind together
susp_rates <- rbind(susp_rates_state, susp_rates_dist)

#Remove rows where Grade = "Not Available"
susp_rates <- susp_rates[susp_rates$Grade != "Not Available",]

#Remove Count column
susp_rates$Count <- NULL

#backfill Districts
district_dp_URL <- 'https://raw.githubusercontent.com/CT-Data-Collaborative/ct-school-district-list/master/datapackage.json'
district_dp <- datapkg_read(path = district_dp_URL)
districts <- (district_dp$data[[1]])

susp_rates_fips <- merge(susp_rates, districts, by = "District", all=T)

susp_rates_fips$District <- NULL

susp_rates_fips<-susp_rates_fips[!duplicated(susp_rates_fips), ]

#backfill year and grade
years <- c("2009-2010", 
           "2010-2011", 
           "2011-2012",
           "2012-2013",
           "2013-2014",
           "2014-2015",
           "2015-2016")

grade <- c("K", "01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")

backfill_years <- expand.grid(
  `FixedDistrict` = unique(districts$`FixedDistrict`),
  `Year` = years, 
  `Grade` = grade
)

backfill_years$FixedDistrict <- as.character(backfill_years$FixedDistrict)
backfill_years$Year <- as.character(backfill_years$Year)

backfill_years <- arrange(backfill_years, FixedDistrict)

complete_susp_rates <- merge(susp_rates_fips, backfill_years, all=T)

#remove duplicated Year rows
complete_susp_rates <- complete_susp_rates[!with(complete_susp_rates, is.na(complete_susp_rates$Year)),]

#return blank in FIPS if not reported
complete_susp_rates$FIPS[is.na(complete_susp_rates$FIPS)] <- ""

#Rename FixedDistrict to District
names(complete_susp_rates)[names(complete_susp_rates) == 'FixedDistrict'] <- 'District'

#reshape from wide to long format
cols_to_stack <- c("%")

long_row_count = nrow(complete_susp_rates) * length(cols_to_stack)

complete_susp_rates_long <- reshape(complete_susp_rates,
                                    varying = cols_to_stack,
                                    v.names = "Value",
                                    timevar = "Measure Type",
                                    times = cols_to_stack,
                                    new.row.names = 1:long_row_count,
                                    direction = "long"
)

#remove ID column
complete_susp_rates_long$id <- NULL

#Rename Measure Type
complete_susp_rates_long$`Measure Type` <- "Percent"

#Rename Variable columns
complete_susp_rates_long$`Variable` <- "Suspensions"

#Recode types of NAs for distrinction between real NAs and 0s
complete_susp_rates_long$Value[which(is.na(complete_susp_rates_long$Value))] <- 0 ##from backfill (not applicable for that district)
complete_susp_rates_long$Value[which(complete_susp_rates_long$Value == "*")] <- -9999 ##suppressions
complete_susp_rates_long$Value[which(complete_susp_rates_long$Value == "N/A")] <- -6666 ##missing

#Recode grades and make a factor for sorting
oldgrades <- c("K", "01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
newgrades <- factor(c("Kindergarten", "Grade 1", "Grade 2", "Grade 3", "Grade 4", "Grade 5", 
                      "Grade 6", "Grade 7", "Grade 8", "Grade 9", "Grade 10", "Grade 11", "Grade 12"))

complete_susp_rates_long$Grade <- newgrades[match(complete_susp_rates_long$Grade, oldgrades)]
complete_susp_rates_long$Grade <- factor(complete_susp_rates_long$Grade, level = newgrades)

#Order and sort columns
complete_susp_rates_long <- complete_susp_rates_long %>% 
  select(District, FIPS, Year, Grade, `Measure Type`, Variable, Value) %>% 
  arrange(District, Year, Grade)

# Write to File
write.table(
  complete_susp_rates_long,
  file.path(getwd(), "data", "suspension_rates_by_grade_2016.csv"),
  sep = ",",
  row.names = F
)


