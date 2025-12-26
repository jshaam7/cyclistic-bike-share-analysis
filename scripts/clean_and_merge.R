install.packages(c("here", "skimr", "janitor"))
library(readr)
library(dplyr)
library(janitor)
library(skimr)
library(here)

# Read CSV files and clean column names
trips_2019 <- read_csv(here("data/raw/Divvy_Trips_2019_Q1.csv"))
trips_2020 <- read_csv(here("data/raw/Divvy_Trips_2020_Q1.csv"))
trips_2019 <- clean_names(trips_2019)
trips_2020 <- clean_names(trips_2020)

#Check column name to analyze possible mappings
names(trips_2019)
names(trips_2020)

# Rename id + time columns
trips_2019$ride_id <- trips_2019$trip_id
trips_2019$started_at <- trips_2019$start_time
trips_2019$ended_at <- trips_2019$end_time

# Rename station columns to 2020 naming
trips_2019$start_station_id <- trips_2019$from_station_id
trips_2019$start_station_name <- trips_2019$from_station_name
trips_2019$end_station_id <- trips_2019$to_station_id
trips_2019$end_station_name <- trips_2019$to_station_name

# Mapping usertype -> member_casual 
trips_2019$usertype <- trimws(trips_2019$usertype)
trips_2019$member_casual <- NA
trips_2019$member_casual[trips_2019$usertype == "Subscriber"] <- "member"
trips_2019$member_casual[trips_2019$usertype == "Customer"]   <- "casual"

# 2019 does not have rideable_type or lat/lng, so add as NA
trips_2019$rideable_type <- NA
trips_2019$start_lat <- NA
trips_2019$start_lng <- NA
trips_2019$end_lat <- NA
trips_2019$end_lng <- NA

master_cols <- c(
  "ride_id",
  "rideable_type",
  "started_at",
  "ended_at",
  "start_station_name",
  "start_station_id",
  "end_station_name",
  "end_station_id",
  "start_lat",
  "start_lng",
  "end_lat",
  "end_lng",
  "member_casual"
)

# Make sure both datasets contain all master columns
missing_2019 <- setdiff(master_cols, names(trips_2019))
missing_2020 <- setdiff(master_cols, names(trips_2020))

for (m in missing_2019) trips_2019[[m]] <- NA
for (m in missing_2020) trips_2020[[m]] <- NA

# Reorder and subset to the same schema
trips_2019_std <- trips_2019[ , master_cols]
trips_2020_std <- trips_2020[ , master_cols]

# Bind rows
trips_master <- rbind(trips_2019_std, trips_2020_std)

# Ride length + weekday; case study specifications 
# Both 2019 and 2020 are in: "YYYY-MM-DD HH:MM"
trips_master$started_at_parsed <- strptime(trips_master$started_at, 
                                           "%Y-%m-%d %H:%M")
trips_master$ended_at_parsed   <- strptime(trips_master$ended_at,   
                                           "%Y-%m-%d %H:%M")

# Ride length in minutes (case-study style)
trips_master$ride_length_min <- as.numeric(
  difftime(trips_master$ended_at_parsed, 
           trips_master$started_at_parsed, units = "secs")
)

# Day of week from started_at
trips_master$weekday <- weekdays(trips_master$started_at_parsed)

# Checks
nrow(trips_master)
table(trips_master$member_casual, useNA = "ifany")

# Remove negative/zero ride lengths 
trips_master <- trips_master[!is.na(trips_master$ride_length_min) & 
                               trips_master$ride_length_min > 0, ]

# Step asked for in case study document 
trips_master$day_of_week <- weekdays(trips_master$started_at_parsed)

# Saving master dataset
write_csv(trips_master, here("data/processed/trips_master_2019Q1_2020Q1.csv"))

