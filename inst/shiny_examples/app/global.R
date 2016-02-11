library(plyr)
library(dplyr) # Data manipulation
library(lubridate) # Get year component of a date
library(ggplot2) # For plotting
library(XLConnect)


# Open data
#nfl.db <- src_postgres("nfl", host = "localhost", user = "dominik")
nfl.db <- src_postgres("nfl", host = "localhost", user = "msharp", 
                       password = "nflpassword")
nfl <- tbl(nfl.db, "scores")


# Get years in db
nfl.years <- nfl %>%
  select(Date) %>%
  collect()

nfl.years <- unique(year(nfl.years$Date))
