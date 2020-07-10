# Create sensor log file with all eels for analysis, taking into account time settings (UTC), pressure drift and redundant data (e.g. data when on the shelf and during DVM)
# By Pieterjan Verhelst
# Pieterjan.Verhelst@UGent.be


# Set time zone
Sys.setenv(TZ='GMT')
Sys.timezone()

# Packages
library(tidyverse)
library(lubridate)



# Read in data ####
eel_A16031 <- read_csv("./data/interim/sensorlogs/sensor_A16031_08-11-2019.csv")
eel_A15714 <- read_csv("./data/interim/sensorlogs/sensor_A15714_13-02-2019.csv")
eel_A15777 <- read_csv("./data/interim/sensorlogs/sensor_A15777_12-11-2019.csv")
eel_A09359 <- read_csv("./data/interim/sensorlogs/sensor_A09359_11-12-2012.csv")
    eel_A09359$track_tag_id <- "A09359"


# Combine all datasets ####
all <- do.call("rbind", list(eel_A09359,
                             eel_A15714,
                             eel_A15777,
                             eel_A16031))
    
all <- all %>%
  rename(ID = track_tag_id)
    

#list_dfs <- list("A16031" = eel_A16031,
#                 "A15714" = eel_A15714,
#                 "A15777" = eel_A15777,
#                 "A09359" = eel_A09359)



# Read in parameter file ####
parameters <- read_csv("./additionals/parameters.csv")
parameters$start_datetime <-  dmy_hm(parameters$start_datetime)
parameters$end_datetime <-  dmy_hm(parameters$end_datetime)
parameters$UTC <-  factor(parameters$UTC)




# Eel A16031####


# Aggregate data
eel_A16031$datetime <- dmy_hms(eel_A16031$datetime)
eel_A16031$datetime2 <- droplevels(cut(eel_A16031$datetime, breaks="1 min"))   # 1 min cut
eel_A16031 <- aggregate(cbind(pressure, temperature) ~ datetime2, data=eel_A16031, FUN=mean, na.rm=TRUE) 
eel_A16031$datetime2 <- ymd_hms(eel_A16031$datetime2)

# Correct for Brussels Time zone UTC + 1
eel_A16031$datetime2 <- eel_A16031$datetime2 - (60*60)
#aggdata$datetime2 <- aggdata$datetime2 - (2*60*60)  # - 2 hours when UTC+2 (summer daylight saving time)
eel_A16031$datetime2 <- as.POSIXct(eel_A16031$datetime2, "%Y-%m-%d %H:%M:%S", tz = "GMT")

# Correct for depth drift
plot(eel_A16031$datetime2, eel_A16031$pressure)
# Select date: moment of release - 15 min and pop-off moment (moment it was certainly at the surface)
subset <- filter(eel_A16031, 
                 datetime2 == as.POSIXct("2018-12-09 18:00:00", "%Y-%m-%d %H:%M:%S", tz = "GMT") |
                   datetime2 == as.POSIXct("2019-02-16 04:25:00", "%Y-%m-%d %H:%M:%S", tz = "GMT"))
plot(subset$datetime2, subset$pressure)
abline(lm(subset$pressure ~ subset$datetime2))
lm(subset$pressure ~ subset$datetime2)  # To get coefficient and estimates
# depth = (5.567e-07 * date)  -8.589e+02
eel_A16031$numericdate <- as.numeric(eel_A16031$datetime2)
eel_A16031$regression <- (5.567e-07    * eel_A16031$numericdate)   -8.589e+02
eel_A16031$corrected_depth <- eel_A16031$pressure - eel_A16031$regression

# Reverse depth
eel_A16031$corrected_depth <- eel_A16031$corrected_depth * -1

# Remove data before release and DVM part; hence, select data on continental shelf
eel_A16031 <- filter(eel_A16031, datetime2 >= "2018-12-09 18:15:00", datetime2 <= "2019-02-13 00:00:00")

###################





# Aggregate data per 1 min ####
all2 <- all %>%
  group_by(ID) %>%
  fill(temperature) %>%   # Fill temperature NA's with previous measured value
  mutate(datetime = dmy_hms(datetime))
  
all2$datetime2 <- droplevels(cut(all2$datetime, breaks="1 min"))   # 1 min cut

all2 <- all2 %>%
  group_by(ID, datetime2) %>%
  summarise(pressure = mean(pressure),            # Calculate mean pressure
            temperature = mean(temperature))      # Calculate mean temperature

all2$datetime2 <- ymd_hms(all2$datetime2)


# Time zone correction ####
all2 <- left_join(all2, parameters, by = "ID") %>%
  mutate(datetime = ifelse(UTC == "-1", (datetime2 - (60*60)), 
                            ifelse(UTC == "-2", datetime2 - (2*60*60))))
all2$datetime <- as.POSIXct(all2$datetime, origin='1970-01-01 00:00:00')
all2$time_diff <- all2$datetime2 - all2$datetime    # Check for time zone correction








