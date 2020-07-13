# Create sensor log file with all eels for analysis, taking into account time settings (UTC), pressure drift and redundant data (e.g. data when on the shelf and during DVM)
# By Pieterjan Verhelst
# Pieterjan.Verhelst@UGent.be


# Set time zone
Sys.setenv(TZ='GMT')
Sys.timezone()

# Packages
library(tidyverse)
library(lubridate)
library(broom) # run regression per grouping variable (in 'Correct for depth drift')



# 1. Read in data ####
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



# 2. Read in parameter file ####
parameters <- read_csv("./data/external/parameters.csv")
parameters$start_datetime <-  dmy_hm(parameters$start_datetime)
parameters$end_datetime <-  dmy_hm(parameters$end_datetime)
parameters$bank_datetime <-  dmy_hm(parameters$bank_datetime)
parameters$popoff_datetime <-  dmy_hm(parameters$popoff_datetime)
parameters$UTC <-  factor(parameters$UTC)



# 3. Aggregate data per 1 min ####
all <- all %>%
  group_by(ID) %>%
  fill(temperature) %>%   # Fill temperature NA's with previous measured value
  mutate(datetime = dmy_hms(datetime))
  
all$datetime2 <- droplevels(cut(all$datetime, breaks="1 min"))   # 1 min cut

all <- all %>%
  group_by(ID, datetime2) %>%
  summarise(pressure = mean(pressure),            # Calculate mean pressure
            temperature = mean(temperature))      # Calculate mean temperature

all$datetime2 <- ymd_hms(all$datetime2)


# 4. Time zone correction ####
all <- left_join(all, parameters, by = "ID") %>%
  mutate(datetime = ifelse(UTC == "-1", (datetime2 - (60*60)), 
                            ifelse(UTC == "-2", datetime2 - (2*60*60))))
all$datetime <- as.POSIXct(all$datetime, origin='1970-01-01 00:00:00')
all$time_diff <- all$datetime2 - all$datetime    # Check for time zone correction


# 5. Correct for depth drift ####

# Select rows with bank datetime and popoff datetime (= when tag was at atmospheric pressure)
bank_popoff <- all %>%
  group_by(ID) %>%
  filter(datetime == bank_datetime | datetime == popoff_datetime,
         pressure_correction != 0) %>%
  mutate(numericdate = as.numeric(datetime))

# Apply linear regression per ID
model <- bank_popoff %>%
  group_by(ID) %>%
  do(fit_model = lm(pressure ~ datetime, data = .))

# Extract model coefficients in a tidy dataframe
model_coef <- tidy(model, fit_model) %>%
  spread(key = term, value = estimate) %>%
  select(-std.error, -statistic, -p.value) %>%
  rename(intercept_val = `(Intercept)`,
         datetime_val = datetime)

# Join values to dataset
all <- left_join(all, model_coef, by = "ID")

# Calculate corrected depth
all$numericdate <- as.numeric(all$datetime)
all$regression <- (all$datetime_val  * all$numericdate)  + all$intercept_val
all$corrected_depth <- all$pressure - all$regression

# For some eels, no correction could be applied. Hence, take original pressure data.
all <- all %>%
  mutate(corrected_depth2 = coalesce(corrected_depth, pressure))

# Reverse depth
all$corrected_depth2 <- all$corrected_depth2 * -1


# 6. Remove data before release and
# - till DVM
# - till one hour before predatino
# - 15 minutes before popoff time
# --> Hence, select data on continental shelf
all <- all %>%
  group_by(ID) %>%
  filter(datetime >= start_datetime, datetime <= end_datetime)


# Check depth profiles before and after correction
#ind_eel <- filter(all, ID == "A16031")

#plot(ind_eel$datetime, ind_eel$pressure)
#plot(ind_eel$datetime, ind_eel$corrected_depth2)



# 7. Clean dataset  ####
all <- all %>%
  select(ID, datetime, numericdate, corrected_depth2, temperature) %>%
  rename(corrected_depth = corrected_depth2)




write.csv(all, "./data/interim/batch_processed_eels.csv")




