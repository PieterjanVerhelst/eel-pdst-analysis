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
# Belgian eels
eel_A16031 <- read_csv("./data/interim/sensorlogs/sensor_A16031_08-11-2019.csv")
eel_A15805 <- read_csv("./data/interim/sensorlogs/sensor_A15805_03-04-2019.csv")
eel_A15981 <- read_csv("./data/interim/sensorlogs/sensor_A15981_06-02-2020.csv")
eel_A15714 <- read_csv("./data/interim/sensorlogs/sensor_A15714_13-02-2019.csv")
eel_A15777 <- read_csv("./data/interim/sensorlogs/sensor_A15777_12-11-2019.csv")
eel_A15706 <- read_csv("./data/interim/sensorlogs/sensor_A15706_01-08-2019.csv")
eel_A17443 <- read_csv("./data/interim/sensorlogs/sensor_A17443_27-01-2020.csv")
eel_A17499 <- read_csv("./data/interim/sensorlogs/sensor_A17499_20-02-2020.csv")
eel_A17513 <- read_csv("./data/interim/sensorlogs/sensor_A17513_20-02-2020.csv")
eel_A17534 <- read_csv("./data/interim/sensorlogs/sensor_A17534_17-03-2020.csv")
eel_A17526 <- read_csv("./data/interim/sensorlogs/sensor_A17526_17-03-2020.csv")
eel_A17522 <- read_csv("./data/interim/sensorlogs/sensor_A17522_17-03-2020.csv")
eel_A17492 <- read_csv("./data/interim/sensorlogs/sensor_A17492_05-05-2020.csv")
eel_A17508 <- read_csv("./data/interim/sensorlogs/sensor_A17508_05-05-2020.csv")
eel_A17536 <- read_csv("./data/interim/sensorlogs/sensor_A17536_05-05-2020.csv")
eel_A17538 <- read_csv("./data/interim/sensorlogs/sensor_A17538_05-05-2020.csv")
eel_A17537 <- read_csv("./data/interim/sensorlogs/sensor_A17537_05-05-2020.csv")
eel_A17510 <- read_csv("./data/interim/sensorlogs/sensor_A17510_22-06-2020.csv")
eel_A15789 <- read_csv("./data/interim/sensorlogs/sensor_A15789_22-06-2020.csv")

# German eels
eel_A09359 <- read_csv("./data/interim/sensorlogs/sensor_A09359_11-12-2012.csv")
eel_A09359$track_tag_id <- "A09359"
eel_A09349 <- read_csv("./data/interim/sensorlogs/sensor_A09349_07-01-2013_2.csv")
eel_A09349$track_tag_id <- "A09349"
eel_A09358 <- read_csv("./data/interim/sensorlogs/sensor_A09358_26-11-2012.csv")
eel_A09358$track_tag_id <- "A09358"
eel_A09374 <- read_csv("./data/interim/sensorlogs/sensor_A09374_09-01-2013.csv")
eel_A09374$track_tag_id <- "A09374"
eel_A09377 <- read_csv("./data/interim/sensorlogs/sensor_A09377_18-04-2013_2.csv")
eel_A09377$track_tag_id <- "A09377"
eel_A09393 <- read_csv("./data/interim/sensorlogs/sensor_A09393_02-08-2013.csv")
eel_A09393$track_tag_id <- "A09393"
eel_A09411 <- read_csv("./data/interim/sensorlogs/sensor_A09411_07-03-13.csv")
eel_A09411$track_tag_id <- "A09411"
eel_A09423 <- read_csv("./data/interim/sensorlogs/sensor_A09423_11-12-2012.csv")
eel_A09423$track_tag_id <- "A09423"
eel_A09424 <- read_csv("./data/interim/sensorlogs/sensor_A09424_21-06-2013.csv")
eel_A09424$track_tag_id <- "A09424"



# Combine all datasets ####
all <- do.call("rbind", list(eel_A16031,
                             eel_A15805,
                             eel_A15981,
                             eel_A15714,
                             eel_A15777,
                             eel_A15706,
                             eel_A17443,
                             eel_A17499,
                             eel_A17513,
                             eel_A17534,
                             eel_A17526,
                             eel_A17522,
                             eel_A17492,
                             eel_A17508,
                             eel_A17536,
                             eel_A17538,
                             eel_A17537,
                             eel_A17510,
                             eel_A15789,
                             eel_A09359,
                             eel_A09349,
                             eel_A09358,
                             eel_A09374,
                             eel_A09377,
                             eel_A09393,
                             eel_A09411,
                             eel_A09423,
                             eel_A09424))

all <- all %>%
  rename(ID = track_tag_id)

# Remove seperate files
rm(eel_A16031,
   eel_A15805,
   eel_A15981,
   eel_A15714,
   eel_A15777,
   eel_A15706,
   eel_A17443,
   eel_A17499,
   eel_A17513,
   eel_A17534,
   eel_A17526,
   eel_A17522,
   eel_A17492,
   eel_A17508,
   eel_A17536,
   eel_A17538,
   eel_A17537,
   eel_A17510,
   eel_A15789,
   eel_A09359,
   eel_A09349,
   eel_A09358,
   eel_A09374,
   eel_A09377,
   eel_A09393,
   eel_A09411,
   eel_A09423,
   eel_A09424)


# 2. Read in parameter file ####
parameters <- read_csv("./data/external/parameters.csv")
parameters$start_datetime <-  dmy_hm(parameters$start_datetime)
parameters$end_datetime <-  dmy_hm(parameters$end_datetime)
parameters$bank_datetime <-  dmy_hm(parameters$bank_datetime)
parameters$popoff_datetime <-  dmy_hm(parameters$popoff_datetime)
#parameters$popoff_datetime15 <-  parameters$popoff_datetime + minutes(15)
parameters$UTC <-  factor(parameters$UTC)


# Temporarily remove 2 PSAT eels
parameters <- parameters %>%
  filter(ID != c('112061')) %>%
  filter(ID != c('112064'))


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

# Select rows with bank datetime and popoff datetime + 15 minutes (= when tag was at atmospheric pressure)
bank_popoff <- all %>%
  group_by(ID) %>%
  filter(datetime == bank_datetime | datetime == popoff_datetime,
         pressure_correction != 0) %>%
  mutate(numericdate = as.numeric(datetime))

# Apply linear regression per ID
# https://cran.r-project.org/web/packages/broom/vignettes/broom_and_dplyr.html
#model <- bank_popoff %>%
#  group_by(ID) %>%
#  do(fit_model = lm(pressure ~ datetime, data = .)) 
model <- bank_popoff %>%
  nest(data = -ID) %>% 
  mutate(
    fit = map(data, ~ lm(pressure ~ datetime, data = .x)),
    tidied = map(fit, tidy)
  ) %>% 
  unnest(tidied)

# Remove redundant columns
model <- select(model, -data, -fit, -std.error, -statistic, -p.value)


# Extract model coefficients in a tidy dataframe
#model_coef <- tidy(model, fit_model) %>%
#  spread(key = term, value = estimate) %>%
#  select(-std.error, -statistic, -p.value) %>%
#  rename(intercept_val = `(Intercept)`,
#         datetime_val = datetime)

model_coef <- model %>%
  spread(key = term, value = estimate) %>%
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




