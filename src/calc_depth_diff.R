# Calculate depth differences between minima and maxima
# By Damiano Oldoni
# damiano.oldoni@inbo.be

# Packages
library(tidyverse) # To do datascience
library(tidylog)  # To get infos about dplyr functions
library(lubridate)



# 1. Import data ####
data <- read_csv("./data/interim/data_circadian_tidal_moon.csv",
                 na = "", 
                 col_types = list(sunrise = col_datetime(),
                                  previous_sunset = col_datetime(),
                                  next_sunrise = col_datetime(),
                                  next_sunmoment = col_datetime(),
                                  direction = col_double()),          # set direction as numeric
                 guess_max = 100000)


# 2. Find minima and maxima ####
data <-
  data %>%
  group_by(ID) %>%
  mutate(is_maximum = if_else(corrected_depth - lag(corrected_depth, 1) > 0 &
                                corrected_depth - lead(corrected_depth, 1) >= 0,
                              TRUE,
                              FALSE),
         is_minimum = if_else(corrected_depth - lag(corrected_depth, 1) < 0 &
                                corrected_depth - lead(corrected_depth, 1) <= 0,
                              TRUE,
                              FALSE)
  ) %>%
  ungroup()

# 3. Take minima and maxima only ####
data_min_max <-
  data %>%
  filter(is_maximum == TRUE | is_minimum == TRUE)

# 4. Calculate changes in max and min depths ####
data_min_max <-
  data_min_max %>%
  group_by(ID) %>%
  mutate(depth_change = abs(corrected_depth - lag(corrected_depth))) %>%
  ungroup()

# Remove depth changes below 1 meter
#data_min_max_1m <-
#  data_min_max %>%
#  filter(depth_change >= 1)

ggplot(data_min_max) +
  geom_histogram(aes(depth_change, color = night_day),
                 binwidth = 10,
                 position = "dodge",
                 fill = "white") +
  facet_wrap(~ID, nrow = 2)

# Arrange depth changes in descending order and see in which phase of the day if occurs
data_min_max %>%
  arrange(desc(depth_change)) %>%
  select(ID, night_day, depth_change)


# Calculate summary values
data_min_max_no_na <- data_min_max[!is.na(data_min_max$depth_change),]
aggregate(data_min_max_no_na$depth_change, list(data_min_max_no_na$night_day, data_min_max_no_na$ID), mean)



# 5. Write csv ####
write.csv(data_min_max, "./data/interim/data_depth_diff.csv")





