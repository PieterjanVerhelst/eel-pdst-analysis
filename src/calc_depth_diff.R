# Calculate depth differences between minima and maxima
# By Damiano Oldoni
# damiano.oldoni@inbo.be

# Packages
library(tidyverse) # To do datascience
library(tidylog)  # To get infos about dplyr functions
library(suncalc)  # To get sunrise, sunset




# Import data ####
data <- read_csv("./data/interim/data_circadian_tidal.csv", na = "", guess_max = 100000)


# Find minima and maxima ####
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

# Take minima and maxima only ####
data_min_max <-
  data %>%
  filter(is_maximum == TRUE | is_minimum == TRUE)

# Calculate changes in max and min depths ####
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



# Create plot with day night ####
# Create subsets of several days
subset <- filter(data_min_max,
                 ID == "16031",
                 datetime >= "2019-02-01 00:00:00", datetime <= "2019-02-07 00:00:00")

# Create line every 24 hours
gnu <-  seq.POSIXt(from = lubridate::floor_date(subset$datetime[1], "day"), to= subset$datetime[nrow(subset)], by = 86400)
class(lubridate::floor_date(subset$datetime[1], "day"))

# Create plot
fig_circadian <- ggplot(data = subset, aes(x = datetime, y = depth_change), size = 1.0, alpha = 0.5, colour = "black") +
  geom_rect(aes(xmin=sunrise, xmax=sunset, ymin=-Inf, ymax=+Inf), fill='grey', alpha=0.3) +
  geom_line(binaxis='x', size=1.0, binwidth = 1) +
  #scale_y_continuous(breaks = seq(8.000, 12.000, by = 500)) +
  #scale_y_continuous(sec.axis = sec_axis(~.*2, name = "Pressure (m)")) +
  theme_minimal() +
  ylab("Depth difference (m)") +
  xlab("Date") +
  theme(axis.title.y = element_text(margin = margin(r = 10))) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14)) +
  scale_x_datetime(date_breaks  ="1 day") +
  #geom_vline(xintercept=ymd_hms(release), colour="blue") + # Release date and time
  geom_vline(xintercept=gnu, color = "red", size = 1) 
fig_circadian


# Calculate summary values
data_min_max <- na.omit(data_min_max)
aggregate(data_min_max$depth_change, list(data_min_max$night_day, data_min_max$ID), mean)

