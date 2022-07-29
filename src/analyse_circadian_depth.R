# Analyse circadian depth pattern: distance from seabed
# By Pieterjan Verhelst
# pieterjan.verhelst@inbo.be


# Load packages
library(tidyverse) # To do datascience
library(lubridate)
library(PairedData)

# 1. Read data ####
data <- read.csv("./data/interim/data_circadian_tidal_moon_sun_5min.csv")
data$ID <- factor(data$ID)
data$datetime <- ymd_hms(data$datetime)
data$night_day <- factor(data$night_day)
data <- data %>%
  rename(direction_x = U,
         direction_y = V)

# Remove DVM data from eel A17535
data <- data[!(data$ID == "17535" & data$datetime >= '2020-01-11 00:00:00'),]

# Arrange data set according to tag ID and datetime, so min and max are calculated accordingly
data <-
  data %>%
  arrange(ID, datetime)


# Calculate depth relative to max depth
data_max_depth <- data %>%
  group_by(ID, Date) %>%
  summarise(max_depth = min(corrected_depth))
data <- left_join(data, data_max_depth, by = c("ID","Date"))
data$rel_depth <- data$corrected_depth / data$max_depth

# Calculate distance from seabed
data$dist_from_seabed <- data$corrected_depth - data$max_depth


# Remove NA in circadian phase
data <- data[!is.na(data$night_day),]


# Calculate summary
aggregate(data$dist_from_seabed, list(data$night_day), mean)
aggregate(data$dist_from_seabed, list(data$night_day), sd)
aggregate(data$dist_from_seabed, list(data$night_day), median)
aggregate(data$dist_from_seabed, list(data$night_day), min)
aggregate(data$dist_from_seabed, list(data$night_day), max)
aggregate(data$dist_from_seabed, list(data$night_day, data$ID), median) # per eel


# Create plot
boxplot <- ggplot(data, aes(x=night_day, y=dist_from_seabed)) + 
  geom_boxplot() +
  theme_minimal() +
  ylab("Distance from seabed (m)") +
  xlab("Circadian phase") +
  theme(axis.title.y = element_text(margin = margin(r = 10))) +
  theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 22)) #+
#ylim(0, 20)
boxplot


# Another plot
# summarise
aggregated <- aggregate(data$dist_from_seabed, list(data$night_day, data$ID), median)
aggregated <- rename(aggregated, 
                     night_day = Group.1,
                     ID = Group.2,
                     depth = x)
# Subset night data before treatment
night <- subset(aggregated,  night_day == "night", depth,
                drop = TRUE)
# subset day data after treatment
day <- subset(aggregated,  night_day == "day", depth,
              drop = TRUE)
# Plot paired data
pd <- paired(day, night)
plot(pd, type = "profile") + 
  theme_bw()


# Paired samples Wilcoxon test
# See: http://www.sthda.com/english/wiki/paired-samples-wilcoxon-test-in-r
wilcox.test(day, night, paired = TRUE)



