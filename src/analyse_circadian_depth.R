# Analyse circadian depth pattern: actual depth
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


# Remove NA in circadian phase
data <- data[!is.na(data$night_day),]


# Calculate summary
aggregate(data$corrected_depth, list(data$night_day), mean)
aggregate(data$corrected_depth, list(data$night_day), sd)
aggregate(data$corrected_depth, list(data$night_day), median)
aggregate(data$corrected_depth, list(data$night_day), min)
aggregate(data$corrected_depth, list(data$night_day), max)
aggregate(data$corrected_depth, list(data$night_day, data$ID), median) # per eel


# Create plot
boxplot <- ggplot(data, aes(x=night_day, y=corrected_depth)) + 
  geom_boxplot() +
  theme_minimal() +
  ylab("Depth (m)") +
  xlab("Circadian phase") +
  theme(axis.title.y = element_text(margin = margin(r = 10))) +
  theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 22)) #+
#ylim(0, 20)
boxplot


# Another plot
# summarise
aggregated <- aggregate(data$corrected_depth, list(data$night_day, data$ID), median)
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



