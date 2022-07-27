# Analyse circadian depth pattern
# By Pieterjan Verhelst
# pieterjan.verhelst@Uinbo.be


# Load packages
library(tidyverse) # To do datascience
library(lubridate)

# Load data
data <- read_csv("./data/interim/data_depth_diff.csv")

data$...1 <- NULL
data$ID <- factor(data$ID)
data$night_day <- factor(data$night_day)

# Remove NA in depth_change (= first row of each animal)
data <- data[!is.na(data$depth_change),]

# Remove NA in circadian phase
data <- data[!is.na(data$night_day),]

# Calculate summary
aggregate(data$depth_change, list(data$night_day), mean)
aggregate(data$depth_change, list(data$night_day), sd)
aggregate(data$depth_change, list(data$night_day), min)
aggregate(data$depth_change, list(data$night_day), max)
aggregate(data$depth_change, list(data$night_day, data$ID), mean) # per eel

# Create plot
boxplot <- ggplot(data, aes(x=night_day, y=depth_change)) + 
  geom_boxplot() +
  theme_minimal() +
  ylab("Depth difference (m)") +
  xlab("Circadian phase") +
  theme(axis.title.y = element_text(margin = margin(r = 10))) +
  theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 22)) #+
  #ylim(0, 20)
boxplot


