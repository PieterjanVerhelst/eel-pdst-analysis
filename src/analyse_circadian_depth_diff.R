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
aggregate(data$depth_change, list(data$night_day, data$ID), mean) # per eel

# Create plot
plot(data$night_day, data$depth_change)
