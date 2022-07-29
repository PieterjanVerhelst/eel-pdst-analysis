# Analyse tidal depth pattern: actual depth
# By Pieterjan Verhelst
# pieterjan.verhelst@inbo.be


# Load packages
library(tidyverse) # To do datascience
library(lubridate)
library(PairedData)
library(car)


# 1. Read data ####
data <- read.csv("./data/interim/data_tidal_phases.csv")
data$ID <- factor(data$ID)
data$datetime <- ymd_hms(data$datetime)
data$night_day <- factor(data$night_day)
data$tidal_phase <- factor(data$tidal_phase)


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


# Remove NA in tidal phase
data <- data[!is.na(data$tidal_phase),]


# Calculate summary
aggregate(data$dist_from_seabed, list(data$tidal_phase), mean)
aggregate(data$dist_from_seabed, list(data$tidal_phase), sd)
aggregate(data$dist_from_seabed, list(data$tidal_phase), median)
aggregate(data$dist_from_seabed, list(data$tidal_phase), min)
aggregate(data$dist_from_seabed, list(data$tidal_phase), max)
aggregate(data$dist_from_seabed, list(data$tidal_phase, data$ID), median) # per eel


# Create plot
boxplot <- ggplot(data, aes(x=tidal_phase, y=dist_from_seabed)) + 
  geom_boxplot() +
  theme_minimal() +
  ylab("Distance from seabed (m)") +
  xlab("Tidal phase") +
  theme(axis.title.y = element_text(margin = margin(r = 10))) +
  theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 22)) #+
#ylim(0, 20)
boxplot


# Another plot
# summarise
aggregated <- aggregate(data$dist_from_seabed, list(data$tidal_phase, data$ID), median)
aggregated <- rename(aggregated, 
                     tidal_phase = Group.1,
                     ID = Group.2,
                     depth = x)
# Subset night data before treatment
ebb <- subset(aggregated,  tidal_phase == "ebb", depth,
                drop = TRUE)
# subset day data after treatment
flood <- subset(aggregated,  tidal_phase == "flood", depth,
              drop = TRUE)
# Plot paired data
pd <- paired(ebb, flood)
plot(pd, type = "profile") + 
  theme_bw()


# Analyse data
# Check assumptions
# 1. Normality

# Create qqplot with qqline
qqnorm(data$dist_from_seabed)
qqline(data$dist_from_seabed)

# Shapiro test
# The p-value > 0.05 implying that the distribution of the data are not significantly different from normal distribution. In other words, we can assume the normality.
shapiro.test(data$dist_from_seabed)

# 2. Check homogeneity of variances
# Levene’s test
# Levene’s test is used to assess whether the variances of two or more populations are equal.
# https://www.datanovia.com/en/lessons/homogeneity-of-variance-test-in-r/
# When p > 0.05, there is no significant difference between the two variances.
leveneTest(dist_from_seabed ~ tidal_phase, data = data)



# Paired t-test
# Assumptions not met

# Paired samples Wilcoxon test
# See: http://www.sthda.com/english/wiki/paired-samples-wilcoxon-test-in-r
wilcox.test(ebb, flood, paired = TRUE)



