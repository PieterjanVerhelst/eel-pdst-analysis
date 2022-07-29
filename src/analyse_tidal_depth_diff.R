# Analyse tidal depth pattern: depth difference
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

# Remove NA in tidal phase
data <- data[!is.na(data$tidal_phase),]

# Arrange data set according to tag ID and datetime, so min and max are calculated accordingly
data <-
  data %>%
  arrange(ID, datetime)


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
  mutate(depth_change = abs(corrected_depth - lag(corrected_depth)),
         diving_time = numericdate - lag(numericdate)) %>%
  ungroup()

data_min_max$diving_speed <- data_min_max$depth_change/data_min_max$diving_time


# Remove depth changes below 1 meter
#data_min_max_1m <-
#  data_min_max %>%
#  filter(depth_change >= 1)

ggplot(data_min_max) +
  geom_histogram(aes(depth_change, color = tidal_phase),
                 binwidth = 10,
                 position = "dodge",
                 fill = "white") +
  facet_wrap(~ID, nrow = 2)

# Arrange depth changes in descending order and see in which tidal phase if occurs
data_min_max %>%
  arrange(desc(depth_change)) %>%
  dplyr::select(ID, tidal_phase, depth_change)


# Remove NA values
data_min_max_no_na <- data_min_max[!is.na(data_min_max$depth_change),]


# 5. Calculate summary ####
aggregate(data_min_max_no_na$depth_change, list(data_min_max_no_na$tidal_phase), mean)
aggregate(data_min_max_no_na$depth_change, list(data_min_max_no_na$tidal_phase), sd)
aggregate(data_min_max_no_na$depth_change, list(data_min_max_no_na$tidal_phase), median)
aggregate(data_min_max_no_na$depth_change, list(data_min_max_no_na$tidal_phase), min)
aggregate(data_min_max_no_na$depth_change, list(data_min_max_no_na$tidal_phase), max)
aggregate(data_min_max_no_na$depth_change, list(data_min_max_no_na$tidal_phase, data_min_max_no_na$ID), median) # per eel

# Create plot
boxplot <- ggplot(data_min_max_no_na, aes(x=tidal_phase, y=depth_change)) + 
  geom_boxplot() +
  theme_minimal() +
  ylab("Depth difference (m)") +
  xlab("Tidal phase") +
  theme(axis.title.y = element_text(margin = margin(r = 10))) +
  theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 22)) #+
#ylim(0, 20)
boxplot


# Another plot
# summarise
aggregated <- aggregate(data_min_max_no_na$depth_change, list(data_min_max_no_na$tidal_phase, data_min_max_no_na$ID), median)
aggregated <- rename(aggregated, 
                     tidal_phase = Group.1,
                     ID = Group.2,
                     median_depth_change = x)
# Subset night data before treatment
ebb <- subset(aggregated,  tidal_phase == "ebb", median_depth_change,
                drop = TRUE)
# subset day data after treatment
flood <- subset(aggregated,  tidal_phase == "flood", median_depth_change,
              drop = TRUE)
# Plot paired data
pd <- paired(ebb, flood)
plot(pd, type = "profile") + 
  theme_bw()


# Analyse data
# Check assumptions
# 1. Normality

# Create qqplot with qqline
qqnorm(data_min_max_no_na$depth_change)
qqline(data_min_max_no_na$depth_change)

# Shapiro test
# The p-value > 0.05 implying that the distribution of the data are not significantly different from normal distribution. In other words, we can assume the normality.
shapiro.test(data_min_max_no_na$depth_change)

# 2. Check homogeneity of variances
# Levene’s test
# Levene’s test is used to assess whether the variances of two or more populations are equal.
# https://www.datanovia.com/en/lessons/homogeneity-of-variance-test-in-r/
# When p > 0.05, there is no significant difference between the two variances.
leveneTest(depth_change ~ tidal_phase, data = data_min_max_no_na)



# Paired t-test
# Assumptions not met

# Paired samples Wilcoxon test
# See: http://www.sthda.com/english/wiki/paired-samples-wilcoxon-test-in-r
wilcox.test(ebb, flood, paired = TRUE)




