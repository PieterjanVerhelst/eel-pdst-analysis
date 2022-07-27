# Analyse circadian depth pattern
# By Pieterjan Verhelst
# pieterjan.verhelst@Uinbo.be


# Load packages
library(tidyverse) # To do datascience
library(lubridate)
library(PairedData)


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
aggregate(data$depth_change, list(data$night_day), median)
aggregate(data$depth_change, list(data$night_day), min)
aggregate(data$depth_change, list(data$night_day), max)
aggregate(data$depth_change, list(data$night_day, data$ID), median) # per eel

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


# Another plot
# summarise
aggregated <- aggregate(data$depth_change, list(data$night_day, data$ID), median)
aggregated <- rename(aggregated, 
                     night_day = Group.1,
                     ID = Group.2,
                     median_depth_change = x)
# Subset night data before treatment
night <- subset(aggregated,  night_day == "night", median_depth_change,
                 drop = TRUE)
# subset day data after treatment
day <- subset(aggregated,  night_day == "day", median_depth_change,
                drop = TRUE)
# Plot paired data
pd <- paired(day, night)
plot(pd, type = "profile") + theme_bw()


# Analyse data
# Check assumptions
# 1. Normality

# Create qqplot with qqline
qqnorm(data$depth_change)
qqline(data$depth_change)

# Shapiro test
# The p-value > 0.05 implying that the distribution of the data are not significantly different from normal distribution. In other words, we can assume the normality.
shapiro.test(data$depth_change)

# 2. Check homogeneity of variances
# Levene’s test
# Levene’s test is used to assess whether the variances of two or more populations are equal.
# https://www.datanovia.com/en/lessons/homogeneity-of-variance-test-in-r/
# When p > 0.05, there is no significant difference between the two variances.
leveneTest(depth_change ~ night_day, data = data)



# Paired t-test
# Assumptions not met

# Paired samples Wilcoxon test
wilcox.test(data$depth_change, data$night_day, paired = TRUE, alternative = "two.sided")


