# Analyse depth pattern: distance from seabed in relation to circadian and current phases
# By Pieterjan Verhelst
# pieterjan.verhelst@inbo.be


# Load packages
library(tidyverse) # To do datascience
library(lubridate)
library(PairedData)
library(car)
library(MASS)
library(mgcv)
library(lme4)


# 1. Read data ####
data <- read.csv("./data/interim/data_current_phases.csv")
data$ID <- factor(data$ID)
data$datetime <- ymd_hms(data$datetime)
data$night_day <- factor(data$night_day)
data$current_phase_x <- factor(data$current_phase_x)
data$current_phase_y <- factor(data$current_phase_y)

# Remove DVM data from eel A17535
data <- data[!(data$ID == "17535" & data$datetime >= '2020-01-11 00:00:00'),]

# Nordic eels
data <- filter(data, ID == "15805" |
                 ID == "15981" |
                 ID == "17492_2" |
                 ID == "17499" |
                 ID == "17525_2")

# Channel eels
data <- filter(data, ID != "15805" ,
               ID != "15981" ,
               ID != "17492_2" ,
               ID != "17499" ,
               ID != "17525_2")

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
#data <- data[!is.na(data$night_day),]


# Calculate summary
## Circadian phases
aggregate(data$dist_from_seabed, list(data$night_day), mean, na.rm = TRUE)
aggregate(data$dist_from_seabed, list(data$night_day), sd, na.rm = TRUE)
aggregate(data$dist_from_seabed, list(data$night_day), median, na.rm = TRUE)
aggregate(data$dist_from_seabed, list(data$night_day), min, na.rm = TRUE)
aggregate(data$dist_from_seabed, list(data$night_day), max, na.rm = TRUE)
aggregate(data$dist_from_seabed, list(data$night_day, data$ID), mean, na.rm = TRUE) # per eel

## Current phases
aggregate(data$dist_from_seabed, list(data$current_phase_x), mean, na.rm = TRUE)
aggregate(data$dist_from_seabed, list(data$current_phase_x), sd, na.rm = TRUE)
aggregate(data$dist_from_seabed, list(data$current_phase_x), median, na.rm = TRUE)
aggregate(data$dist_from_seabed, list(data$current_phase_x), min, na.rm = TRUE)
aggregate(data$dist_from_seabed, list(data$current_phase_x), max, na.rm = TRUE)
aggregate(data$dist_from_seabed, list(data$current_phase_x, data$ID), mean, na.rm = TRUE) # per eel


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


# Paired plot
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
leveneTest(dist_from_seabed ~ night_day, data = data)


# Paired t-test
# Assumptions not met

# Paired samples Wilcoxon test
# See: http://www.sthda.com/english/wiki/paired-samples-wilcoxon-test-in-r
wilcox.test(day, night, paired = TRUE)


# GLMM
subset <- filter(data, ID == "16031" |
                   ID == "17508" |
                   ID == "17538" |
                   ID == "15789" |
                   ID == "17648")
subset <- filter(subset, dist_from_seabed > 0)
summary(subset$dist_from_seabed)

data_no_neg <- filter(data, dist_from_seabed > 0)

### Processing steps
summary(data$dist_from_seabed) # all values need to be > 0

# set 0 to 0.00001
data$dist_from_seabed <- if_else(data$dist_from_seabed == 0,
                      0.00001,
                      data$dist_from_seabed)

# Check correlation
data_no_na <- data %>% drop_na(direction_x)
data_no_na <- data_no_na %>% drop_na(direction_y)
cor(data_no_na$direction_x, data_no_na$direction_y)


## GLMM from MASS
glm_model <- MASS::glmmPQL(dist_from_seabed ~  night_day + tidal_phase + night_day:tidal_phase,
                           random=~1|ID,
                           family = Gamma(link = "inverse"),
                           data = subset, na.action = na.omit)
summary(glm_model)

## bam 
bam_model <- bam(dist_from_seabed ~  night_day + tidal_phase + night_day:tidal_phase +
                   s(ID, bs="re"),
                 family = Gamma(link = "log"), data = subset, discrete = TRUE,
                 rho=0.99,
                 na.action = na.omit)
summary(bam_model)

## GLM from glmer
mod_glmer <- glmer(dist_from_seabed ~  night_day + current_phase_x + current_phase_y + 
                    (1|ID), 
                    data=data_no_neg,
                    family=Gamma(link = "log"))
summary(mod_glmer)

plot(mod_glmer)

# Check model
par(mfrow=c(2,2))
qqnorm(resid(mod_glmer))
hist(resid(mod_glmer))
plot(fitted(mod_glmer),resid(mod_glmer))

# Check overdispersion
library("blmeco") 
dispersion_glmer(mod_glmer) #it shouldn't be over 1.4


