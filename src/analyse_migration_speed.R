# Calculate and analyse horizontal migration speeds
# By Pieterjan Verhelst
# Pieterjan.Verhelst@UGent.be

# Packages
library(car)
library(lme4)
library(nlme)


# Load file with trajectory distances for all eels
source("./src/read_trajectory_data.R")


# Set columns in according formats
tr_data$ID <- as.factor(tr_data$ID)
tr_data$Date <- as.Date(tr_data$Date)
tr_data$Distance <- as.numeric(tr_data$Distance)


# Calculate total number of days and total distance
tr_summary <- tr_data %>%
  group_by(ID) %>%
  summarize(total_dist = sum(Distance),
            days = max(Date)-min(Date))

# Calculate total speed
tr_summary$days <- as.numeric(tr_summary$days)
tr_summary$total_speed <- tr_summary$total_dist / tr_summary$days

# Select eels that migrated >= 100 km
tr_summary <- filter(tr_summary, total_dist >= 100)
tr_summary$ID <- factor(tr_summary$ID)
unique(tr_summary$ID)


# Remove 2 eels
# 9355 does not have a net distance displacement of >= 100 km. It did have a total track of >= 100 km. Check with geolocation plots
# 17533 is unlikely an eel based on vertical movement pattern. Needs further checking
tr_summary <- filter(tr_summary, ID != 9355)
tr_summary <- filter(tr_summary, ID != 17533)
tr_summary$ID <- factor(tr_summary$ID)
unique(tr_summary$ID)

# Upload file with migration direction, eel sizes and release positions
direction <- read_csv("./data/external/migration_direction.csv")

# Merge direction to dataset
tr_summary <- merge(tr_summary, direction, by = "ID")

# Calculate avg, sd, min and max distance & tracking days per country of origin
total_distance_days <- tr_summary %>%
  group_by(Country) %>%
  summarize(avg_dist = mean(total_dist),
            sd_dist = sd(total_dist),
            min_dist = min(total_dist),
            max_dist = max(total_dist),
            avg_days = mean(days),
            sd_days = sd(days),
            min_days = min(days),
            max_days = max(days))



# Filter eels that migrated >= 100 km by merging tr_summary dataset to tr_data
tr_data <- merge(tr_data, tr_summary, by = "ID")
tr_data$ID <- factor(tr_data$ID)
unique(tr_data$ID)



# Calculate average, sd, min and max km per day per eel per country
#avg_km_day <- tr_data %>%
#  group_by(Country) %>%
#  summarize(avg_dist = mean(Distance),
#            sd = sd(Distance),
#            min = min(Distance),
#            max = max(Distance))



# 1. Analyse difference in size between countries  ####
tr_summary$Country <- factor(tr_summary$Country)
boxplot(tr_summary$Weight ~ tr_summary$Country)
#cor(tr_summary$speed, tr_summary$Weight)

# independent 2-group t-test
# t.test(y~x) # where y is numeric and x is a binary factor 
t.test(tr_summary$Weight~tr_summary$Country, var.equal = TRUE)

# Assumptions:
# 1. 2 independent groups
# N and SW are independent (each direction is attributed to a different animal)

# 2. Normality
qqnorm(tr_summary$Weight, pch = 1, frame = TRUE)
qqline(tr_summary$Weight, col = "steelblue", lwd = 2)

shapiro.test(tr_summary$Weight)
# The p-value > 0.05 implying that the distribution of the data are not significantly different from normal distribution. In other words, we can assume the normality.

# 3. Homogeneity of variances
# F-test 
# The F-test is used to assess whether the variances of two populations (A and B) are equal. The test requires normality (in case no normality, apply Fligner-Killeen’s test)
# https://www.datanovia.com/en/lessons/homogeneity-of-variance-test-in-r/
var.test(Weight ~ Country, data = tr_summary)
# When p > 0.05, there is no significant difference between the two variances.



# 2. Analyse difference in total speed between countries ####
# Calculate speed per group
aggregate(tr_summary$total_speed, list(tr_summary$Country), mean)
aggregate(tr_summary$total_speed, list(tr_summary$Country), sd)
aggregate(tr_summary$total_speed, list(tr_summary$Country), min)
aggregate(tr_summary$total_speed, list(tr_summary$Country), max)

# Create elaborated boxplot
# make a named list for the location of the number of eels
eel_per_group <- tr_summary %>% group_by(Country) %>% 
  summarise(n_eels = n_distinct(ID))
eels_per_group_list <- rep(50, nrow(eel_per_group))
names(eels_per_group_list) <- as.vector(eel_per_group$Country)
# create ggplot (cfr. styling earlier plot)
boxplot_country <- ggplot(tr_summary, aes(x = Country,
                                              y = total_speed)) +
  geom_boxplot(outlier.shape = NA) +
  #coord_flip() +
  #scale_y_continuous(breaks = seq(0, 600, by = 50)) +
  ylim(0,50) + 
  theme_minimal() +
  ylab("Speed (km/day)") +
  geom_text(data = data.frame(),
            aes(x = names(eels_per_group_list),
                y = eels_per_group_list,
                label = as.character(eel_per_group$n_eels)),
            col = 'black', size = 6) +
  #xlab("ALS position relative to shipping lock complex") +
  scale_x_discrete(limits=c("Germany",      # Changes oreder of plots
                            "Belgium")) +    
  theme(axis.title.y = element_text(margin = margin(r = 10))) +
  theme(axis.title.x = element_text(margin = margin(r = 10))) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        axis.title.x = element_blank()) 
boxplot_country

# independent 2-group t-test
# t.test(y~x) # where y is numeric and x is a binary factor 
t.test(tr_summary$total_speed~tr_summary$Country, var.equal = TRUE)

# Assumptions:
# 1. 2 independent groups
# N and SW are independent (each direction is attributed to a different animal)

# 2. Normality
qqnorm(tr_summary$total_speed, pch = 1, frame = TRUE)
qqline(tr_summary$total_speed, col = "steelblue", lwd = 2)

shapiro.test(tr_summary$total_speed)
# The p-value > 0.05 implying that the distribution of the data are not significantly different from normal distribution. In other words, we can assume the normality.

# 3. Homogeneity of variances
# F-test 
# The F-test is used to assess whether the variances of two populations (A and B) are equal. The test requires normality (in case no normality, apply Fligner-Killeen’s test)
# https://www.datanovia.com/en/lessons/homogeneity-of-variance-test-in-r/
var.test(total_speed ~ Country, data = tr_summary)
# When p > 0.05, there is no significant difference between the two variances.





# 3. Analyse difference in total speed between countries and directions ####
tr_summary$Direction_Country <- paste(tr_summary$Direction, tr_summary$Country)
tr_summary$Direction_Country <- factor(tr_summary$Direction_Country)
boxplot(total_speed ~ Direction_Country, data = tr_summary)

# Calculate speed per group
aggregate(tr_summary$total_speed, list(tr_summary$Direction_Country), mean)
aggregate(tr_summary$total_speed, list(tr_summary$Direction_Country), sd)
aggregate(tr_summary$total_speed, list(tr_summary$Direction_Country), min)
aggregate(tr_summary$total_speed, list(tr_summary$Direction_Country), max)

# Create elaborated boxplot
# make a named list for the location of the number of eels
eel_per_group <- tr_summary %>% group_by(Direction_Country) %>% 
  summarise(n_eels = n_distinct(ID))
eels_per_group_list <- rep(50, nrow(eel_per_group))
names(eels_per_group_list) <- as.vector(eel_per_group$Direction_Country)
# create ggplot (cfr. styling earlier plot)
boxplot_dir_country <- ggplot(tr_summary, aes(x = Direction_Country,
                                              y = total_speed)) +
  geom_boxplot(outlier.shape = NA) +
  #coord_flip() +
  #scale_y_continuous(breaks = seq(0, 600, by = 50)) +
  ylim(0,50) + 
  theme_minimal() +
  ylab("Speed (km/day)") +
  geom_text(data = data.frame(),
            aes(x = names(eels_per_group_list),
                y = eels_per_group_list,
                label = as.character(eel_per_group$n_eels)),
            col = 'black', size = 6) +
  #xlab("ALS position relative to shipping lock complex") +
  scale_x_discrete(limits=c("N Germany",      # Changes oreder of plots
                            "SW Germany",
                            "N Belgium",
                            "SW Belgium")) +    
  theme(axis.title.y = element_text(margin = margin(r = 10))) +
  theme(axis.title.x = element_text(margin = margin(r = 10))) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        axis.title.x = element_blank()) 
boxplot_dir_country



# One-way anova
# The one-way anova is an extension of independent two-samples t-test for comparing means in a situation where there are more than two groups (= grouping variable with more than 2 levels)
# http://www.sthda.com/english/wiki/one-way-anova-test-in-r
model_aov <- aov(total_speed ~ Direction_Country, data = tr_summary)
summary(model_aov)
TukeyHSD(model_aov)

# Assumptions
# 1. Homogeneity of variances
plot(model_aov, 1)
leveneTest(total_speed ~ Direction_Country, data = tr_summary)
# When p > 0.05, there is no significant difference between the two variances => assumption met

# 2. Normality
plot(model_aov, 2)

# Extract the residuals
aov_residuals <- residuals(object = model_aov)
# Run Shapiro-Wilk test
shapiro.test(x = aov_residuals)
# When p > 0.05, the assumption of normality is met





# 4. Analyse difference in daily distance (= speed per day) between countries and directions ####
tr_data$Direction_Country <- paste(tr_data$Direction, tr_data$Country)
tr_data$Direction_Country <- factor(tr_data$Direction_Country)
boxplot(Distance ~ Direction_Country, data = tr_data)

par(mfrow=c(2,1))
boxplot(total_speed ~ Direction_Country, data = tr_summary)
boxplot(Distance ~ Direction_Country, data = tr_data)

model_aov <- aov(Distance ~ Direction_Country, data = tr_data)
summary(model_aov)
TukeyHSD(model_aov)

# Assumptions
# 1. Homogeneity of variances
plot(model_aov, 1)
leveneTest(Distance ~ Direction_Country, data = tr_data)
# When p > 0.05, there is no significant difference between the two variances => assumption met

# 2. Normality
plot(model_aov, 2)

# Extract the residuals
aov_residuals <- residuals(object = model_aov)
# Run Shapiro-Wilk test
shapiro.test(x = aov_residuals)
# When p > 0.05, the assumption of normality is met




# 5. Analyse difference in daily distance (= speed per day) according to longitude ####
tr_data$Lon <- as.numeric(tr_data$Lon)
tr_data$Lat <- as.numeric(tr_data$Lat)


swb <- filter(tr_data, Direction_Country == "SW Belgium")
nb <- filter(tr_data, Direction_Country == "N Belgium")
swg <- filter(tr_data, Direction_Country == "SW Germany")
ng <- filter(tr_data, Direction_Country == "N Germany")

b <- filter(tr_data, Country == "Belgium")
g <- filter(tr_data, Country == "Germany")



plot <- ggplot(tr_data, aes(x=Lon, y=Distance)) +
  geom_line(alpha = 0.5) +
  geom_smooth(method = "loess", se = FALSE)
plot


#tr_data$Lon <- as.numeric(tr_data$Lon)
# GLM
glm_model <- glm(Distance ~  Lon, family = gaussian(link = "identity"), data = tr_data)

summary(glm_model)
vif(glm_model)
par(mfrow=c(2,2))
plot(glm_model)
dev.off()

# GLMM
# 1. lmer() from lme4 package
# Allows to calculate variance explained by random effects
mixed.lmer <- lmer(Distance ~ Lon + Lat + Country + (1|ID), data = tr_data)
summary(mixed.lmer)
# variance explained by the random effect 'ID' (= individual variability)
46.17/(46.17+79.53)

# 2. lme() from nlme package
# Gives p-values
mixed <- lme(Distance ~ Lon + Lat + Country, random = ~1|ID, data = tr_data)
summary(mixed)
anova(mixed)


# Check assumptions
# 1. Homogeneity of variance
plot(mixed.lmer)  # Make sure there are no patterns

# 2. Normality
qqnorm(resid(mixed.lmer))
qqline(resid(mixed.lmer))


# Plot predicted over response variable with loess smoother
tr_data$predicted <- predict(mixed, type="response")
tr_data$residuals <- residuals(mixed, type="response")
tr_data$fitted <- fitted(mixed, type="response")

plot <- ggplot(tr_data, aes(x=Lon, y=Distance)) +
  geom_line(alpha = 0.5) +
  geom_line(tr_data, mapping = aes(x=Lon, y=predicted), colour = "red") +
  geom_smooth(method = "loess", se = FALSE)
plot











# OLD CODE ####

# Calculate number of days and total distance
tr_summary <- tr_data %>%
  group_by(ID) %>%
  summarize(total_dist = sum(Distance),
            days = max(Date)-min(Date))

tr_summary <- merge(tr_summary, avg_km_day, by = "ID")
summary(tr_summary$total_dist)
sd(tr_summary$total_dist)

tr_summary$days <- as.numeric(tr_summary$days)
summary(tr_summary$days)
sd(tr_summary$days)


# Calculate migration speed (km/day)
tr_summary$speed <- tr_summary$total_dist/tr_summary$days
summary(tr_summary$speed)
sd(tr_summary$speed)

# Upload file with migration direction
direction <- read_csv("./data/external/migration_direction.csv")

# Merge direction to dataset
tr_summary <- merge(tr_summary, direction, by = "ID")


#write.csv(tr_summary, "./data/interim/summary_tracks.csv")


# Data exploration

# 1. Speed per direction
boxplot(speed ~ Direction, data = tr_summary)
tr_summary %>%
  group_by(Direction) %>%
  summarize(mean = mean(speed))

# 2. Speed per country
boxplot(speed ~ Country, data = tr_summary)
tr_summary %>%
  group_by(Country) %>%
  summarize(mean = mean(speed))

# 3. Speed per direction and country
tr_summary$Direction_Country <- paste(tr_summary$Direction, tr_summary$Country)
tr_summary$Direction_Country <- factor(tr_summary$Direction_Country)
boxplot(speed ~ Direction_Country, data = tr_summary)

# 4. Relationship between speed and size
# Without eel A09355 that did not show a clear migration direction
tr_summary2 <- filter(tr_summary, ID != "9355")
plot(speed ~ Length, data = tr_summary2)
plot(speed ~ Weight, data = tr_summary2)
cor(tr_summary2$speed, tr_summary2$Weight)

# Belgian eels
bel <- filter(tr_summary2, Country == "Belgium")
plot(speed ~ Weight, data = bel)
cor(bel$speed, bel$Weight)



# Data analysis

# Calculate speed per group
aggregate(tr_summary$speed, list(tr_summary$Direction_Country), mean)
aggregate(tr_summary$speed, list(tr_summary$Direction_Country), sd)
aggregate(tr_summary$speed, list(tr_summary$Direction_Country), min)
aggregate(tr_summary$speed, list(tr_summary$Direction_Country), max)

# Correlation between speed and size
plot(tr_summary$speed ~ tr_summary$Weight)
cor(tr_summary$speed, tr_summary$Weight)




# Create boxplot
boxplot(speed ~ Direction_Country, data = tr_summary)

# make a named list for the location of the number of eels
eel_per_group <- tr_summary %>% group_by(Direction_Country) %>% 
  summarise(n_eels = n_distinct(ID))
eels_per_group_list <- rep(50, nrow(eel_per_group))
names(eels_per_group_list) <- as.vector(eel_per_group$Direction_Country)
# create ggplot (cfr. styling earlier plot)
boxplot_dir_country <- ggplot(tr_summary, aes(x = Direction_Country,
                                     y = speed)) +
  geom_boxplot(outlier.shape = NA) +
  #coord_flip() +
  #scale_y_continuous(breaks = seq(0, 600, by = 50)) +
  ylim(0,50) + 
  theme_minimal() +
  ylab("Speed (km/day)") +
  geom_text(data = data.frame(),
            aes(x = names(eels_per_group_list),
                y = eels_per_group_list,
                label = as.character(eel_per_group$n_eels)),
            col = 'black', size = 6) +
  #xlab("ALS position relative to shipping lock complex") +
  scale_x_discrete(limits=c("N Germany",      # Changes oreder of plots
                            "SW Germany",
                            "N Belgium",
                            "SW Belgium")) +    
  theme(axis.title.y = element_text(margin = margin(r = 10))) +
  theme(axis.title.x = element_text(margin = margin(r = 10))) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        axis.title.x = element_blank()) 
boxplot_dir_country



# One-way anova
# The one-way anova is an extension of independent two-samples t-test for comparing means in a situation where there are more than two groups (= grouping variable with more than 2 levels)
# http://www.sthda.com/english/wiki/one-way-anova-test-in-r
model_aov <- aov(speed ~ Direction_Country, data = tr_summary)
summary(model_aov)
TukeyHSD(model_aov)


# 1. Homogeneity of variances
plot(model_aov, 1)
leveneTest(speed ~ Direction_Country, data = tr_summary)
# When p > 0.05, there is no significant difference between the two variances => assumption met

# 2. Normality
plot(model_aov, 2)

# Extract the residuals
aov_residuals <- residuals(object = model_aov)
# Run Shapiro-Wilk test
shapiro.test(x = aov_residuals)
# When p > 0.05, the assumption of normality is met




###################################################################

# independent 2-group t-test
# t.test(y~x) # where y is numeric and x is a binary factor 
t.test(tr_summary2$speed~tr_summary2$Direction_Country)

# Assumptions:
# 1. 2 independent groups
# N and SW are independent (each direction is attributed to a different animal)

# 2. Normality
qqnorm(tr_summary2$speed, pch = 1, frame = TRUE)
qqline(tr_summary2$speed, col = "steelblue", lwd = 2)

shapiro.test(tr_summary2$speed)
# The p-value > 0.05 implying that the distribution of the data are not significantly different from normal distribution. In other words, we can assume the normality.

# 3. Homogeneity of variances
# F-test 
# The F-test is used to assess whether the variances of two populations (A and B) are equal. The test requires normality (in case no normality, apply Fligner-Killeen’s test)
# https://www.datanovia.com/en/lessons/homogeneity-of-variance-test-in-r/
var.test(speed ~ Direction, data = tr_summary2)
# When p > 0.05, there is no significant difference between the two variances.




