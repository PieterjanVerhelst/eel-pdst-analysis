# Calculate and analyse horizontal migration speeds
# By Pieterjan Verhelst
# Pieterjan.Verhelst@UGent.be

# Packages
library(car)


# Load file with trajectory distances for all eels
source("./src/read_trajectory_data.R")


# Set columns in according formats
tr_data$ID <- as.factor(tr_data$ID)
tr_data$Date <- as.Date(tr_data$Date)
tr_data$Distance <- as.numeric(tr_data$Distance)

# Filter eels with >= 14 days out at large
tr_data <- filter(tr_data, ID == '9349' |
                        ID == '9358' |
                        ID == '9359' |
                        ID == '9374' |
                        ID == '9377' |
                        ID == '9393' |
                        ID == '9411' |
                        ID == '9423' |
                        ID == '9424' |
                        ID == '15714' |
                        ID == '16031' |
                        ID == '15706' |
                        ID == '15805' |
                        ID == '15981' |
                        ID == '15777' |
                        ID == '17443' |
                        ID == '17499' |
                        ID == '17513' |
                        ID == '17534' |
                        ID == '17526' |
                        ID == '17522' |
                        ID == '17492' |
                        ID == '17508' |
                        ID == '17536' |
                        ID == '17537' |
                        ID == '17538' |
                        ID == '17510' |
                        ID == '15789' |
                        ID == '112061' |
                        ID == '112064')


# Calculate average, sd, min and max km per day per eel and for all eels
avg_km_day <- tr_data %>%
  group_by(ID) %>%
  summarize(avg_dist = mean(Distance),
            sd = sd(Distance),
            min = mean(Distance),
            max = max(Distance))

summary(tr_data$Distance)
sd(tr_data$Distance)


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
model_aov <- aov(speed ~ Direction_Country, data = tr_summary2)
summary(model_aov)
TukeyHSD(model_aov)


# 1. Homogeneity of variances
plot(model_aov, 1)
leveneTest(speed ~ Direction_Country, data = tr_summary2)
# When p > 0.05, there is no significant difference between the two variances => assumption met

# 2. Normality
plot(model_aov, 2)

# Extract the residuals
aov_residuals <- residuals(object = model_aov )
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




