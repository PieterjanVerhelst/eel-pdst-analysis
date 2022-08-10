# Analyse depth pattern: depth difference in relation to circadian and current phases
# By Pieterjan Verhelst
# pieterjan.verhelst@inbo.be


# Load packages
library(tidyverse) # To do datascience
library(lubridate)


# Load data
data <- read_csv("./data/interim/data_depth_diff.csv")

data$...1 <- NULL
data$ID <- factor(data$ID)
data$night_day <- factor(data$night_day)

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

# Remove NA in depth_change (= first row of each animal)
data <- data[!is.na(data$depth_change),]

# Remove NA in circadian phase
data <- data[!is.na(data$night_day),]

# Remove NA in current_phase_x and current_phase_y
data <- data[!is.na(data$current_phase_x),]
# Remove NA in circadian phase
data <- data[!is.na(data$current_phase_y),]

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




# Processing steps
summary(data$depth_change) # all values need to be > 0


## Check correlation
#data_no_na <- data %>% drop_na(direction_x)
#data_no_na <- data_no_na %>% drop_na(direction_y)
#cor(data_no_na$direction_x, data_no_na$direction_y)

## Add tracking day number
#data_summary$Date <- ymd(data_summary$date_hour)
#data_summary$Date <- as.Date(data_summary$date_hour)
data <- data %>% 
  #mutate(day_number = lubridate::ymd(Date)) %>% 
  group_by(ID) %>% 
  mutate(day_ordernumber = Date - first(Date))
data$day_ordernumber <- as.numeric(data$day_ordernumber) + 1



# Check data distribution
# Create qqplot with qqline
qqnorm(data$depth_change)
qqline(data$depth_change)



### GLMM from MASS
glm_model <- MASS::glmmPQL(depth_change ~  night_day + current_phase_x + current_phase_y,
                           random = ~1|ID,
                           correlation = corAR1(form = ~ 1 | ID),
                           family = Gamma(link = "log"),
                           data = data, na.action = na.omit)

glm_model2 <- MASS::glmmPQL(depth_change ~  night_day + current_phase_x + current_phase_y +
                              night_day:current_phase_x +
                              night_day:current_phase_y,
                            random = ~1|ID/Date,
                            correlation = corAR1(form = ~ 1|ID/Date),
                            family = Gamma(link = "log"),
                            data = data, na.action = na.omit)

glm_model3 <- MASS::glmmPQL(depth_change ~  night_day + current_phase_x + current_phase_y,
                            random = ~1|ID/Date,
                            correlation = corAR1(form = ~ 1|ID/Date),
                            family = gaussian,
                            data = data, na.action = na.omit)

# Best model with easiest interpretration
# Note it is not that different from glm_model2 (Gamma log link), but easier to interpret
glm_model4 <- MASS::glmmPQL(sqrt(depth_change) ~  night_day + current_phase_x + current_phase_y +
                              night_day:current_phase_x +
                              night_day:current_phase_y,
                            random = ~1|ID/Date,
                            correlation = corAR1(form = ~ 1|ID/Date),
                            family = gaussian,
                            data = data, na.action = na.omit)

summary(glm_model4)

# Check model
plot(glm_model4)
par(mfrow=c(2,2))
qqnorm(resid(glm_model4, type = "n"))  # type = "n"   means that the normalised residues are used; these take into account autocorrelation
hist(resid(glm_model4, type = "n"))
plot(fitted(glm_model4),resid(glm_model4, type = "n"))
dev.off()

coefplot2(glm_model4)




