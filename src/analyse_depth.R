# Analyse depth pattern: distance from seabed in relation to circadian and current phases
# By Pieterjan Verhelst
# pieterjan.verhelst@inbo.be


# Load packages
library(tidyverse) # To do datascience
library(lubridate)
library(MASS)
library(nlme)    # For the corAR1() functionality
#library(mgcv)
#library(lme4)
#library("blmeco") # To calculate overdispersion of GLMM
library(coefplot2)
library(ggeffects)
library(pracma)  # For the 'deg2rad()' function
library(car)


# 1. Read data ####
data <- read.csv("./data/interim/data_current_phases.csv")
data$ID <- factor(data$ID)
data$datetime <- ymd_hms(data$datetime)
data$night_day <- factor(data$night_day)
data$current_phase_x <- factor(data$current_phase_x)
data$current_phase_y <- factor(data$current_phase_y)
data$current_phase_p <- factor(data$current_phase_p)

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

mean(data_max_depth$max_depth)
sd(data_max_depth$max_depth)
min(data_max_depth$max_depth)
max(data_max_depth$max_depth)

# Calculate distance from seabed
data$dist_from_seabed <- data$corrected_depth - data$max_depth


# Remove NA in circadian phase
data <- data[!is.na(data$night_day),]

# Remove NA in current_phase_x and current_phase_y
data <- data[!is.na(data$current_phase_x),]
# Remove NA in circadian phase
data <- data[!is.na(data$current_phase_y),]


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


# Summarise data per hour
#data$date_hour <- floor(data$datetime/3600)
data$date_hour <- lubridate::floor_date(data$datetime, "hour")  
data_summary <- data %>%
  group_by(ID, date_hour, night_day, current_phase_p, current_phase_y) %>%
  summarise(mean_depth = mean(corrected_depth),
            mean_rel_depth = mean(rel_depth),
            mean_seabed = mean(dist_from_seabed),
            mean_temp = mean(temperature),
            mean_moon = mean(moon_fraction),
            mean_sun_altitude = mean(sun_altitude),
            mean_sun_azimuth = mean(sun_azimuth),
            mean_direction_x = mean(direction_x),
            mean_direction_y = mean(direction_y))
  



### For model with mean distance from seabed as response variable ####
# Processing steps
summary(data_summary$mean_seabed) # all values need to be > 0

## set 0 to 0.00001 to apply Gamma distribution
data_summary$mean_seabed <- if_else(data_summary$mean_seabed == 0,
                      0.00001,
                      data_summary$mean_seabed)

### For model with relative depth from seabed as response variable ####
# Remove mean_rel_depth < 0
# This is due to depths above sea surface
plot(data_summary$mean_rel_depth)
data_summary <- filter(data_summary, mean_rel_depth > 0)

# Remove mean_rel_depth values larger than 1
data_summary <- filter(data_summary, mean_rel_depth < 1)


mean(data_summary$mean_depth)
sd(data_summary$mean_depth)
min(data_summary$mean_depth)
max(data_summary$mean_depth)

mean(data_summary$mean_rel_depth)
sd(data_summary$mean_rel_depth)
min(data_summary$mean_rel_depth)
max(data_summary$mean_rel_depth)


## Check correlation
#data_no_na <- data %>% drop_na(direction_x)
#data_no_na <- data_no_na %>% drop_na(direction_y)
#cor(data_no_na$direction_x, data_no_na$direction_y)

## Add tracking day number
#data_summary$Date <- ymd(data_summary$date_hour)
data_summary$Date <- as.Date(data_summary$date_hour)
data_summary <- data_summary %>% 
  #mutate(day_number = lubridate::ymd(Date)) %>% 
  group_by(ID) %>% 
  mutate(day_ordernumber = Date - first(Date))
data_summary$day_ordernumber <- as.numeric(data_summary$day_ordernumber) + 1



# Check data distribution
# Create qqplot with qqline
qqnorm(data_summary$mean_seabed)
qqline(data_summary$mean_seabed)

qqnorm(data_summary$mean_rel_depth)
qqline(data_summary$mean_rel_depth)


### GLMM from MASS
glm_model <- MASS::glmmPQL(mean_seabed ~  night_day + current_phase_x + current_phase_y,
                           random = ~1|ID,
                           correlation = corAR1(form = ~ 1 | ID),
                           family = Gamma(link = "log"),
                           data = data_summary, na.action = na.omit)

glm_model2 <- MASS::glmmPQL(mean_seabed ~  night_day + current_phase_x + current_phase_y +
                              night_day:current_phase_x +
                              night_day:current_phase_y,
                            random = ~1|ID/Date,
                            correlation = corAR1(form = ~ 1|ID/Date),
                            family = Gamma(link = "log"),
                            data = data_summary, na.action = na.omit)

glm_model3 <- MASS::glmmPQL(mean_seabed ~  night_day + current_phase_x + current_phase_y,
                            random = ~1|ID/Date,
                            correlation = corAR1(form = ~ 1|ID/Date),
                            family = gaussian,
                            data = data_summary, na.action = na.omit)

# Best model with easiest interpretration
# Note it is not that different from glm_model2 (Gamma log link), but easier to interpret
glm_model4 <- MASS::glmmPQL(sqrt(mean_seabed) ~  night_day + current_phase_x + current_phase_y +
                              night_day:current_phase_x +
                              night_day:current_phase_y,
                            random = ~1|ID/Date,
                            correlation = corAR1(form = ~ 1|ID/Date),
                            family = gaussian,
                            data = data_summary, na.action = na.omit)



# Since the Gaussian model is chosen, we can also work with lme() from the nlme package
# Info by Pieter Verschelde (INBO): vierkantwortel zorgt voor een betere benadering van een normale distributie, maar het is zeker nog geen normale distributie, niettemin lijkt een gamma distributie geen betere benadering. Omdat de distributie niet perfect normaal is moet wel opgelet worden met interpreteren van p-waarden die dicht bij 00.05 zijn zoals bv de 0.022 van de interactie, eigenlijk is dit geen sterk effect en kan het gewoon significant zijn door de fout in responsdistributie

# For Channel eels: test on current_phase_p
# For Nordic eels: test on current_phase_y

# Change name of factor levels for current_phase_p
data_summary$current_phase_p <- recode_factor(data_summary$current_phase_p, "favourable" = "west southwest", 
                                              "non-favourable" = "east northeast")

# Set order of levels for current phase, so the favourable current is in the model output
data_summary$current_phase_p <- factor(data_summary$current_phase_p, levels = c("east northeast", "west southwest"))
data_summary$current_phase_y <- factor(data_summary$current_phase_y, levels = c("southward", "northward"))


glm_model_channel <- lme(mean_rel_depth ~  night_day + current_phase_p + 
                    night_day:current_phase_p,
                    random = ~1|ID/Date,
                    correlation = corAR1(form = ~ 1|ID/Date),
                    data = data_summary, na.action = na.omit)


glm_model_nordic <- lme(mean_rel_depth ~  night_day + current_phase_y + 
                           night_day:current_phase_y,
                         random = ~1|ID/Date,
                         correlation = corAR1(form = ~ 1|ID/Date),
                         data = data_summary, na.action = na.omit)


# Stepwise backward selection
glm_model_channel <- lme(mean_rel_depth ~  night_day + current_phase_p +
                   
                    night_day:current_phase_p,
                  random = ~1|ID/Date,
                  correlation = corAR1(form = ~ 1|ID/Date),
                  data = data_summary, na.action = na.omit)



glm_model5 <- glm_model_channel
summary(glm_model5)


# Check model
plot(glm_model5)
par(mfrow=c(2,2))
qqnorm(resid(glm_model5, type = "n"))  # type = "n"   means that the normalised residues are used; these take into account autocorrelation
hist(resid(glm_model5, type = "n"))
plot(fitted(glm_model5),resid(glm_model5, type = "n"))
dev.off()

coefplot2(glm_model5)

# In case mean water temperature was added, check correlation with the factor covariables via one-way anova
res.aov <- aov(mean_temp ~ Date, data = data_summary)
summary(res.aov)

# Check assumptions
leveneTest(data_summary$mean_temp ~ data_summary$Date)
par(mfrow=c(2,2))
plot(res.aov)
dev.off

# Non-parametric test
kruskal.test(data_summary$mean_temp ~ data_summary$Date)



#newdata
newdata <- expand.grid(night_day = c("night", "day"), current_phase_x = c("eastward", "westward"), current_phase_y = c("northward", "southward"))
newdata$pred_sqrt <- predict(glm_model5, newdata = newdata, level = 0)

newdata <- expand.grid(night_day = c("night", "day"), current_phase_p = c("west southwest", "east northeast"))
newdata$pred_sqrt <- predict(glm_model5, newdata = newdata, level = 0)

newdata <- expand.grid(night_day = c("night", "day"), current_phase_y = c("northward", "southward"))
newdata$pred_sqrt <- predict(glm_model5, newdata = newdata, level = 0)

#confidence bounds
#code gehaald van https://rdrr.io/github/bsurial/bernr/src/R/bolker_ci.R (is een klassieke methode die veel gebruikt worden. Referentie kan Zuur et al. zijn
bolker_ci <- function(model, newdat, pred_int = FALSE, conf_level = 0.95) {
  if(class(model) != "lme") {
    stop("works for lme-models only")
  }
  z <- round(qnorm((1-conf_level)/2, lower.tail = FALSE), 2)
  newdat$pred <- predict(model, newdat, level = 0)
  Designmat <- model.matrix(formula(model)[-2], newdat)
  predvar <- diag(Designmat %*% vcov(model) %*% t(Designmat))
  newdat$se <- sqrt(predvar)
  newdat$ci_l <- newdat$pred - z*newdat$se
  newdat$ci_h <- newdat$pred + z*newdat$se
  if(pred_int == TRUE) {
    newdat$se2 <- sqrt(predvar+model$sigma^2)
    newdat$predint_l <- newdat$pred - z*newdat$se2
    newdat$predint_h <- newdat$pred + z*newdat$se2
  }
  newdat
}


# Calculate confidence intervals
conf_bounds <- bolker_ci(glm_model5, newdata, pred_int = FALSE) %>%
  mutate(predictie = pred,
         lcl = ci_l,
         ucl = ci_h,
         combi = interaction(night_day, current_phase_x, current_phase_y))

# For Channel eels
conf_bounds <- bolker_ci(glm_model5, newdata, pred_int = FALSE) %>%
  mutate(predictie = pred,
         lcl = ci_l,
         ucl = ci_h,
         combi = interaction(night_day, current_phase_p))

# For Nordic eels
conf_bounds <- bolker_ci(glm_model5, newdata, pred_int = FALSE) %>%
  mutate(predictie = pred,
         lcl = ci_l,
         ucl = ci_h,
         combi = interaction(night_day, current_phase_y))

# Confidence intervals in case response variable was squared
#conf_bounds <- bolker_ci(glm_model5, newdata, pred_int = FALSE) %>%
#  mutate(predictie = pred^2,
#         lcl = ci_l^2,
#         ucl = ci_h^2,
#         combi = interaction(night_day, current_phase_x, current_phase_y))

# Plot confidence intervals
ggplot(conf_bounds, aes(x = combi, y = predictie, ymin = lcl, ymax = ucl)) +
  xlab("Scenario") + ylab("Relative depth") +
  geom_point() + geom_errorbar() + theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(axis.text = element_text(size = 22),
        axis.title = element_text(size = 24))

#alternatief die alles voor je doet (dit is wel nog in de vierkantwortelschaal)
effects <- ggeffects::ggpredict(glm_model5)
effects
plot(effects)

# Check model
plot(glm_model5) #niet genormaliseerde residu"s


newdata2 <- data_summary %>%
  mutate(normres = resid(glm_model5, type = "n"),
         fitted = fitted(glm_model5),
         fitted0 = fitted(glm_model5, level = 0))
ggplot(newdata2, aes(x = night_day, y = normres)) + geom_boxplot()
ggplot(newdata2, aes(x = current_phase_x, y = normres)) + geom_boxplot()
ggplot(newdata2, aes(x = current_phase_y, y = normres)) + geom_boxplot()
#niet normaal verdeeld, maar wel symmetrisch en heel smalle schouders
ggplot(newdata2, aes(sample = normres)) + geom_qq() + geom_qq_line()
ggplot(newdata2, aes(x = normres)) + geom_histogram()
#eigenlijk zou hier geen trend in mogen zitten, maar het zijn vooral enkele extremen bij hoge fits die de lijn hier omhoog trekken
ggplot(newdata2, aes(x = fitted, y = normres)) + geom_point() + geom_smooth()
ggplot(newdata2, aes(x = fitted, y = sqrt_mean_seabed)) + geom_point() + geom_smooth() +
  geom_abline(intercept = 0, slope = 1, color = "red")


