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

# Since the Gaussian model is chosen, we can also work with lme() from the nlme package
# Info by Pieter Verschelde (INBO): vierkantwortel zorgt voor een betere benadering van een normale distributie, maar het is zeker nog geen normale distributie, niettemin lijkt een gamma distributie geen betere benadering. Omdat de distributie niet perfect normaal is moet wel opgelet worden met interpreteren van p-waarden die dicht bij 00.05 zijn zoals bv de 0.022 van de interactie, eigenlijk is dit geen sterk effect en kan het gewoon significant zijn door de fout in responsdistributie
glm_model5 <- lme(sqrt(depth_change) ~  night_day + current_phase_x + current_phase_y +
                    night_day:current_phase_x +
                    night_day:current_phase_y,
                  random = ~1|ID/Date,
                  correlation = corAR1(form = ~ 1|ID/Date),
                  data = data, na.action = na.omit)


summary(glm_model5)



# Check model
plot(glm_model5)
par(mfrow=c(2,2))
qqnorm(resid(glm_model5, type = "n"))  # type = "n"   means that the normalised residues are used; these take into account autocorrelation
hist(resid(glm_model5, type = "n"))
plot(fitted(glm_model5),resid(glm_model5, type = "n"))
dev.off()

coefplot2(glm_model4)


# In case mean water temperature was added, check correlation with the factor covariables via one-way anova
res.aov <- aov(temperature ~ Date, data = data)
summary(res.aov)

#newdata
newdata <- expand.grid(night_day = c("night", "day"), current_phase_x = c("eastward", "westward"), current_phase_y = c("northward", "southward"))
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

# Confidence intervals in case response variable was squared
#conf_bounds <- bolker_ci(glm_model5, newdata, pred_int = FALSE) %>%
#  mutate(predictie = pred^2,
#         lcl = ci_l^2,
#         ucl = ci_h^2,
#         combi = interaction(night_day, current_phase_x, current_phase_y))

# Plot confidence intervals
ggplot(conf_bounds, aes(x = combi, y = predictie, ymin = lcl, ymax = ucl)) +
  xlab("Scenario") + ylab("Depth difference (m)") +
  geom_point() + geom_errorbar() + theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 18))

