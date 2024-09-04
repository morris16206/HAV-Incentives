rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


library(readxl)
library(openxlsx)
library(plyr)
library(tidyverse)
library(geepack)
library(changepoint)


### Preprocessing
data = read_xlsx("UCSD Modeling_Project VIBE_Event and Gift Card Data_24.06.26.xlsx")
data$`Event ID` = as.numeric(data$`Event ID`)
data$`Number of gift cards distributed` = ifelse(data$`Number of gift cards distributed` == "N/A", NA, data$`Number of gift cards distributed`)
data$`Number of gift cards distributed` = as.numeric(data$`Number of gift cards distributed`)

data1 = data %>%
  filter(`Event Date` < "2023-02-21" | `Event Date` > "2023-11-30" | `Incentive Offered?` == "Yes") %>%
  drop_na(`Number refused vaccination for HAV`) %>%
  arrange(`Location ID`, `Event Date`) %>%
  mutate(Location_ID_unique = if_else(`Location ID` == "L000",
                                      1000+row_number(),
                                      as.numeric(str_sub(`Location ID`, -3, -1)))) %>%
  arrange(Location_ID_unique)

data1$`Vaccine uptake` = data1$`Hep A 1st doses administered` / (data1$`Hep A 1st doses administered` + data1$`Number refused vaccination for HAV`)
data1$`Vaccine eligible proportion` = (data1$`Hep A 1st doses administered` + data1$`Number refused vaccination for HAV`) /
  (data1$`Hep A 1st doses administered` + data1$`Number refused vaccination for HAV` + data1$`Number already fully vaccinated for HAV` + data1$`Number not yet due for 2nd dose for HAV`)

# Convert date to numerical and create spline variables
data1$daysbl = as.numeric(as.Date(data1$`Event Date`) - as.Date("2023-02-13"))
data1$daysbl1plus = with(data1, pmax(as.numeric(as.Date(data1$`Event Date`) - as.Date("2023-02-21")), 0))
data1$daysbl2plus = with(data1, pmax(as.numeric(as.Date(data1$`Event Date`) - as.Date("2023-11-30")), 0))


## ID for refused missingness
data2 = data %>%
  filter(`Event Date` < "2023-02-21" | `Event Date` > "2023-11-30" | `Incentive Offered?` == "Yes") %>%
  filter(is.na(`Number refused vaccination for HAV`)) %>%
  select(`Event ID`) %>%
  arrange(`Event ID`)
write.xlsx(data2, file = "ID for missing refused.xlsx")

## ID for first dose equal to zero
data3 = data %>%
  filter(`Event Date` < "2023-02-21" | `Event Date` > "2023-11-30" | `Incentive Offered?` == "Yes") %>%
  filter(`Hep A 1st doses administered` == 0 & `Number refused vaccination for HAV` == 0) %>%
  select(`Event ID`) %>%
  arrange(`Event ID`)
write.xlsx(data3, file = "ID for both first dose and refused equal to 0.xlsx")

## Weekly total
weekly_total = read_xlsx("UCSD Modeling_Project VIBE_24.05.14.xlsx")
weekly_total = weekly_total[-nrow(weekly_total), ]
weekly_total$total = rowSums(weekly_total[, -1])
weekly_total$`Disease Week End` = as.POSIXct(as.numeric(weekly_total$`Disease Week End`) * 86400, origin = "1899-12-30", tz = "UTC")


### Plot
data_loess = data1 %>%
  mutate(segment = cut(`Event Date`,
                       breaks = as.POSIXct(c(ymd_hms("2023-02-13 00:00:00", tz = "UTC"),
                                             ymd_hms("2023-02-21 00:00:00", tz = "UTC"),
                                             ymd_hms("2023-11-30 00:00:00", tz = "UTC"),
                                             ymd_hms("2024-03-26 00:00:00", tz = "UTC"))),
                       labels = c("Before", "During", "After"),
                       include.lowest = T))

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

## Boxplot
ggplot(data_loess, aes(x = factor(segment), y = 100 * `Vaccine uptake`)) +
  geom_boxplot(aes(color = factor(segment))) +
  scale_colour_manual(values = gg_color_hue(3)) +
  xlab("Intervention") +
  ylab("Vaccine uptake (%)") +
  theme_bw() +
  theme(title = element_text(size = 16),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        legend.position = "none")

## LOESS curve
# Vaccine uptake
ggplot(data1, aes(x = `Event Date`, y = 100 * `Vaccine uptake`)) +
  geom_vline(xintercept = as.POSIXct(c(ymd_hms("2023-02-21 00:00:00", tz = "UTC"),
                                       ymd_hms("2023-11-30 00:00:00", tz = "UTC"))), linetype = "dashed", color = "red") +
  geom_point(shape = 1) +
  geom_smooth(data = data_loess %>% filter(segment != "Before"),
              aes(color = factor(segment)),
              method = "loess",
              formula = y ~ x,
              span = 3) +
  ylab("Vaccine uptake (%)") +
  scale_color_manual(name = "Intervention",
                     values = gg_color_hue(3)[2:3]) +
  theme_bw() +
  theme(title = element_text(size = 16),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 16))

# Vaccine eligible proportion
ggplot(data1, aes(x = `Event Date`, y = 100 * `Vaccine eligible proportion`)) +
  geom_vline(xintercept = as.POSIXct(c(ymd_hms("2023-02-21 00:00:00", tz = "UTC"),
                                       ymd_hms("2023-11-30 00:00:00", tz = "UTC"))), linetype = "dashed", color = "red") +
  geom_point(shape = 1) +
  geom_smooth(data = data_loess %>% filter(segment != "Before"),
              aes(color = factor(segment)),
              method = "loess",
              formula = y ~ x,
              span = 3) +
  ylab("Vaccine eligible proportion (%)") +
  scale_color_manual(name = "Intervention",
                     values = gg_color_hue(3)[2:3]) +
  theme_bw() +
  theme(title = element_text(size = 16),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 16))

## Spaghetti plot
# Vaccine eligible proportion
ggplot(data1, aes(x = `Event Date`, y = 100 * `Vaccine eligible proportion`, group = Location_ID_unique)) +
  geom_vline(xintercept = as.POSIXct(c(ymd_hms("2023-02-21 00:00:00", tz = "UTC"),
                                       ymd_hms("2023-11-30 00:00:00", tz = "UTC"))), linetype = "dashed", color = "red") +
  geom_line(color = "gray") +
  geom_point(shape = 1) +
  ylab("Vaccine eligible proportion (%)") +
  theme_bw() +
  theme(title = element_text(size = 16),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 16))

ggplot(data1, aes(x = `Event Date`, y = 100 * `Vaccine uptake`, group = Location_ID_unique)) +
  geom_vline(xintercept = as.POSIXct(c(ymd_hms("2023-02-21 00:00:00", tz = "UTC"),
                                       ymd_hms("2023-11-30 00:00:00", tz = "UTC"))), linetype = "dashed", color = "red") +
  geom_line(color = "gray") +
  geom_point(shape = 1) +
  ylab("Vaccine eligible proportion (%)") +
  theme_bw() +
  theme(title = element_text(size = 16),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 16))


### Analysis
## Lagged correlation, vaccine uptake vs. number of cases
start_date = as.POSIXct(c(ymd_hms("2023-02-11 00:00:00", tz = "UTC")))
group_by_custom_start = function(date, start_date, interval = 7*24*60*60) {
  start_date + ((as.numeric(difftime(date, start_date, units = "secs")) %/% interval + 1) * interval)
}
data_ts = data1 %>%
  mutate(`Week End` = group_by_custom_start(`Event Date`, start_date)) %>%
  group_by(`Week End`) %>%
  summarise(mean_uptake = mean(`Vaccine uptake`, na.rm = T)) %>%
  full_join(weekly_total[, c("Disease Week End", "total")], by = join_by(`Week End` == `Disease Week End`)) %>%
  drop_na()

vaccine_uptake = ts(data_ts$mean_uptake)
number_cases = ts(data_ts$total)

fit.ccf = ccf(vaccine_uptake, number_cases, lag.max = 45)
data.frame(correlation = fit.ccf$acf, lag = fit.ccf$lag)
#lag = 21 (positive correlation) and -4 (negative correlation)

## GEE, vaccine uptake vs. media effects
data_media_loess = data1 %>%
  mutate(segment = cut(`Event Date`,
                       breaks = as.POSIXct(c(ymd_hms("2023-02-13 00:00:00", tz = "UTC"),
                                             ymd_hms("2023-05-16 00:00:00", tz = "UTC"),
                                             ymd_hms("2023-06-14 00:00:00", tz = "UTC"),
                                             ymd_hms("2024-03-26 00:00:00", tz = "UTC"))),
                       labels = c("Before", "After First", "After Second"),
                       include.lowest = T))

ggplot(data1, aes(x = `Event Date`, y = 100 * `Vaccine uptake`)) +
  geom_vline(xintercept = as.POSIXct(c(ymd_hms("2023-05-16 00:00:00", tz = "UTC"),
                                       ymd_hms("2023-06-14 00:00:00", tz = "UTC"))), linetype = "dashed", color = "purple") +
  geom_point(shape = 1) +
  geom_smooth(data = data_media_loess,
              aes(color = factor(segment)),
              method = "loess",
              formula = y ~ x,
              span = 3) +
  ylab("Vaccine uptake (%)") +
  scale_color_manual(name = "Media Communication",
                     values = gg_color_hue(3)) +
  theme_bw() +
  theme(title = element_text(size = 16),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 16))

data_pt = data1
data_pt$daysbl_media0 = with(data1, pmax(as.numeric(as.Date(data1$`Event Date`) - as.Date("2023-04-30")), 0))
data_pt$daysbl_media1 = with(data1, pmax(as.numeric(as.Date(data1$`Event Date`) - as.Date("2023-05-16")), 0))
data_pt$daysbl_media2 = with(data1, pmax(as.numeric(as.Date(data1$`Event Date`) - as.Date("2023-06-14")), 0))
data_pt$daysbl_media3 = with(data1, pmax(as.numeric(as.Date(data1$`Event Date`) - as.Date("2023-07-31")), 0))

fit.gee.media = geeglm(`Vaccine uptake` ~ daysbl + daysbl_media0 + daysbl_media1 + daysbl_media2 + daysbl_media3, id = Location_ID_unique, data = data_pt)
summary(fit.gee.media)

data_pt.vu = data_pt %>%
  filter(is.na(`Vaccine uptake`) == F)

data_pt_plot = data_pt.vu
data_pt_plot$predicted = c(predict(fit.gee.media))

ggplot(data_pt_plot, aes(x = `Event Date`)) +
  geom_point(aes(y = `Vaccine uptake`), size = 1) +  # Original data points
  geom_line(aes(y = predicted), color = "red", size = 1) +  # Fitted values
  geom_vline(xintercept = as.POSIXct(ymd_hms("2023-05-16 00:00:00", tz = "UTC")), linetype = "dashed", color = "purple") +  # Knot 1
  geom_vline(xintercept = as.POSIXct(ymd_hms("2023-06-14 00:00:00", tz = "UTC")), linetype = "dashed", color = "purple") +  # Knot 2
  labs(title = "Linear Spline with Fitted Values",
       x = "Event Date",
       y = "Vaccine uptake") +
  theme_bw()

fit.gee = geeglm(data_pt.vu$`Hep A 1st doses administered` ~ daysbl + daysbl_media0 + daysbl_media1 + daysbl_media2 + daysbl_media3, family = poisson, id = Location_ID_unique, data = data_pt.vu, offset = log(data_pt.vu$`Hep A 1st doses administered` + data_pt.vu$`Number refused vaccination for HAV`))
summary(fit.gee)

## GEE, final
fit.gee = geeglm(`Vaccine uptake` ~ `Incentive Offered?`, id = Location_ID_unique, data = data1)
summary(fit.gee)

# fit.gee = geeglm(data1$`Hep A 1st doses administered` / (data1$`Hep A 1st doses administered` + data1$`Number refused vaccination for HAV`) ~ `Incentive Offered?`, family = binomial, id = Location_ID_unique, data = data1, weights = (data1$`Hep A 1st doses administered` + data1$`Number refused vaccination for HAV`))
# summary(fit.gee)

data1.vu = data1 %>%
  filter(is.na(`Vaccine uptake`) == F)
fit.gee = geeglm(data1.vu$`Hep A 1st doses administered` ~ `Incentive Offered?`, family = poisson, id = Location_ID_unique, data = data1.vu, offset = log(data1.vu$`Hep A 1st doses administered` + data1.vu$`Number refused vaccination for HAV`))
summary(fit.gee)
exp(fit.gee$coefficients[2])
coef(fit.gee) - qnorm(0.975) * sqrt(diag(fit.gee$geese$vbeta))
coef(fit.gee) + qnorm(0.975) * sqrt(diag(fit.gee$geese$vbeta))
exp(coef(fit.gee) - qnorm(0.975) * sqrt(diag(fit.gee$geese$vbeta)))
exp(coef(fit.gee) + qnorm(0.975) * sqrt(diag(fit.gee$geese$vbeta)))

data1.vu.2 = data1.vu %>%
  drop_na(`Vaccine eligible proportion`)
fit.gee2 = geeglm(data1.vu.2$`Vaccine uptake` ~ `Incentive Offered?` + `Vaccine eligible proportion`, id = Location_ID_unique, data = data1.vu.2)
summary(fit.gee2)
fit.gee2 = geeglm(data1.vu.2$`Hep A 1st doses administered` ~ `Incentive Offered?` + `Vaccine eligible proportion`, family = poisson, id = Location_ID_unique, data = data1.vu.2, offset = log(data1.vu.2$`Hep A 1st doses administered` + data1.vu.2$`Number refused vaccination for HAV`))
summary(fit.gee2)
exp(fit.gee2$coefficients[2])
coef(fit.gee2) - qnorm(0.975) * sqrt(diag(fit.gee2$geese$vbeta))
coef(fit.gee2) + qnorm(0.975) * sqrt(diag(fit.gee2$geese$vbeta))
exp(coef(fit.gee2) - qnorm(0.975) * sqrt(diag(fit.gee2$geese$vbeta)))
exp(coef(fit.gee2) + qnorm(0.975) * sqrt(diag(fit.gee2$geese$vbeta)))

data1.vu.3 = data1.vu
data1.vu.3$daysbl_media1 = with(data1.vu.3, pmax(as.numeric(as.Date(data1.vu.3$`Event Date`) - as.Date("2023-05-16")), 0))
data1.vu.3$daysbl_media2 = with(data1.vu.3, pmax(as.numeric(as.Date(data1.vu.3$`Event Date`) - as.Date("2023-06-14")), 0))
data1.vu.3$daysbl_media3 = with(data1.vu.3, pmax(as.numeric(as.Date(data1.vu.3$`Event Date`) - as.Date("2023-07-31")), 0))
fit.gee3 = geeglm(data1.vu.3$`Vaccine uptake` ~ `Incentive Offered?` + (daysbl + daysbl_media1 + daysbl_media2 + daysbl_media3), id = Location_ID_unique, data = data1.vu.3)
summary(fit.gee3)
fit.gee3 = geeglm(data1.vu.3$`Hep A 1st doses administered` ~ `Incentive Offered?` + (daysbl + daysbl_media1 + daysbl_media2 + daysbl_media3), family = poisson, id = Location_ID_unique, data = data1.vu.3, offset = log(data1.vu.3$`Hep A 1st doses administered` + data1.vu.3$`Number refused vaccination for HAV`))
summary(fit.gee3)
exp(fit.gee3$coefficients[2])
coef(fit.gee3) - qnorm(0.975) * sqrt(diag(fit.gee3$geese$vbeta))
coef(fit.gee3) + qnorm(0.975) * sqrt(diag(fit.gee3$geese$vbeta))
exp(coef(fit.gee3) - qnorm(0.975) * sqrt(diag(fit.gee3$geese$vbeta)))
exp(coef(fit.gee3) + qnorm(0.975) * sqrt(diag(fit.gee3$geese$vbeta)))

data1.vu.4 = data1.vu %>%
  drop_na(`Vaccine eligible proportion`)
data1.vu.4$daysbl_media1 = with(data1.vu.4, pmax(as.numeric(as.Date(data1.vu.4$`Event Date`) - as.Date("2023-05-16")), 0))
data1.vu.4$daysbl_media2 = with(data1.vu.4, pmax(as.numeric(as.Date(data1.vu.4$`Event Date`) - as.Date("2023-06-14")), 0))
data1.vu.4$daysbl_media3 = with(data1.vu.4, pmax(as.numeric(as.Date(data1.vu.4$`Event Date`) - as.Date("2023-07-23")), 0))
fit.gee4 = geeglm(data1.vu.4$`Hep A 1st doses administered` ~ `Incentive Offered?` + `Vaccine eligible proportion` + (daysbl + daysbl_media1 + daysbl_media2 + daysbl_media3), family = poisson, id = Location_ID_unique, data = data1.vu.4, offset = log(data1.vu.4$`Hep A 1st doses administered` + data1.vu.4$`Number refused vaccination for HAV`))
summary(fit.gee4)
exp(fit.gee4$coefficients[2])
coef(fit.gee4) - qnorm(0.975) * sqrt(diag(fit.gee4$geese$vbeta))
coef(fit.gee4) + qnorm(0.975) * sqrt(diag(fit.gee4$geese$vbeta))
exp(coef(fit.gee4) - qnorm(0.975) * sqrt(diag(fit.gee4$geese$vbeta)))
exp(coef(fit.gee4) + qnorm(0.975) * sqrt(diag(fit.gee4$geese$vbeta)))

