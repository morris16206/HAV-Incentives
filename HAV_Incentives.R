rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


library(readxl)
library(openxlsx)
library(plyr)
library(tidyverse)
library(geepack)


## Preprocessing
data = read_xlsx("UCSD Modeling_Project VIBE_Event and Gift Card Data_24.06.26.xlsx")
data$`Event ID` = as.numeric(data$`Event ID`)
ifelse(data$`Number of gift cards distributed` == "N/A", NA, data$`Number of gift cards distributed`)
data$`Number of gift cards distributed` = as.numeric(data$`Number of gift cards distributed`)

data1 = data %>%
  filter(`Event Date` < "2023-02-21" | `Event Date` > "2023-11-30" | `Incentive Offered?` == "Yes") %>%
  drop_na(`Number refused vaccination for HAV`) %>%
  arrange(`Location ID`, `Event Date`) %>%
  mutate(Location_ID_unique = if_else(`Location ID` == "L000",
                                      1000+row_number(),
                                      as.numeric(str_sub(`Location ID`, -3, -1)))) %>%
  arrange(Location_ID_unique)

data1$`Vaccine uptake` = 100 * data1$`Hep A 1st doses administered` / (data1$`Hep A 1st doses administered` + data1$`Number refused vaccination for HAV`)


# ID for refused missingness
data2 = data %>%
  filter(`Event Date` < "2023-02-21" | `Event Date` > "2023-11-30" | `Incentive Offered?` == "Yes") %>%
  filter(is.na(`Number refused vaccination for HAV`)) %>%
  select(`Event ID`) %>%
  arrange(`Event ID`)
write.xlsx(data2, file = "ID for missing refused.xlsx")

# ID for first dose equal to zero
data3 = data %>%
  filter(`Event Date` < "2023-02-21" | `Event Date` > "2023-11-30" | `Incentive Offered?` == "Yes") %>%
  filter(`Hep A 1st doses administered` == 0 & `Number refused vaccination for HAV` == 0) %>%
  select(`Event ID`) %>%
  arrange(`Event ID`)
write.xlsx(data3, file = "ID for both first dose and refused equal to 0.xlsx")


## Plot
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

# Boxplot
ggplot(data_loess, aes(x = factor(segment), y = `Vaccine uptake`)) +
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

# LOESS curve
ggplot(data1, aes(x = `Event Date`, y = `Vaccine uptake`)) +
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


## GEE
fit.gee = geeglm(`Vaccine uptake` / 100 ~ `Incentive Offered?`, id = Location_ID_unique, data = data1)
summary(fit.gee)

