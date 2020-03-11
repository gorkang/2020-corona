# USING CODE FROM: 
# https://github.com/JonMinton/COVID-19 # @JonMinton
# https://gist.github.com/christophsax/dec0a57bcbc9d7517b852dd44eb8b20b # @christoph_sax


# Libraries ---------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(ggrepel)
library(readr)
library(tidyr)
library(scales)


# Data prep ---------------------------------------------------------------

# Data Repo Johns Hopkins CSSE (https://github.com/CSSEGISandData/COVID-19)
url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv"
dta_raw <- read_csv(url, col_types = cols()) %>% select(-Lat, -Long)

selection <- c("Italy", "Iran", "Spain", "South Korea", "France", "Germany", "US", "Japan", "Mainland China")

dta <<- dta_raw %>%
  
  # tidy data
  rename(province = `Province/State`, country = `Country/Region`) %>%
  pivot_longer(c(-province, -country), "time") %>%
  mutate(time = as.Date(time, "%m/%d/%y")) %>%
  
  # rename some countries
  mutate(
    country = case_when(
      country == "Iran (Islamic Republic of)" ~ "Iran",
      country == "Hong Kong SAR"  ~ "Hong Kong",
      country == "Republic of Korea" ~ "South Korea",
      TRUE ~ country
    )) %>% 
  

  # ignore provinces
  group_by(country, time) %>%
  summarize(value = sum(value)) %>%
  ungroup() %>%
  
  # calculate new infections
  arrange(time) %>%
  group_by(country) %>%
  mutate(diff = value - lag(value)) %>%
  ungroup() %>%
  filter(!is.na(diff)) %>%
  arrange(country, time) %>% 
  
  filter(value >= 100) %>%
  group_by(country) %>% 
  mutate(days_after_100 = 0:(length(country)-1)) %>% 
  
  # Create labels for last instance for each country
  group_by(country) %>% 
  mutate(
    name_end = 
      case_when(
        value == max(value) ~ paste0(as.character(country), ": ", format(value, big.mark=","), " - ", days_after_100, " days"),
        TRUE ~ "")) 



