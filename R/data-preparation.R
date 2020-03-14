# USING CODE FROM: 
# https://github.com/JonMinton/COVID-19 # @JonMinton
# https://gist.github.com/christophsax/dec0a57bcbc9d7517b852dd44eb8b20b # @christoph_sax

# min_n = 100

# Libraries ---------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(ggrepel)
library(readr)
library(tidyr)
library(scales)


# Parameters ---------------------------------------------------------------

data_source = "JH" #"JH" # OWID



# Data prep ---------------------------------------------------------------

if (data_source == "JH") {

  # Data Repo Johns Hopkins CSSE (https://github.com/CSSEGISandData/COVID-19)
  url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv"
  dta_raw <- read_csv(url, col_types = cols()) %>% 
    select(-Lat, -Long, -`Province/State`) %>% 
    rename(
      # province = `Province/State`, 
      country = `Country/Region`) %>% 
    # tidy data
    pivot_longer(c(-country), "time") %>%
    mutate(time = as.Date(time, "%m/%d/%y"))
  
  
} else if (data_source == "OWID") {
  
  # NOT SO UPDATED? 
  
  url = "http://cowid.netlify.com/data/full_data.csv"
  dta_raw <- read_csv(url, col_types = cols()) %>% 
    rename(country = location, 
           value = total_cases,
           time = date) %>% 
    select(country, time, value)
  
}



dta <<- dta_raw %>%
  
  # rename some countries
  mutate(
    country = case_when(
      # country == "Iran (Islamic Republic of)" ~ "Iran",
      country == "Korea, South" ~ "South Korea",
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
  
  # filter(value >= min_n) %>%
  group_by(country) %>%
  mutate(days_after_100 = 0:(length(country)-1)) %>%

  # Create labels for last instance for each country
  group_by(country) %>%
  mutate(
    name_end =
      case_when(
        days_after_100 == max(days_after_100) ~ paste0(as.character(country), ": ", format(value, big.mark=","), " - ", days_after_100, " days"),
        TRUE ~ ""))


source(here::here("R/join_worldometers.R"))

dta =  dta %>% mutate(source = "JHU") %>% 
  bind_rows(table_countries %>% 
              mutate(source = "worldometers")) %>% 
  arrange(time) %>%
  group_by(country) %>%
  mutate(days_after_100 = 
           case_when(
             is.na(days_after_100) ~ as.integer(lag(days_after_100) + 1),
             TRUE ~ days_after_100),
         diff = value - lag(value),
         name_end = 
           case_when(
             is.na(name_end) ~ "",
             TRUE ~ name_end)) %>% 
  ungroup() %>%   # Create labels for last instance for each country
  group_by(country) %>% 
  mutate(
    name_end = 
      case_when(
        days_after_100 == max(days_after_100) ~ paste0(as.character(country), ": ", format(value, big.mark=","), " - ", days_after_100, " days"),
        TRUE ~ "")) 




V1_alternatives = dta %>%
  filter(value > 10) %>% 
  distinct(country) %>% pull(country)

top_countries = dta %>%
  group_by(country) %>% 
  filter(value == max(value), 
         country != "Cruise Ship") %>% 
  distinct(country, value) %>% 
  ungroup() %>%
  top_n(n = 10, wt = value) %>% 
  filter(!country %in% c("Total:", "China")) %>% 
  pull(country)
