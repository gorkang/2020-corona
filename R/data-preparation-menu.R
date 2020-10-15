library(readr)
library(dplyr)

raw_data_exists = file.info(here::here("outputs/raw_data.csv"))$mtime
if (is.na(raw_data_exists)) data_download()

dta_raw = read_csv(here::here("outputs/raw_data.csv"), 
                   col_types = 
                     cols(
                       country = col_character(),
                       time = col_date(format = ""),
                       cases_sum = col_double(),
                       cases_diff = col_double(),
                       deaths_sum = col_double(),
                       deaths_diff = col_double(),
                       source = col_character()
                     )) 
# Menu vars ---------------------------------------------------------------

 DF_menu <- dta_raw %>%
  # filter(value > 1) %>% 
  filter(!country %in% c("Total:", "Diamond Princess", "World")) %>% 
  arrange(desc(cases_sum)) %>% 
  distinct(country)

 V1_alternatives <<- DF_menu %>% 
  pull(country)

top_countries <<- DF_menu %>% 
  filter(!country %in% c("Total:", "Cruise Ship", "China", "Diamond Princess")) %>% 
  slice_head(n = 6) %>% #, wt = value) %>% 
  pull(country)
