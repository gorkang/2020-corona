library(tidyverse)
library(rvest)
library(janitor)

# url = "https://en.wikipedia.org/wiki/National_responses_to_the_2019%E2%80%9320_coronavirus_pandemic"
#   
# raw_table = url %>%
#   read_html() %>%
#   html_node(xpath = '/html/body/div[3]/div[3]/div[4]/div/table[5]') %>%
#   html_table(fill = TRUE)
# 
# DF = 
# raw_table %>% 
#   janitor::row_to_names(row_number = 1) %>% 
#   janitor::clean_names() %>% 
#   as_tibble() %>% 
#   mutate(start_date = as.Date(gsub("([0-9]{4}-[0-9]{2}-[0-9]{2}).*", "\\1", start_date)),
#          what = "lockdown",
#          source = "https://en.wikipedia.org/wiki/National_responses_to_the_2019%E2%80%9320_coronavirus_pandemic") %>% 
#   filter(level == "National") %>% 
#   rename(time = start_date) %>% 
#   select(time, what, country, source)


  # DF %>% write_csv("data/lockdown_countries.csv")
  

  DF = read_csv("data/lockdown_countries.csv")
  