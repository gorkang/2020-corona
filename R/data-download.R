data_download <- function(cases_deaths = "cases") {

  # DEBUG
  # source(here::here("R/fetch_worldometers_safely.R"), local = TRUE)
  # source(here::here("R/download_or_load.R"))
  
  
  # Data preparation --------------------------------------------------------

  # Download worldometers
  fetch_worldometers_safely()
  
  table_countries = read_csv(here::here("outputs/raw_data_worldometers.csv"), 
           col_types = 
               cols(
                 country = col_character(),
                 time = col_date(format = ""),
                 cases_sum = col_double(),
                 deaths_sum = col_double()
               ))
    
  # Data Repo Johns Hopkins CSSE (https://github.com/CSSEGISandData/COVID-19)
  url_cases <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv"
  download_or_load("outputs/url_cases.csv", URL = url_cases)
  
  url_deaths = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv"
  download_or_load("outputs/url_deaths.csv", URL = url_deaths)
  
  
  dta_raw_cases <- read_csv("outputs/url_cases.csv", col_types = cols()) %>% 
    select(-Lat, -Long, -`Province/State`) %>% 
    rename(country = `Country/Region`) %>% 
    # tidy data
    pivot_longer(c(-country), "time") %>%
    mutate(time = as.Date(time, "%m/%d/%y")) %>% 
    rename(cases = value) %>% 
    drop_na(cases) %>% 
    # ignore provinces
    group_by(country, time) %>%
    summarize(cases_sum = sum(cases)) %>%
    ungroup() 

  dta_raw_deaths <- read_csv("outputs/url_deaths.csv", col_types = cols()) %>% 
    select(-Lat, -Long, -`Province/State`) %>% 
    rename(country = `Country/Region`) %>% 
    # tidy data
    pivot_longer(c(-country), "time") %>%
    mutate(time = as.Date(time, "%m/%d/%y")) %>% 
    rename(deaths = value) %>% 
    drop_na(deaths) %>% 
    # ignore provinces
    group_by(country, time) %>%
    summarize(deaths_sum = sum(deaths)) %>%
    ungroup()
  
  dta_raw = dta_raw_cases %>% 
    full_join(dta_raw_deaths, by = c("country", "time"))
  
  
  
  dta_raw %>%
    # rename some countries
    mutate(
      country = case_when(
        country == "Korea, South" ~ "South Korea",
        country == "Taiwan*" ~ "Taiwan",
        country == "US" ~ "USA",
        TRUE ~ country
      )) %>% 
    
    mutate(source = "JHU") %>% 
    
    # Join worldometers
    bind_rows(table_countries %>% 
                mutate(source = "worldometers")) %>%
    
    # calculate new infections
    arrange(time) %>%
    group_by(country) %>%
    mutate(cases_diff = cases_sum - lag(cases_sum),
           deaths_diff = deaths_sum - lag(deaths_sum),
           cases_diff_pct = cases_diff / lag(cases_sum),
           deaths_diff_pct = deaths_diff / lag(deaths_sum)) %>% 
    ungroup() %>%
    filter(!is.na(cases_diff)) %>%
    arrange(country, time) %>%
    select(country, time, cases_sum, cases_diff,  deaths_sum, deaths_diff, source) %>% 

    # replace_na(0)
    
    # write_data
    write_csv("outputs/raw_data.csv")

}
