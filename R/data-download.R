data_download <- function(cases_deaths = "cases") {

  # Data preparation --------------------------------------------------------

  # Download worldometers
  source(here::here("R/join_worldometers.R"), local = TRUE)

  # Data Repo Johns Hopkins CSSE (https://github.com/CSSEGISandData/COVID-19)
  url_cases <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv"
  url_deaths = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv"
  
  dta_raw_cases <- read_csv(url_cases, col_types = cols()) %>% 
    select(-Lat, -Long, -`Province/State`) %>% 
    rename(country = `Country/Region`) %>% 
    # tidy data
    pivot_longer(c(-country), "time") %>%
    mutate(time = as.Date(time, "%m/%d/%y")) %>% 
    rename(cases = value) %>% 
    # ignore provinces
    group_by(country, time) %>%
    summarize(cases_sum = sum(cases)) %>%
    ungroup() 

  dta_raw_deaths <- read_csv(url_deaths, col_types = cols()) %>% 
    select(-Lat, -Long, -`Province/State`) %>% 
    rename(country = `Country/Region`) %>% 
    # tidy data
    pivot_longer(c(-country), "time") %>%
    mutate(time = as.Date(time, "%m/%d/%y")) %>% 
    rename(deaths = value) %>% 
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
        TRUE ~ country
      )) %>% 
    

    
    # calculate new infections
    arrange(time) %>%
    group_by(country) %>%
    mutate(cases_diff = cases_sum - lag(cases_sum),
           deaths_diff = deaths_sum - lag(deaths_sum)) %>%
    ungroup() %>%
    filter(!is.na(cases_diff)) %>%
    arrange(country, time) %>% 
    mutate(source = "JHU") %>% 
    
    # Join worldometers
    bind_rows(table_countries %>% 
                mutate(source = "worldometers")) %>% 
    # replace_na(0)
    
    # write_data
    write_csv("raw_data.csv")

}
