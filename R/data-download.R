data_download <- function(cases_deaths = "cases") {

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
               )) %>% as_tibble()

  
  # JHU API ------------------------------------------------------------------

  download_or_load_JH_API(file_name = "outputs/raw_JH.csv")

  DF_JHU_raw = read_csv(here::here("outputs/raw_JH.csv"), 
                             col_types = 
                          cols(
                            Country = col_character(),
                            CountryCode = col_character(),
                            Lat = col_double(),
                            Lon = col_double(),
                            Confirmed = col_double(),
                            Deaths = col_double(),
                            Recovered = col_double(),
                            Active = col_double(),
                            Date = col_datetime(format = ""),
                            LocationID = col_character(),
                            Province = col_character(),
                            City = col_character(),
                            CityCode = col_character()
                          ))
  

  DF_JHU_clean = DF_JHU_raw %>% 
    as_tibble() %>% 
    select(Country, Province, Confirmed, Deaths, Date) %>% 
    rename(country = Country,
           time = Date) %>% 
    mutate(time = as.Date(time)) %>% 
    filter(country != "") %>% 
    mutate(country = 
             case_when(
               country == "Iran (Islamic Republic of)" ~ "Iran",
               country == "United States of America" ~ "USA",
               country == "Iran, Islamic Republic of" ~ "Iran",
               country == "Taiwan, Republic of China" ~ "Taiwan",
               country == "Mainland China" ~ "China",
               country == "UK" ~ "United Kingdom",
               country == "Bahamas, The" ~ "Bahamas",
               country == "Gambia, The" ~ "Gambia",
               country == "Hong Kong SAR" ~ "Hong Kong",
               country == "Korea, South" ~ "South Korea",
               country == "Russian Federation" ~ "Russia",
               country == "occupied Palestinian territory" ~ "Palestine",
               TRUE ~ country
             )) %>% 
    filter(!(time == "2020-03-11")) %>% 
    pivot_longer(c("Confirmed", "Deaths"), names_to = "Status", values_to = "Cases") %>% 
    distinct(country, Province, time, Cases, Status) %>%
    group_by(country, Province, time, Status) %>% 
    summarise(Cases = sum(Cases)) %>% 
    pivot_wider(names_from = Status, values_from = Cases) %>% 
    mutate(time = as.Date(time, "%m/%d/%y")) %>% 
    ungroup() %>% 
    group_by(country, time) %>%
    summarize(cases_sum = sum(Confirmed),
              deaths_sum = sum(Deaths)) %>% 
              # recovered_sum = sum(recovered)) %>%
    ungroup()

  DF_write = DF_JHU_clean %>%
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
    select(country, time, cases_sum, cases_diff,  deaths_sum, deaths_diff, source)

    # write_data
    DF_write %>% 
      write_csv("outputs/raw_data.csv")

}
