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
               ))
    
  
  # JHU API ------------------------------------------------------------------

  download_or_load_JH_API(file_name = "outputs/raw_JH.csv")
  
  DF_JHU_raw = read_csv(here::here("outputs/raw_JH.csv"), 
                             col_types = 
                               cols(
                                 Country = col_character(),
                                 Province = col_character(),
                                 Lat = col_double(),
                                 Lon = col_double(),
                                 Date = col_datetime(format = ""),
                                 Cases = col_double(),
                                 Status = col_character()
                               ))
  
  DF_JHU_clean = DF_JHU_raw %>% 
    as_tibble() %>% 
    select(-Lat, -Lon) %>% 
    rename(country = Country,
           time = Date) %>% 
    mutate(time = as.Date(time)) %>% 
    filter(country != "") %>% 
    mutate(country = 
             case_when(
               country == "Iran (Islamic Republic of)" ~ "Iran",
               country == "Mainland China" ~ "China",
               TRUE ~ country
             )) %>% 
    filter(!(time == "2020-03-11")) %>% 
    distinct(country, Province, time, Cases, Status) %>%
    group_by(country, Province, time, Status) %>% 
    summarise(Cases = sum(Cases)) %>% 
    # tidy data
    pivot_wider(names_from = Status, values_from = Cases) %>% 
    mutate(time = as.Date(time, "%m/%d/%y")) %>% 
    ungroup() %>% 
    group_by(country, time) %>%
    summarize(cases_sum = sum(confirmed),
              deaths_sum = sum(deaths),
              recovered_sum = sum(recovered)) %>%
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

    # replace_na(0)
    
    # write_data
    DF_write %>% 
      write_csv("outputs/raw_data.csv")

}
