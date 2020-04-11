data_preparation <- function(data_source = "JHU", cases_deaths = "cases", countries_plot = "", min_n = 1, relative = FALSE) {

  
  # Data preparation --------------------------------------------------------
  
  DF_population_countries = read_csv("data/population_countries.csv",
                                     col_types = 
                                       cols(
                                         country = col_character(),
                                         population = col_double()
                                       ))

  
  DF_lockdowns = read_csv("data/lockdown_countries.csv", 
                          col_types = 
                            cols(
                              time = col_date(format = ""),
                              what = col_character(),
                              country = col_character(),
                              source = col_character()
                              )) %>% 
    select(time, country, what)
  
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
                       )) %>% 
    mutate(CFR_sum = round((deaths_sum/cases_sum) * 100, 2),
           CFR_diff = round((deaths_diff/cases_diff) * 100, 2))

  if (relative == TRUE) {
    dta_raw = dta_raw %>% 
      left_join(DF_population_countries, by = "country") %>% 
      mutate(cases_sum = round((cases_sum / population) * 1000000, 0),
             cases_diff = round((cases_diff / population) * 1000000, 0),
             deaths_sum = round((deaths_sum / population) * 1000000, 0),
             deaths_diff = round((deaths_diff / population) * 1000000, 0))
  } 
  
  
  # If there are countries, FILTER DF by countries, min_n...
  if (!is.null(countries_plot)) {
    
    # CFR is a special case. We need to filter by deaths to avoid 0's
    if (cases_deaths == "CFR") {
      
      dta_raw_filtered =
        dta_raw %>% 
        
        # filter min num
        filter(deaths_sum >= min_n) %>% 
        # rename
        rename(value = paste0(cases_deaths, "_sum")) %>% 
        rename(diff = paste0(cases_deaths, "_diff")) %>% 
        # selection
        filter(country %in% countries_plot)
      
    } else {
      
      dta_raw_filtered =
        dta_raw %>% 
        # rename
        rename(value = paste0(cases_deaths, "_sum")) %>% 
        rename(diff = paste0(cases_deaths, "_diff")) %>% 
        
        # selection
        filter(country %in% countries_plot) %>%
        
        # filter min num
        filter(value >= min_n)
    }


dta <<-
  dta_raw_filtered %>%
    group_by(country) %>%
    mutate(days_after_100 = 0:(length(country)-1)) %>%
    arrange(time) %>%
    group_by(country) %>%
    mutate(days_after_100 = 
             case_when(
               is.na(days_after_100) ~ as.integer(lag(days_after_100) + 1),
               TRUE ~ days_after_100),
           diff = round(value - lag(value), 2),
           diff_pct = diff / lag(value)
           ) %>% 
    ungroup() %>%   # Create labels for last instance for each country
    group_by(country) %>% 
    
    left_join(DF_lockdowns, by = c("country", "time")) %>% 
    mutate(
      name_end = 
        case_when(
          days_after_100 == max(days_after_100) & source == "worldometers" ~ paste0(as.character(country), ": ", format(value, big.mark=","), " - ", days_after_100, " days"),
          what == "lockdown" ~ "*",
          TRUE ~ "")) %>% 
    select(country, time, value, diff, everything())

  }
}
