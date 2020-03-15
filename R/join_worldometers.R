library(rvest)
url = "https://www.worldometers.info/coronavirus/#countries"

raw_web = read_html(url)  
table_countries_raw = html_table(raw_web)[[1]]

time_worldometer = stringr::str_extract(string = html_text(raw_web),
                                        pattern = '\\w+\\s\\d+(st)?(nd)?(rd)?(th)?,\\s+\\d+, \\d+:\\d+ GMT')

# Select variable to use
variable_to_use = "TotalCases"
if (cases_deaths == "deaths") variable_to_use = "TotalDeaths"
  

table_countries = table_countries_raw %>% as_tibble() %>%
  rename(country = `Country,Other`,
         value = {{variable_to_use}}) %>% 
  mutate(time = as.Date(time_worldometer, "%b %d, %Y"),
         value = as.numeric(gsub(",", "", value))) %>% 
  select(country, time, value) %>%
  mutate(
    country = 
      case_when(
        country == "S. Korea" ~ "South Korea",
        country == "USA" ~ "US",
        country == "UK" ~ "United Kingdom",
        TRUE ~ country
      )
  )
