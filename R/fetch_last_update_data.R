fetch_last_update_data <- 
  purrr::safely(
    
    function() {
      curl::curl_download('https://api.github.com/repos/CSSEGISandData/COVID-19/commits?path=csse_covid_19_data%2Fcsse_covid_19_time_series%2Ftime_series_19-covid-Confirmed.csv&page=1&per_page=1', destfile = "temp.html")
      last_commit = jsonlite::read_json(here::here("temp.html"))
      date_last_commit_raw = last_commit[[1]]$commit$author$date
      strptime(date_last_commit_raw, "%FT%T", tz = "GMT")
      }
    )

# last_commit_time = fetch_last_update_data()$result
