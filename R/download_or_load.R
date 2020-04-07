download_or_load <- 
  
  purrr::safely(
    
    function(URL, file_name, hours_threshold = .5, maxTimes = 5) {
    
      # DEBUG
      # library(dplyr)
      # library(httr)
      # library(readr)
      # library(rvest)
      # library(tidyr)
      # file_name = "temp_worldometers.html"
      # hours_threshold = 1
      # maxTimes = 10
      # URL = "https://www.worldometers.info/coronavirus/#countries"
      
      file_info = file.info(file_name)$mtime
      
      # If file does not exist, should_download
      if (is.na(file_info)) {
        
        message("File ", file_name, " does not exist, downloading")
        should_download = TRUE
        
      # If file exists check how old it is. If older than hours_threshold, should_download
      } else {
        
        time_downloaded = round(difftime(Sys.time(), file_info, units='hours'), 2)
        should_download = time_downloaded > hours_threshold
        message("File ", file_name, " exists since ", time_downloaded, " hours ago, will ", "NOT"[!should_download], " download again")
        
      }
      
      # If should_download, try to download maxTimes times
      if (should_download == TRUE) {
        
        RETRY(
          verb = "GET",
          url = URL,
          times = maxTimes,
          quiet = FALSE,
          terminate_on = NULL,
          encode = c("multipart", "form", "json", "raw")
          ) %>%
          write_lines(path = file_name)
        
        file_info_updated = file.info(file_name)$mtime
        time_downloaded_updated = round(difftime(Sys.time(), file_info_updated, units='hours'), 2)
        message("File ", file_name, " re-downloaded ", time_downloaded_updated, " hours ago\n")
        output = "downloaded"; output
        
      } else {
        
        message("Did not download again, file was ", time_downloaded, " hours old\n")
        output = "present"; output
        
      }
    }
  )

# download_or_load("temp_worldometers.html", URL = "https://www.worldometers.info/coronavirus/#countries")

# url_cases <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv"
# download_or_load("outputs/url_cases.csv", URL = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv")

