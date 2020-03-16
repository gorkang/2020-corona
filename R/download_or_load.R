download_or_load <- function(URL, file_name, hours_threshold = 1, maxTimes = 10) {
  
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
  
  if (is.na(file_info)) {
    message("File does not exist, downloading")
    
    should_download = TRUE
  } else {
    time_downloaded = (Sys.time() - file_info)
    message("File downloaded ", time_downloaded, " hours ago")
    should_download = (Sys.time() - file_info) > hours_threshold
  }
  
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
    time_downloaded_updated = (Sys.time() - file_info_updated)
    message("File downloaded ", time_downloaded_updated, " hours ago")
    
    
  }
  
}

# download_or_load("temp_worldometers.html", URL = "https://www.worldometers.info/coronavirus/#countries")
