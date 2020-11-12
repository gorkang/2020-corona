download_or_load_OWID <- 
  
  purrr::safely(
    
    function(URL = "https://covid.ourworldindata.org/data/owid-covid-data.csv", file_name, hours_threshold = 6, maxTimes = 5) {

      # DEBUG
      # URL = "https://covid.ourworldindata.org/data/owid-covid-data.csv"
      # file_name = "outputs/raw_OWID.csv"
      # hours_threshold = 6
      # maxTimes = 5
      
      file_info_OWID <- file.info(file_name)$mtime
      
      # If file does not exist, should_download
      if (is.na(file_info_OWID)) {
        
        message("File ", file_name, " does not exist, downloading")
        should_download = TRUE
        
        # If file exists check how old it is. If older than hours_threshold, should_download
      } else {
        
        time_downloaded = round(difftime(Sys.time(), file_info_OWID, units='hours'), 2)
        should_download = time_downloaded > hours_threshold
        message("File ", file_name, " downloded ", time_downloaded, " hours ago, will ", "NOT"[!should_download], " download again")
        
      }
      
      # If should_download, try to download maxTimes times
      if (should_download == TRUE) {
        
        # DF_all = jsonlite::fromJSON(URL) %>% as.data.frame %>% 
        #   write_csv(paste0(file_name))
        
        DF_OWID_RAW <- read_csv(URL,
                                # guess_max = 10000,
                                col_types =

                                  cols(
                                    .default = col_double(),
                                    iso_code = col_character(),
                                    continent = col_character(),
                                    location = col_character(),
                                    date = col_date(format = ""),
                                    tests_units = col_character()
                                  ))
        
        write_csv(DF_OWID_RAW, "outputs/raw_OWID.csv")
        
        
        file_info_updated = file.info(file_name)$mtime
        time_downloaded_updated = round(difftime(Sys.time(), file_info_updated, units='hours'), 2)
        message("File ", file_name, " re-downloaded ", time_downloaded_updated, " hours ago\n")
        output = "downloaded"; output
        
      } else {
        
        message("Did not download again, file was ", time_downloaded, " hours old\n")
        output = "present"; output
        
      }
      
      file_info_OWID <<- file.info(file_name)$mtime
      
    }
  )

# url = "https://covid.ourworldindata.org/data/owid-covid-data.csv"
# DF_OWID_RAW <- read_csv(url, 
#                         # guess_max = 10000,
#                         col_types = 
#                           
#                           cols(
#                             .default = col_double(),
#                             iso_code = col_character(),
#                             continent = col_character(),
#                             location = col_character(),
#                             date = col_date(format = ""),
#                             tests_units = col_character()
#                           ))
# 
# # write_csv(DF_OWID_RAW, "outputs/raw_OWID.csv")
