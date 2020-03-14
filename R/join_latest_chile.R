# library(tidyverse)
# url = "https://www.minsal.cl/wp-content/uploads/2020/03/2020-03-14-Casos-confirmados.pdf"

# Build filename and url
filename_minsal = paste0(Sys.Date(), "-Casos-confirmados.pdf")
url_minsal = paste0("https://www.minsal.cl/wp-content/uploads/", format(Sys.Date(), "%Y"), "/", format(Sys.Date(), "%m"), "/", filename_minsal)

# Download only if we don't already have the file
if (!filename_minsal %in% list.files(pattern = ".pdf")) download_html(url = url_minsal, file = filename)

# Extract Total from table
latest_chile = tabulizer::extract_tables(filename_minsal)[[1]] %>% tibble::as_tibble() %>% filter(V1 == "Total") %>% pull(V2) %>% as.numeric(.)

# Build DF with new info
DF_latest_chile = tibble(
  country = "Chile",
  time = Sys.Date(),
  value = latest_chile,
  diff = NA_real_,
  days_after_100 = NA_integer_,
  name_end = NA_character_,
  source = "www.minsal.cl")

# Bind to main DF (the main DF will be cleaned from duplicates in app.R)
dta = dta %>% 
  bind_rows(DF_latest_chile)
