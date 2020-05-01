library(tidyverse)

library(jsonlite)

urls_raw <- tibble(web_archive_url = read_json("data/source/urls.json", simplifyVector = TRUE))

urls <- urls_raw %>%
  mutate(original_url = str_remove(web_archive_url, fixed("http://web.archive.org/web/*/")))
