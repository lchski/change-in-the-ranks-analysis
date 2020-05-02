library(tidyverse)
library(jsonlite)
library(lubridate)

source("load/download-announcement-pages.R")

urls_raw <- tibble(web_archive_url = read_json("data/source/urls.json", simplifyVector = TRUE))

announcements <- urls_raw %>%
  mutate(original_url = str_remove(web_archive_url, fixed("http://web.archive.org/web/*/"))) %>%
  mutate(date = as_date(str_extract(original_url, "[0-9]{4}/[0-9]{2}/[0-9]{2}"))) %>%
  arrange(date) %>%
  mutate(page = map(original_url, retrieve_page_at_url)) %>%
  mutate(is_actual_result = ! map_lgl(page, is_bare_list)) %>%
  filter(is_actual_result) %>%
  select(-is_actual_result, -web_archive_url) %>%
  mutate(id = row_number()) %>%
  select(id, everything())

announcements %>%
  mutate(announcement = map(page, process_announcement_page)) %>%
  unnest_wider(c(announcement))
