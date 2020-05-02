library(tidyverse)
library(jsonlite)
library(lubridate)

library(tidytext)

library(helpers)

source("load/download-announcement-pages.R")

urls_raw <- tibble(web_archive_url = read_json("data/source/urls.json", simplifyVector = TRUE))

announcements <- urls_raw %>%
  mutate(url = str_remove(web_archive_url, fixed("http://web.archive.org/web/*/"))) %>%
  mutate(date = as_date(str_extract(url, "[0-9]{4}/[0-9]{2}/[0-9]{2}"))) %>%
  arrange(date) %>%
  filter(str_detect(url, "news-releases")) %>%
  mutate(page = map(url, retrieve_page_at_url)) %>%
  mutate(is_actual_result = ! map_lgl(page, is_bare_list)) %>%
  filter(is_actual_result) %>%
  select(-is_actual_result, -web_archive_url) %>%
  mutate(id = row_number()) %>%
  select(id, everything()) %>%
  mutate(announcement = map(page, process_article_page)) %>%
  unnest_wider(c(announcement))

backgrounder_links <- announcements %>%
  select(id, page) %>%
  mutate(backgrounder_links = map(page, extract_backgrounder_links)) %>%
  unnest(c(backgrounder_links))

backgrounder_link_urls <- backgrounder_links %>%
  select(url) %>%
  distinct() %>%
  filter(! is.na(url)) %>%
  mutate(url = paste0("https://pm.gc.ca", url)) %>%
  mutate(url = str_replace(url, fixed("https://pm.gc.cahttps://pm.gc.ca/"), fixed("https://pm.gc.ca/")))

backgrounders <- backgrounder_link_urls %>%
  mutate(date = as_date(str_extract(url, "[0-9]{4}/[0-9]{2}/[0-9]{2}"))) %>%
  arrange(date) %>%
  mutate(page = map(url, retrieve_page_at_url)) %>%
  mutate(id = row_number()) %>%
  select(id, everything()) %>%
  mutate(backgrounder = map(page, process_article_page)) %>%
  unnest_wider(c(backgrounder)) %>%
  mutate(title = trimws(str_remove(title, fixed("Biography of", ignore_case = TRUE))))



announcements %>%
  select(-page) %>%
  write_csv("data/out/announcements.csv")

backgrounders %>%
  select(-page) %>%
  write_csv("data/out/backgrounders.csv")
  

