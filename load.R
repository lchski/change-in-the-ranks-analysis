library(tidyverse)
library(jsonlite)
library(lubridate)
library(janitor)

library(tidytext)

# library(helpers)

source("lib/download-announcement-pages.R")

announcements_raw <- fs::dir_ls("data/source/github--lchski--pm.gc.ca-news-data/pages/", glob = "*.json") %>%
  map_dfr(read_json, .id = "source_file") %>%
  filter(str_detect(path, "change"), str_detect(path, "rank"), str_detect(path, "service"))

announcements <- announcements_raw %>%
  rename(url = path, page = articleHtml, page_title = pageTitle) %>%
  mutate(date = as_date(str_extract(url, "[0-9]{4}/[0-9]{2}/[0-9]{2}"))) %>%
  arrange(date) %>%
  mutate(id = row_number()) %>%
  select(id, everything()) %>%
  mutate(page = map(page, read_html)) %>%
  mutate(text = map_chr(page, html_text))

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
  write_csv("data/out/announcements.csv", na = "")

backgrounders %>%
  select(-page) %>%
  write_csv("data/out/backgrounders.csv", na = "")
  

