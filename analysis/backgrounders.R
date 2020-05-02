library(textclean)

backgrounders %>%
  filter(id %in% broken_backgrounder_ids) %>%
  slice(1) %>%
  pull(page) %>%
  map(~ .x %>%
        html_node("article.full-article") %>%
        html_node(".content-news-article") %>%
        as.character %>%
        replace_html)

backgrounders2 <- backgrounders %>%
  filter(id %in% broken_backgrounder_ids) %>%
  mutate(text = map_chr(page, ~ .x %>%
        html_node("article.full-article") %>%
        html_node(".content-news-article") %>%
        as.character %>%
        replace_html))

backgrounders2 %>%
  select(id, date, title, text) %>%
  unnest_tokens(token, text, token = "regex", pattern = "\n")


backgrounder_paragraphs <- backgrounders %>%
  mutate(text = map_chr(
    page,
    ~ .x %>%
      html_node("article.full-article") %>%
      html_node(".content-news-article") %>%
      as.character %>%
      replace_html
  )) %>%
  select(id, date, title, text) %>%
  unnest_tokens(token, text, token = "regex", pattern = "\n") %>%
  mutate(token = trimws(token)) %>%
  filter(token != "") %>%
  mutate(section = case_when(
    str_detect(token, "^education") ~ "education",
    str_detect(token, "^professional experience") ~ "professional experience",
    str_detect(token, "^related product") ~ "related products",
    TRUE ~ NA_character_
  ))

## find the backgrounders which, for whatever reason, don't properly parse into multiple lines
broken_backgrounder_ids <- backgrounder_paragraphs %>%
  count_group(id) %>%
  filter(count == 1) %>%
  pull(id)

backgrounders %>%
  filter(id %in% broken_backgrounder_ids) %>%
  slice(1) %>%
  pull(page) %>%
  map(~ .x %>%
        html_node("article.full-article"))
  
