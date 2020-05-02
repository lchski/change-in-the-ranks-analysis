library(textclean)

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
  mutate(token = trimws(token, whitespace = "[\\h\\v]")) %>%
  filter(token != "") %>%
  mutate(section = case_when(
    str_detect(token, "^education") ~ "education",
    str_detect(token, "^professional experience") ~ "professional experience",
    str_detect(token, "^related product") ~ "related products",
    TRUE ~ NA_character_
  )) %>%
  group_by(id) %>%
  fill(section)


backgrounder_paragraphs %>%
  mutate(
    from = str_match(token, "^(?:since [a-z]{0,9}[[:space:]]*)?([0-9]{4})")[,2], ## [fn1] for regex explanation
    to = str_match(token, "^(?:since [a-z]{0,9}[[:space:]]*)?(?:[0-9]{4})[[[:space:]]\\-]*([0-9]{4})")[,2]
  ) %>%
  filter(section == "professional experience") %>%
  filter(! is.na(to))

## [fn1]:
## Looks for a four-digit number `([0-9]{4})` that can come either:
##   - right at the start of the string
##   - optionally, after "since [month] " at the start of the string `(?:since [a-z]{0,9}[[:space:]]*)?`



backgrounder_paragraphs %>%
  left_join(backgrounders %>% select(id, url)) %>%
  select(id, url, everything()) %>%
  write_csv("data/out/backgrounder-paragraphs.csv")

