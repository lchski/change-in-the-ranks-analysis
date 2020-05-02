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
  fill(section) %>%
  filter(! token %in% c("education", "professional experience", "related product", "related products")) %>%
  mutate(
    from = str_match(token, "^(?:since [a-z]{0,9}[[:space:]]*)?([0-9]{4})")[,2], ## [fn1] for regex explanation
    to = str_match(token, "^(?:since [a-z]{0,9}[[:space:]]*)?(?:[0-9]{4})[[[:space:]]\\-–]*([0-9]{4})")[,2] ## [fn2] for regex explanation
  ) %>%
  mutate(
    to = case_when(
      section == "professional experience" &
        str_detect(token, "^since") ~ as.character(year(date)), ## when it starts with "since", assume it ran until the announcement year
      section == "professional experience" &
        str_detect(token, "(?:since [a-z]{0,9}[[:space:]]*)?(?:[0-9]{4})[[[:space:]]\\-–]*(?:present)") ~ as.character(year(date)), ## same as above, but edge case of "since YYYY - present"
      section == "professional experience" &
        is.na(to) &
        ! is.na(from) ~ from, ## deal with one-year positions
      TRUE ~ to
    )
  ) %>%
  mutate_at(vars(from, to), as.integer) %>%
  group_by(id, section) %>%
  fill(from, to) %>% ## filling while grouped ensures that positions where the years were in a row above get properly dated 
  mutate(token = trimws(str_remove(token, "(?:since [a-z]{0,9}[[:space:]]*)?(?:[0-9]{4})[[[:space:]]\\-–]*(?:present|[0-9]{4})?"))) %>%
  filter(token != "")

## [fn1]:
## Looks for a four-digit number `([0-9]{4})` that can come either:
##   - right at the start of the string
##   - optionally, after "since [month] " at the start of the string `(?:since [a-z]{0,9}[[:space:]]*)?`
##
## [fn2]:
## Builds on the regex in [fn1], looks for a four-digit number again that;
##   - follows the [fn1] pattern
##   - has " - " after the [fn1] pattern `[[[:space:]]\\-]*`



backgrounder_paragraphs %>%
  left_join(backgrounders %>% select(id, url)) %>%
  select(id, url, date, title, section, from, to, token) %>%
  write_csv("data/out/backgrounder-paragraphs.csv", na = "")

