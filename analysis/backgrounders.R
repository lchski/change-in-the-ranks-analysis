
backgrounder_paragraphs_raw

backgrounders2 <- backgrounders %>%
  mutate(content = map_chr(page, ~ .x %>% html_node("article.full-article .content-news-article") %>% as.character))


backgrounders %>%
  select(id, date, title, text) %>%
  unnest_tokens(token, text, token = "regex", pattern = "\n") %>%
  mutate(token = trimws(token)) %>%
  mutate(educ = str_detect(token, "^education")) %>%
  filter(educ) %>%
  mutate(token = case_when(
    educ & str_detect(token, "^education$") ~ list(token),
    educ & ! str_detect(token, "^education$") ~ list("education", str_split(token, "^education")),
    TRUE ~ list(token)
  ))
