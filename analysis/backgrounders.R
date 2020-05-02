
backgrounder_paragraphs_raw

backgrounders2 <- backgrounders %>%
  mutate(content = map_chr(page, ~ .x %>% html_node("article.full-article .content-news-article") %>% as.character))


backgrounders %>%
  select(id, date, title, text) %>%
  unnest_tokens(token, text, token = "regex", pattern = "\n") %>%
  mutate(token = trimws(token)) %>%
  mutate(educ = str_detect(token, "^education")) %>%
  filter(educ) %>%
  mutate(token = map(token, function(line_to_parse) {
    if (str_detect(line_to_parse, "^education$")) {
      return(list(line_to_parse))
    }

    if (str_detect(line_to_parse, "^education")) {
      return(c("education", str_split(line_to_parse, "^education")))
    }

    return(list(line_to_parse))
  })) %>%
  unnest(c(token)) %>%
  unnest(c(token)) %>%
  mutate(token = trimws(token)) %>%
  filter(token != "")
