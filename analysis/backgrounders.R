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
  filter(! token %in% c("education", "professional experience", "related product", "related products"))

profexp <- backgrounder_paragraphs %>%
  filter(section == "professional experience") %>%
  mutate(
    from = str_match(token, "^(?:since [a-z]{0,9}[[:space:]]*)?([0-9]{4})")[,2], ## [fn1] for regex explanation
    to = str_match(token, "^(?:since [a-z]{0,9}[[:space:]]*)?(?:[0-9]{4})[[[:space:]]\\-–]*([0-9]{4})")[,2] ## [fn2] for regex explanation
  ) %>%
  mutate(
    to = case_when(
      str_detect(token, "^since") ~ as.character(year(date)), ## when it starts with "since", assume it ran until the announcement year
      str_detect(token, "(?:since [a-z]{0,9}[[:space:]]*)?(?:[0-9]{4})[[[:space:]]\\-–]*(?:present)") ~ as.character(year(date)), ## same as above, but edge case of "since YYYY - present"
      is.na(to) & ! is.na(from) ~ from, ## deal with one-year positions
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

educ <- backgrounder_paragraphs %>%
  filter(section == "education") %>%
  mutate(token = str_replace(token, "post graduate certificate", "postgradcertificate")) %>%
  mutate(token = str_replace(token, "executive master", "execmaster")) %>%
  mutate(token = str_split(token, "(?= bachelor| graduate diploma| postgradcertificate| certificate| directors education program| licence| master| execmaster| ph\\.d| doctorate| doctoral)")) %>%
  unnest(c(token)) %>%
  mutate(token = str_replace(token, "postgradcertificate", "post graduate certificate")) %>%
  mutate(token = str_replace(token, "execmaster", "executive master")) %>%
  mutate(token = str_replace(token, "graduate diploma in", "graduate diploma,")) %>%
  mutate(token = str_split(token, "(?=diploma in)")) %>%
  unnest(c(token)) %>%
  mutate(token = str_replace_all(token, "’", "'")) %>%
  mutate(token = str_replace(token, "bachelor's degree in", "bachelor's degree of")) %>%
  mutate(token = str_replace(token, "master's degree in", "master's degree of")) %>%
  mutate(token = str_replace(token, "doctorate in", "doctorate of")) %>%
  mutate(token = str_replace(token, "bachelor in", "bachelor of")) %>%
  mutate(token = str_replace(token, "master in", "master of")) %>%
  mutate(token = str_replace(token, " in ", ", ")) %>%
  mutate(token = trimws(token, whitespace = "[\\h\\v]")) %>%
  filter(token != "") %>%
  mutate(token = str_replace(token, "bachelor's degree", "bachelor of")) %>%
  mutate(token = str_remove(token, fixed(" (honours)"))) %>%
  mutate(token = str_remove(token, fixed(" (specialized honours)"))) %>%
  mutate(token = str_remove(token, fixed("specialization, "))) %>%
  mutate(token = str_replace(token, fixed(" ("), ", ")) %>%
  mutate(token = str_replace(token, fixed(")"), ", ")) %>%
  mutate(token = str_replace(token, fixed(", ,"), ",")) %>%
  mutate(token = str_replace(token, fixed("britishcolombia"), "british columbia")) %>%
  mutate(token = str_replace(token, fixed("brownuniversity"), "brown university")) %>%
  mutate(token = str_replace(token, fixed("mcgilluniversity"), "mcgill university")) %>%
  mutate(token = str_replace(token, fixed("astonuniversity"), "aston university")) %>%
  mutate(token = str_replace(token, fixed("westernontario"), "western ontario")) %>%
  mutate(token = str_replace(token, fixed("st.francisxavieruniversity"), "st. francis xavier university")) %>%
  mutate(token = str_replace(token, " insead", ", insead")) %>%
  mutate(token = str_replace(token, "chartered professional accountant, cpa ontario$|chartered professional accountant$|chartered professional accountant, chartered accountant, cpa, ca, canadian institute of chartered accountants$|fellow of the chartered professional accountants, chartered professional accountant, chartered accountant, fcpa, cpa, ca, $|chartered professional accountant \\(cpa/cma\\)", ";cpa;")) %>%
  mutate(token = str_remove(token, "member of the law society of upper canada|bar admission course, law society of upper canada")) %>%
  mutate(token = str_split(token, ";")) %>%
  unnest(c(token)) %>%
  mutate(token = trimws(token)) %>%
  mutate(token = str_split(token, "(?= executive program, queen's university)")) %>%
  unnest(c(token)) %>%
  mutate(token = trimws(token)) %>%
  filter(token != "") %>%
  mutate(token = str_remove(token, ", england$")) %>%
  mutate(token = str_replace(token, fixed("école nationale d'administration, paris"), fixed("école nationale d'administration (france)"))) %>%
  mutate(token = str_replace(token, fixed("london school of economics and political science"), fixed("london school of economics"))) %>%
  mutate(token = str_replace(token, fixed("aston university, birmingham, united kingdom"), fixed("aston university (united kingdom)"))) %>%
  mutate(token = str_replace(token, fixed("john hopkins school of advanced international studies"), fixed("johns hopkins university"))) %>%
  mutate(token = str_replace(token, fixed("bachelor of social science, economics university of ottawa"), fixed("bachelor of social science, economics, university of ottawa"))) %>%
  mutate(institution = str_split(token, ",")) %>%
  unnest(c(institution)) %>%
  mutate(institution = trimws(institution, whitespace = "[\\h\\v]")) %>%
  filter(institution != "") %>%
  group_by(id, date, title, token) %>%
  filter(row_number() == n()) %>% ## get last in list (the institution)
  ungroup() %>%
  mutate(token = str_replace(token, "^bachelor of law,", "bachelor of laws,")) %>%
  mutate(token = str_replace(token, "^master of law,", "master of laws,")) %>%
  mutate(token = str_replace(token, "^masters of arts,", "master of arts,")) %>%
  mutate(token = str_replace(token, "^bachelor of science with honours,", "bachelor of science,")) %>%
  mutate(token = str_replace(token, "^master of regional studies, université du québec", "master of social science, regional studies, université du québec")) %>%
  mutate(token = str_replace(token, "^graduate diploma, management of public services, université du québec", "graduate diploma, management of public services, université du québec")) %>%
  mutate(token = str_replace(token, "^master of arts, slavic languages, university of toronto", "master of arts, slavic languages and literature, university of toronto")) %>%
  mutate(token = str_replace(token, "^master's degree of environmental sciences|master's degree, environmental sciences", "master of environmental sciences")) %>%
  mutate(degree = str_split(token, ",")) %>%
  unnest(c(degree)) %>%
  mutate(degree = trimws(degree)) %>%
  filter(degree != "") %>%
  group_by(id, date, title, token) %>%
  filter(row_number() == 1) %>% ## get first in list (the degree)
  mutate(
    degree_type = case_when(
      str_detect(degree, "^bachelor|^undergrad") ~ "1st cycle / bachelor",
      str_detect(degree, "^master") ~ "2nd cycle / masters, certificate, professional",
      str_detect(degree, "^ph|^doctor") ~ "3rd cycle / doctorate",
      TRUE ~ "2nd cycle / masters, certificate, professional"
    )
  ) %>%
  mutate(
    degree_type = factor(degree_type, levels = c("1st cycle / bachelor", "2nd cycle / masters, certificate, professional", "3rd cycle / doctorate"))
  )

educ %>%
  count_group(degree) %>%
  View()


educ %>%
  ungroup() %>% select(title, token) %>% distinct() %>% count_group(token) %>% View()






backgrounder_paragraphs %>%
  left_join(backgrounders %>% select(id, url)) %>%
  select(id, url, date, title, section, token) %>%
  write_csv("data/out/backgrounder-paragraphs.csv", na = "")

educ %>%
  left_join(backgrounders %>% select(id, url)) %>%
  select(id, url, date, title, section, institution, degree, token) %>%
  write_csv("data/out/backgrounder-paragraphs--education.csv", na = "")

profexp %>%
  left_join(backgrounders %>% select(id, url)) %>%
  select(id, url, date, title, section, from, to, token) %>%
  write_csv("data/out/backgrounder-paragraphs--professional-experience.csv", na = "")

