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
  select(id, date, name_full = title, text) %>%
  unnest_tokens(token, text, token = "regex", pattern = "\n") %>%
  mutate(token = trimws(token, whitespace = "[\\h\\v]")) %>%
  filter(token != "") %>%
  mutate(section = case_when(
    str_detect(token, "^education$") ~ "education",
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
  filter(token != "") %>%
  rename(position = token)

profexp

# position standardize / department notes:
# - we can standardize, but we want to know what the position was (e.g., "deputy minister" etc) -- may be better to break it down, into, e.g., "position" (extract most senior of, DM, AssocDM, AsstDM, Director, etc etc), "organization"
# - we can put in a department, but trickier for, e.g., "Chief Information" (which would be both across depts and at TBS)

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
  mutate(token = str_replace_all(token, c(
    "école nationale d'administration, paris" = "école nationale d'administration (france)",
    "london school of economics and political science" = "london school of economics",
    "aston university, birmingham, united kingdom" = "aston university (united kingdom)",
    "john hopkins school of advanced international studies" = "johns hopkins university",
    "bachelor of social science, economics university of ottawa" = "bachelor of social sciences, economics, university of ottawa",
    "bachelor of arts economic and social studies, university of east anglia" = "bachelor of arts, economic and social studies, university of east anglia",
    "bachelor of political science, university of ottawa" = "bachelor of social sciences, political science, university of ottawa",
    "bachelor of political science, mcgill university" = "bachelor of arts, political science, mcgill university",
    "bachelor of political science and journalism, carleton university" = "bachelor of arts, political science and journalism, carleton university",
    "bachelor of economics, university of ottawa" = "bachelor of social sciences, economics, university of ottawa",
    "^master of economics," = "master of arts, economics,",
    "bachelor of psychology, university of ottawa" = "bachelor of science, psychology, university of ottawa",
    "of of" = "of",
    "bachelor of history," = "bachelor of arts, history,",
    "bachelor of arts, political studies, queen's university" = "bachelor of arts, political science, queen's university"
  ))) %>%
  mutate(institution = str_split(token, ",")) %>%
  unnest(c(institution)) %>%
  mutate(institution = trimws(institution, whitespace = "[\\h\\v]")) %>%
  filter(institution != "") %>%
  group_by(id, date, name_full, token) %>%
  filter(row_number() == n()) %>% ## get last in list (the institution)
  ungroup() %>%
  mutate(token = str_replace_all(token, c(
    "^bachelor of law," = "bachelor of laws,",
    "^master of law," = "master of laws,",
    "^masters of arts," = "master of arts,",
    "^bachelor of science with honours," = "bachelor of science,",
    "^master of regional studies, université du québec" = "master of social science, regional studies, université du québec",
    "^graduate diploma, management of public services, université du québec" = "graduate diploma, management of public services, université du québec",
    "^master of arts, slavic languages, university of toronto" = "master of arts, slavic languages and literature, university of toronto",
    "^master's degree of environmental sciences|master's degree, environmental sciences" = "master of environmental sciences"
  ))) %>%
  mutate(degree = str_split(token, ",")) %>%
  unnest(c(degree)) %>%
  mutate(degree = trimws(degree)) %>%
  filter(degree != "") %>%
  group_by(id, date, name_full, token) %>%
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
  ) %>%
  ungroup() %>%
  mutate(subject = str_split(token, ",")) %>%
  unnest(c(subject)) %>%
  mutate(subject = trimws(subject)) %>%
  filter(subject != "") %>%
  group_by(id, date, name_full, token) %>%
  filter(row_number() == 2) %>% # get second in list (subject, presumably, though likely not always)
  mutate(subject = case_when(
    subject == institution ~ NA_character_,
    TRUE ~ subject
  )) %>%
  mutate(subject = case_when(
    is.na(subject) & str_detect(degree, "law") ~ "law",
    is.na(subject) & str_detect(degree, "public policy|policy analysis") ~ "public policy",
    is.na(subject) & str_detect(degree, "commerce") ~ "commerce",
    is.na(subject) & str_detect(degree, "environmental science") ~ "environmental science",
    is.na(subject) & str_detect(degree, "veterinary|animal biotechnology") ~ "veterinary science / medicine",
    is.na(subject) & str_detect(degree, "pharm") ~ "pharmacy / pharmacology",
    is.na(subject) & str_detect(degree, "business administration") ~ "business administration",
    is.na(subject) & str_detect(degree, "public administration") ~ "public administration",
    is.na(subject) & str_detect(degree, "urban planning") ~ "urban planning",
    is.na(subject) & str_detect(degree, "computer science") ~ "computer science",
    TRUE ~ subject
  ))

educ %>% filter(is.na(subject)) %>% View("no subject")
educ %>% filter(! is.na(subject)) %>% View("has subject")
educ %>% ungroup %>% count(degree) %>% View("degree")
educ %>% ungroup %>% count(subject) %>% View("subject")

educ %>%
  ungroup %>%
  count(degree, sort = TRUE) %>%
  View()


educ %>%
  ungroup() %>% select(name_full, token) %>% distinct() %>% count(token) %>% View()


educ_deduped <- educ %>%
  ungroup %>%
  distinct(name_full, token, institution, degree, degree_type, subject)


profexp_deduped <- profexp %>%
  ungroup %>%
  group_by(name_full) %>%
  distinct(position, from, to) %>%
  select(name_full, everything()) %>%
  arrange(name_full, -to)

profexp %>%
  ungroup %>%
  group_by(name_full) %>%
  distinct(position, from, to) %>%
  select(name_full, everything()) %>%
  arrange(name_full, -to) %>% ungroup %>% count(position, sort = TRUE)




backgrounder_paragraphs %>%
  left_join(backgrounders %>% select(id, url)) %>%
  select(id, url, date, name_full, section, token) %>%
  write_csv("data/out/backgrounder-paragraphs.csv", na = "")

educ %>%
  left_join(backgrounders %>% select(id, url)) %>%
  select(id, url, date, name_full, section, institution, degree, token) %>%
  write_csv("data/out/backgrounder-paragraphs--education.csv", na = "")

profexp %>%
  left_join(backgrounders %>% select(id, url)) %>%
  select(id, url, date, name_full, section, from, to, token) %>%
  write_csv("data/out/backgrounder-paragraphs--professional-experience.csv", na = "")

