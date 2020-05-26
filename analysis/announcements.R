
announcement_sentences_raw <- announcements %>%
  select(id, date, text) %>%
  unnest_tokens(token, text, token = "sentences") %>%
  mutate(token = str_remove_all(token, "the prime minister, justin trudeau, today announced the following changes{0,1} in the senior ranks of the public service:")) %>%
  mutate(token = trimws(token)) %>%
  filter(token != "")

## recombine fragmented sentences mistakenly broken on abbreviations like Dr., Ms., Mr., or middle initials
## note: only works for sentences with one abbreviation, preserving the last in sentence; "Dr. FNAME I. LNAME" becomes "FNAME I. LNAME"
## this is very rare, though. to find instances of two in a row, run this before token mutations: `filter(ends_in_abbreviation & lag(ends_in_abbreviation))`
announcement_sentences <- announcement_sentences_raw %>%
  mutate(ends_in_abbreviation = str_detect(token, " [a-z]{1,3}\\.$|^[a-z]{1,3}\\.$")) %>%
  mutate(token_fixed = ifelse(
    ends_in_abbreviation,
    paste(token, lead(token)),
    token
  )) %>%
  mutate(token = ifelse(lag(ends_in_abbreviation), lag(token_fixed), token)) %>%
  mutate(token = ifelse(row_number() == 1, token_fixed, token)) %>%
  filter(! ends_in_abbreviation) %>%
  select(id:token) %>%
  filter(
    ! token %in% (
      (.) %>%
        count_group(token) %>%
        filter(count > 1) %>%
        pull(token)
    )
  ) %>% ## filter out sentences that are duplicated across entries (e.g., "biographical notes"), we only want unique ones
  filter(! str_detect(token, "biographical note")) %>%
  filter(! str_detect(token, "[a-z]$")) %>% ## remove sentences that end with just a word (usually indicates an error/weird markup)
  mutate(
    describes_role = str_detect(token, "becomes|been elevated|will undertake|will take on|now serves|will serve|will also serve|will assume"),
    describes_retirement = str_detect(token, "retire|took the opportunity")
  ) ## use `filter(! describes_role & ! describes_retirement)` to find others (which may be misses)


announcement_sentences %>%
  mutate(ifelse(## fix a sentence that doesn't fit the model
    str_detect(token, "^annette gibbons, assistant secretary to the cabinet"),
    "annette gibbons, currently assistant secretary to the cabinet",
    token
  )) %>%
  group_by(id) %>%
  mutate(sentence_id = row_number()) %>%
  group_by(id, sentence_id) %>%
  filter(describes_role) %>%
  separate(token, into = c("name", "current_role"), "currently") %>%
  separate(current_role, into = c("current_role", "new_role"), "becomes|will take on additional responsibilities as") %>%
  mutate_at(vars(name, current_role, new_role), trimws) %>%
  mutate_at(vars(name, current_role, new_role), ~ str_remove(.x, ",$"))

announcement_sentences %>%
  left_join(announcements %>% select(id, url)) %>%
  select(id, url, everything()) %>%
  write_csv("data/out/announcement-sentences.csv", na = "")
