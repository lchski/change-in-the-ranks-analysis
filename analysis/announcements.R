
announcements %>%
  select(id, date, text) %>%
  unnest_tokens(token, text, token = "sentences") %>%
  mutate(token = str_remove_all(token, "the prime minister, justin trudeau, today announced the following changes{0,1} in the senior ranks of the public service:")) %>%
  mutate(token = trimws(token)) %>%
  filter(token != "")
