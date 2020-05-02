
announcements %>%
  select(id, date, text) %>%
  unnest_tokens(token, text, token = "sentences") %>%
  mutate(token = str_remove_all(token, "the prime minister, justin trudeau, today announced the following changes{,1} in the senior ranks of the public service:")) %>%
  mutate(token = trimws(token)) %>%
  filter(token != "")



the prime minister, justin trudeau, today announced the following changes in the senior ranks of the public service: malcolm brown, currently special advisor to the clerk of the privy council on the syrian refugee initiative, becomes deputy minister of public safety, effective april 4, 2016.
