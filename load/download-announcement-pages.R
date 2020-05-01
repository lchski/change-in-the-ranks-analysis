library(rvest)

test_url <- urls %>% slice(1) %>% pull(original_url)

test_page <- read_html(test_url)

page_title <- test_page %>%
  html_nodes("h1.page-header") %>%
  html_text

announcement_content <- test_page %>%
  html_node("article.full-article")

announcement_text <- announcement_content %>%
  html_text

backgrounder_links <- announcement_content %>%
  html_nodes(xpath = ".//a[contains(@href, 'backgrounder')]")

