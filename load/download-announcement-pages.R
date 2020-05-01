library(rvest)

test_url <- urls %>% slice(1) %>% pull(original_url)

test_page <- read_html(test_url)



process_announcement_page <- function(page_to_process) {
  page_title <- page_to_process %>%
    html_nodes("h1.page-header") %>%
    html_text
  
  announcement_content <- page_to_process %>%
    html_node("article.full-article")
  
  announcement_text <- announcement_content %>%
    html_text
  
  backgrounder_link_elems <- announcement_content %>%
    html_nodes(xpath = ".//a[contains(@href, 'backgrounder')]")
  
  backgrounder_link_names <- backgrounder_link_elems %>%
    html_text
  
  backgrounder_link_urls <- backgrounder_link_elems %>%
    html_attr("href")
  
  return(
    tibble(
      title = page_title,
      text = announcement_text,
      names = backgrounder_link_names,
      urls = backgrounder_link_urls
    ) %>%
    nest(backgrounder_links = c(names, urls))
  )
}

process_announcement_page(test_page)
