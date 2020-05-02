library(rvest)
library(fs)

test_url <- urls %>% slice(1) %>% pull(original_url)

test_page <- read_html(test_url)



## Check if URL in storage, download and save if not.
retrieve_page_at_url <- function(url, redownload = FALSE) {
  file_path <- url %>% convert_url_to_filename
  
  file_already_downloaded <- file_exists(file_path)
  
  if (file_already_downloaded) {
    return(read_html(file_path))
  }
  
  ## TODO: the rest... also `redownload`
}

convert_url_to_filename <- function(url) {
  url %>%
    str_replace_all(fixed("/"), fixed("SLASH")) %>%
    str_replace_all(fixed(":"), fixed("COLON")) %>%
    path("data/source/downloaded-pages", ., ext = "html")
}

convert_filename_to_url <- function(filename) {
  filename %>%
    str_remove(fixed("data/source/downloaded-pages/")) %>%
    str_remove(fixed(".html")) %>%
    str_replace_all(fixed("SLASH"), fixed("/")) %>%
    str_replace_all(fixed("COLON"), fixed(":"))
}


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
