library(rvest)
library(xml2)
library(fs)

## Check if URL in storage, download and save if not.
retrieve_page_at_url <- function(url_to_scrape, scrape_waiting_period = 1) {
  file_to_return <- NULL

  # convert accented characters to non-accented
  url <- str_replace_all(url_to_scrape, c(
    "é|è" = "e",
    "ç" = "c"
  ))

  file_path <- url %>% convert_url_to_filename

  file_is_already_downloaded <- file_exists(file_path)
  
  if (file_is_already_downloaded) {
    message(
      paste0(
        "Already downloaded",
        "\n\t url = ", url
      )
    )

   file_to_return = read_html(file_path)
  } else {
    tryCatch(
      {
        Sys.sleep(scrape_waiting_period)

        file_to_return = read_html(url)

        file_to_return %>% write_html(file_path)

        message(
          paste0(
            "Downloaded",
            "\n\t url = ", url
          )
        )
      },
      error = function(c) {
        message(
          paste0(
            "Got an error when trying to read_html a page",
            "\n\t url = ", url,
            "\n\t error = ", c
          )
        )
      }
    )
  }

  if (is_null(file_to_return)) {
    message(
      paste0(
        "Could not retrieve a page\n\t ",
        "url = ", url
      )
    )

    return(list())
  }

  return(file_to_return)
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

process_article_page <- function(page_to_process, article_identifier = "article.full-article") {
  page_title <- page_to_process %>%
    html_nodes("h1.page-header") %>%
    html_text
  
  if (length(page_title) == 0) {
    page_title <- page_to_process %>%
      html_node("div.title-header-inner > h1") %>%
      html_text
  }

  article_content <- page_to_process %>%
    html_node(article_identifier)
  
  article_text <- article_content %>%
    html_node(".content-news-article") %>%
    html_text # TODO: consider using html_text2

  return(
    tibble(
      title = page_title,
      text = article_text
    )
  )
}

extract_backgrounder_links <- function(page_to_process) {
  content_container <- page_to_process

  backgrounder_link_elems <- content_container %>%
    html_nodes(xpath = ".//a[contains(@href, 'backgrounder')]")
  
  if (length(backgrounder_link_elems) == 0) {
    ## avoid `character(0)` situations which break the tibble
    backgrounder_link_names = NA_character_
    backgrounder_link_urls = NA_character_
  } else {
    backgrounder_link_names <- backgrounder_link_elems %>%
      html_text
    
    backgrounder_link_urls <- backgrounder_link_elems %>%
      html_attr("href")
  }
  
  return(
    tibble(
      name = backgrounder_link_names,
      url = backgrounder_link_urls
    )
  )
}
