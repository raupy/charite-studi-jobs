library(httr)
library(rvest)
library(xml2)
library(dplyr)
library(stringr)
library(purrr)
library(lubridate)
library(assertive)


map_p_tags <- function(links, date_regex) {
    p_tags_text <- links %>%
      html_nodes("p") %>%
      html_text()
    date_indices <- p_tags_text %>%
      str_detect(date_regex) %>%
      which()
    if (length(date_indices) == 0) {
      res <- "no dates"
    } else {
      split_list <- split(date_indices, ceiling(seq_along(date_indices) / 2))
      res <- map(map(split_list, ~ .x[1]:.x[2]), ~ p_tags_text [.x])
    }
    res
}

extract_features <- function(mapped_p_tags, date_regex) {
  cleaned_text <- mapped_p_tags %>%
    map(~ str_remove(.x, "Zum Stellenangebot")) %>%
    map(~ str_remove(.x, "Bewerbungsfrist:")) %>%
    map(~ str_trim(.x))
  dates <- map(cleaned_text, ~ str_extract_all(.x, date_regex)) %>%
    unlist() %>%
    dmy()
  odds <- seq(1, length(dates), 2)

  cleaned_text  <- cleaned_text %>%
    map(~ str_remove_all(.x, date_regex)) %>%
    map_chr(~ paste0(.x, collapse = ": "))

  tibble(institut = cleaned_text,
                           eingestellt = dates[odds],
                           bewerbungsfrist = dates[-odds]) %>%
    mutate(institut = ifelse(str_detect(institut, "[a-z]"),
                             str_remove_all(institut, "-") %>%
                               map_chr(~ str_remove(.x, ":")) %>%
                               map_chr(~ str_sub(.x, end = -3)) %>%
                               map_chr(~ str_trim(.x)),
                             "-"))
}

find_list_jobs <- function(url, search_string) {
  resp <- GET(url)
  assert_are_set_equal(resp$status_code, 200)
  html <- read_html(url)
  links <- html %>% html_nodes(".list-jobs") %>% html_nodes("li a")
  html_text <- links %>% html_text()
  search_result <- which(str_detect(html_text, search_string))
  jobs_tbl <- tibble()
  if (length(search_result) > 0) {
    results <- links[search_result]
    date_regex <- "(0[1-9]|[12][0-9]|3[01]).(0[1-9]|1[0-2]).\\d{4}"
    urls <- results %>% html_attr("href")
    urls <- paste0("https://www.charite.de", urls)
    mapped_p_tags <- map_p_tags(results, date_regex)
    if (class(mapped_p_tags) == "character") {
      jobs_tbl <- tibble(institut = html_text,
                         eingestellt = dmy(NA),
                         bewerbungsfrist = dmy(NA),
                         url = urls)
    } else {
      jobs_tbl <- mapped_p_tags %>%
        extract_features(date_regex) %>%
        mutate(url = urls)
    }
  }
  jobs_tbl
}

get_sites_to_crawl <- function(url) {
  resp <- GET(url)
  assert_are_set_equal(resp$status_code, 200)
  number_of_results_text <- read_html(url) %>%
    html_node(".tx-indexedsearch-browsebox") %>%
    html_nodes("p") %>%
    html_text()
  number_of_results_vec <- str_extract_all(
    number_of_results_text,
    "\\(?[0-9,.]+\\)?")[[1]] %>%
    as.numeric()
  if (length(number_of_results_vec) == 0) {
    warning("The length of number_of_results_vec is 0.
            Cannot find out the number of sites to crawl.")
    sites <- url
  } else {
    assert_are_set_equal(length(number_of_results_vec), 3)
    assert_is_not_null(number_of_results_vec[2])
    number_of_pages <- as.integer(number_of_results_vec[3] /
                                    number_of_results_vec[2])
    sites <- c(url, paste0(url, "seite/") %>%
                 paste0(1:number_of_pages))
  }
  sites
}

get_all_jobs <- function(urls, search_string) {
  map_dfr(urls, find_list_jobs, search_string)
}


url <- "https://www.charite.de/karriere/stellenboerse/"
search_string <- "STUD"
urls <- get_sites_to_crawl(url)
charite_jobs <- get_all_jobs(urls, search_string)
