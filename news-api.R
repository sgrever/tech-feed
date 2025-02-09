# Practice working with News API here 
# https://newsapi.org/docs/get-started



# Tests with dates:
  # test_dates <- as.Date(paste0("2025-02-0", c(2:8)))
  # wday(test_dates)
  # # [1] 1 2 3 4 5 6 7
  # 
  # test_function <- function(x) {
  #   x - ((wday(x) - 1)) <--- SUNDAY
  # } 
  # 
  # purrr::map_vec(test_dates, .f = ~test_function(.x))
  # # [1] "2025-02-02" "2025-02-02" "2025-02-02" "2025-02-02"
  # # [5] "2025-02-02" "2025-02-02" "2025-02-02"
  # 
  # test_function <- function(x) {
  #   x - ((wday(x) %% 7)) - 6 <--- SATURDAY
  # }
  # 
  # purrr::map_vec(test_dates, .f = ~test_function(.x))


# Establish parameters and build API string
# Category options: https://newsapi.org/docs/endpoints/top-headlines
category <- "technology" 
key <- Sys.getenv('NEWS_API_KEY')
end_date <- today() - ((wday(today()) %% 7)) # latest Saturday
start_date <- end_date - 6 # preceding Sunday

base_url <- glue(
  "https://newsapi.org/v2/top-headlines?country=us&category={category}&from={start_date}&to={end_date}&sortBy=popularity&apiKey={key}"
)

# Create a request object
request <- request(base_url)

response <- request %>%
  req_perform()

news_raw <- response %>%
  resp_body_json()

extract_elements <- function(x) {
  data.frame(
    source = ifelse(is.null(x$source$name), "", x$source$name),
    author = ifelse(is.null(x$author), "", x$author),
    title = ifelse(is.null(x$title), "", x$title),
    description = ifelse(is.null(x$description), "", x$description),
    content = ifelse(is.null(x$content), "", x$content),
    url = ifelse(is.null(x$url), "", x$url),
    url_image = ifelse(is.null(x$urlToImage), "", x$urlToImage),
    date = ifelse(is.null(x$publishedAt), "", x$publishedAt)
  )
}
extract_elements(news_raw$articles[[1]])


articles_list <- purrr::map(
  .x = news_raw$articles,
  .f = ~ extract_elements(.x)
)

articles_table <- bind_rows(articles_list)

# saveRDS(articles_table, "data/articles_table.RDS")



