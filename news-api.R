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

# Ex. tesla 
  # sort <- "publishedAt"
  # keyword <- "tesla"
  # ### A - didn't work
  # url <- glue(
  #   "https://newsapi.org/v2/everything?&q={keyword}&from={start_date}&sortBy={sort}&apiKey={key}"
  # )
  # ### B - works. Same thing, just less glue()
  # url <- glue(
  #   "https://newsapi.org/v2/everything?q=tesla&from={start_date}&sortBy=publishedAt&apiKey={key}"
  # )



# this httr request shouldn't work
keyword <- "tesla"
sort <- "publishedAt"
broken_url <- glue(
  "https://newsapi.org/v2/everything?&q={keyword}&from={start_date}&sortBy={sort}&apiKey={key}"
)

# let's see how function handles it
create_article_results(broken_url)
# yes!!
# # A tibble: 1 × 8
# source author title description content url   url_image date 
# <chr>  <chr>  <chr> <chr>       <chr>   <chr> <chr>     <chr>
#   1 ""     ""     ""    ""          ""      ""    ""        ""   


# this is also working - great!
broken_url |> 
  create_article_results() |> 
  display_prep() |> 
  news_reactable()
# [1] "No results returned for keyword"


# 1. import ----


create_article_results <- function(url) {
  #' @title Create Article Results
  #' 
  #' @description 
  #' Query NewsAPI and save results
  #' The request is executed using httr2 and results are parsed with jsonlite.
  #' Output is saved as an RDS file.
  #' 
  #' @param url (str)
  #' Request URL with specified parameters
  #' Ex: glue("https://newsapi.org/v2/everything?q=ai&from={start_date}&sortBy=publishedAt&apiKey={key}")
  #'
  #' @return 
  #' Dataframe with API results
  
  
  # Create a request object
  request <- request(url)
  
  response <- request %>%
    req_perform()
  
  # Check status
  if (response$status_code == 200) {
    message("GOOD - Status 200") 
  } else {
    stop("Status not 200 OK")
  }
  
  news_raw <- response %>%
    resp_body_json()
  
  if (news_raw$totalResults >= 10) {
    message("GOOD - At least 10 results")
  } else {
    stop("Fewer than 10 results")
  }
  
  # Convert json to dataframe
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
  
  articles_list <- purrr::map(
    .x = news_raw$articles,
    .f = ~ extract_elements(.x)
  )
  
  articles_table <- bind_rows(articles_list)
  
  message(glue::glue(
    "{nrow(articles_table)} records returned"
  ))
  
  articles_table
  
}  


# 1. Popular
key <- Sys.getenv('NEWS_API_KEY')
end_date <- today() - ((wday(today()) %% 7)) # latest Saturday
start_date <- end_date - 6 # preceding Sunday

pop_url <- glue(
  "https://newsapi.org/v2/top-headlines?country=us&category=technology&from={start_date}&to={end_date}&sortBy=popularity&apiKey={key}"
)

# saveRDS(create_article_results(pop_url), "data/articles_popular.RDS")
articles_popular <- readRDS("data/articles_popular.RDS")


# 2. DOGE or DEI
doge_url <- glue(
  "https://newsapi.org/v2/everything?q=doge&from={start_date}&sortBy=popularity&language=en&apiKey={key}"
)
# saveRDS(create_article_results(doge_url), "data/articles_doge.RDS")
articles_doge <- readRDS("data/articles_doge.RDS")

# 3. Cybersecurity 
cyber_url <- glue(
  "https://newsapi.org/v2/everything?q=cybersecurity&from={start_date}&sortBy=popularity&language=en&apiKey={key}"
)
# saveRDS(create_article_results(cyber_url), "data/articles_cyber.RDS")
articles_cyber <- readRDS("data/articles_cyber.RDS")

# 4. AI
openai_url <- glue(
  "https://newsapi.org/v2/everything?q=openai&from={start_date}&sortBy=popularity&language=en&apiKey={key}"
)
# saveRDS(create_article_results(openai_url), "data/articles_ai2.RDS")
articles_openai <- readRDS("data/articles_ai2.RDS")

  
# 2. prep for display ----

  
# articles_prep <- articles_table |> 
#   mutate(display_preview = 
#            case_when(
#              description == "" & content == "" ~ "No preview",
#              description == "" ~ content,
#              content == "" ~ description, 
#              T ~ description
#            ))  
#   
# 
# render_image <- function(url) {
#   as.character(tags$img(src = url, height = '100px'))
# }
# 
# articles_image <- articles_prep |>
#   rowwise() |> 
#   mutate(url_image_display = render_image(url_image))
# 
# articles_image <- articles_image |> 
#   mutate(read_more = sprintf(
#     paste0('<a href="', 
#            URLdecode(url),
#            '" target="_blank">', 
#            'Read more',
#            '</a>')
#   ))
# 
# articles_short <- articles_image |> 
#   # Replace "Read more" text with icon
#   mutate(read_more = str_replace(read_more,
#                                  'Read more',
#                                  '&#x1F517;')) |>
#   # Sneak in some inline styling because CSS isn't working...
#   mutate(read_more = str_replace(
#     read_more,
#     ' target=',
#     ' style="text-decoration:none" target ='
#   )) |> 
#   select(
#     `Image` = url_image, # plain text link
#     `Title` = title,
#     `Preview` = display_preview,
#     `Link` = read_more,
#     url
#   ) 
# 
# articles_short$Rank <- seq(1:nrow(articles_short))
# 
# articles_short <- articles_short |> 
#   select(Rank, everything())

### 1. Image render
render_image <- function(url) {
  as.character(tags$img(src = url, height = '100px'))
}


### 2. Everything else
display_prep <- function(tab) {
  
  articles_preview <- tab |> 
    mutate(display_preview = 
             case_when(
               description == "" & content == "" ~ "No preview",
               description == "" ~ content,
               content == "" ~ description, 
               T ~ description
             ))  
  
  articles_image <- articles_preview |>
    rowwise() |> 
    mutate(url_image_display = render_image(url_image))
  
  articles_hyperlink <- articles_image |> 
    # Hyperlink with icon
    mutate(read_more = sprintf(
      paste0('<a href="', 
             URLdecode(url),
             '" target="_blank">', 
             '&#x1F517;',
             '</a>')
    )) |> 
    # Sneak in some inline styling because CSS isn't working...
    mutate(read_more = str_replace(
      read_more,
      ' target=',
      ' style="text-decoration:none" target ='
    )) |> 
    select(
      `Image` = url_image, # plain text link
      `Title` = title,
      `Preview` = display_preview,
      `Link` = read_more, # active link
      url
    ) 

  articles_dedup <- articles_hyperlink |> 
    group_by(url) |> # some duplicates
    slice(1) |> 
    ungroup() |> 
    head(20)
  
  articles_dedup$Rank <- seq(1:nrow(articles_dedup))
  
  articles_ordered <- articles_dedup |> 
    select(Rank, everything())
  
  articles_ordered
  
}


articles_doge |> 
  display_prep() |> 
  head(20) 


# 3. reactable ----

### colors
blue <- '#4368B6'
green <- '#78A153' 
yellow <- '#DEC23B' 
orange <- '#E4930A' 
red <- '#C53211' 
gray <- '#64605f'
black <- '#242424'
tan <- '#fffdf5'

### tippy
with_tooltip <- function(value, tooltip) {
  tags$abbr(
    style = "cursor: help",
    title = tooltip, 
    value)
}

### function
news_reactable <- function(display_data) {
  
  display_data |> 
    reactable(
      columns = list(
        
        Rank = colDef(maxWidth = 75, 
                      vAlign = "center",
                      header = with_tooltip(
                        "Rank", "Popularity ranking according to NewsAPI"
                      )
        ),
        
        Image = colDef(
          maxWidth = 300,
          align = "center",
          vAlign = "center",
          cell = function(value) {
            tags$img(src = value, height = "100px")
          },
          header = with_tooltip(
            "Image", "Publication image, if available"
          )
        ),
        
        # Transform title to include Preview underneath it  
        Title = colDef(
          name = "Title / Preview",
          vAlign = "center",
          cell = function(value, index) {
            Preview <- display_data$Preview[index]
            div(
              div(style = list(fontWeight = 600, fontSize = "24px"), value),
              div(style = list(fontSize = "16px"), Preview)
            )
          },
          header = with_tooltip(
            "Title", "Article title and description"
          )
        ),
        
        Link = colDef(html = T, maxWidth = 75, vAlign = "center",
                      header = with_tooltip(
                        "Link", "Link to full article in new window"
                      )
        ),
        
        Preview = colDef(show = F),
        url = colDef(show = F),
        Image = colDef(show = F, vAlign = "center")
      ),
      highlight = T,
      pagination = FALSE,
      striped = T,
      outlined = F,
      theme = reactableTheme(
        stripedColor = tan,
        backgroundColor = '#F5F2E7',
        highlightColor = '#e3e1dc'
      )
    )
  
}


# 4. test ----


articles_popular |> 
  display_prep() |> 
  news_reactable()

articles_doge |> 
  display_prep() |> 
  news_reactable()

articles_cyber |> 
  display_prep() |> 
  news_reactable()

articles_openai |> 
  display_prep() |> 
  news_reactable()



