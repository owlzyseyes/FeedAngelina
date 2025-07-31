library(RSelenium)
library(polite)
library(purrr)
library(tidyverse)
library(jsonlite)
library(mongolite)
library(dotenv)
library(wdman)
library(netstat)
library(rvest)
library(progressr)

handlers(global = TRUE)

# Note: Site says over 51K recipes exist but I hit 14K. I'm trippin in logic but eh... 
# Good enough for me.
# Start the server 
rD <- rsDriver(browser = "firefox",
               chromever = NULL,
               phantomver = NULL,
               port = free_port())

recipes_a_to_z <- "https://www.allrecipes.com/recipes-a-z-6735880#alphabetical-list-z"
cuisines_a_to_z <- "https://www.allrecipes.com/cuisine-a-z-6740455"

# Open the browser and navigate to the URL of interest
remDr <- rD$client
remDr$open()
remDr$close()
remDr$navigate(recipes_a_to_z)

# Grab page source
page_source <- remDr$getPageSource()[[1]]

# Parse with rvest
page <- read_html(page_source)

# Extract anchor tags within the alphabetical list div
links <- page |>
  html_node("#mntl-alphabetical-list_1-0") |>
  html_nodes("a")

# Build tibble of names and URLs
recipe_index <- tibble::tibble(
  name = html_text(links, trim = TRUE),
  link = html_attr(links, "href")
)

category_urls <- recipe_index$link

scrape_category_recipes <- function(url) {
  remDr$navigate(url)
  Sys.sleep(1.5)  # Allow page to fully render
  
  page <- read_html(remDr$getPageSource()[[1]])
  
  # "Featured" section
  featured_anchors <- page |>
    html_node("#mntl-three-post__inner_1-0") |>
    html_nodes("a")
  
  featured <- tibble::tibble(
    name = featured_anchors |> html_nodes("span.card__title-text ") |> html_text(trim = TRUE),
    link = featured_anchors |> html_attr("href")
  ) |>
    filter(str_detect(link, "/recipe/|recipe-[0-9]+")) |>
    mutate(type = "Featured", source_url = url)
  
  # Main recipe list across multiple divs
  main_anchors <- page |>
    html_nodes("div.comp.tax-sc__recirc-list.card-list.mntl-universal-card-list.mntl-document-card-list.mntl-card-list.mntl-block") |>
    html_nodes("a")
  
  main <- tibble::tibble(
    name = main_anchors |> html_nodes("span.card__title-text ") |> html_text(trim = TRUE),
    link = main_anchors |> html_attr("href")
  ) |>
    filter(str_detect(link, "/recipe/|recipe-[0-9]+")) |>
    mutate(type = "List", source_url = url)
  
  # Unified result
  bind_rows(featured, main)
}

# Loop through all category URLs and bind results
results <- purrr::map_dfr(category_urls, scrape_category_recipes)

# I spy duplicates
dupes <- results |> dplyr::count(link) |> dplyr::filter(n > 1)

# So I do the deduping
results <- results |> dplyr::distinct(link, .keep_all = TRUE)

mongo_uri <- Sys.getenv("MONGODB_URI")

mongo_conn <- mongo(
  collection = "Recipes",
  db = "Cook",
  url = mongo_uri
)

mongo_conn <- mongo(collection = "Recipes",
                    db = "Cook",
                    url = "mongodb+srv://admin:admin@mycluster.6mngz.mongodb.net/Cook")

scrape_recipe_details <- function(recipe_url) {
  remDr$navigate(recipe_url)
  Sys.sleep(0.5)
  
  page <- read_html(remDr$getPageSource()[[1]])
  
  # Author & date
  author <- page |> html_nodes(".mntl-attribution__item-name, 
              span.comp.mntl-bylines__item.mntl-attribution__item.mntl-attribution__item-name") |> html_text(trim = TRUE)
  date <- page |> html_node("div.mntl-attribution__item-date") |> html_text(trim = TRUE)
  
  # Ratings
  average_rating <- page |> html_node("#mm-recipes-review-bar__rating_1-0") |> html_text(trim = TRUE)
  total_ratings  <- page |> html_node("#mm-recipes-review-bar__rating-count_1-0") |> html_text(trim = TRUE)
  review_count   <- page |> html_node("#mm-recipes-review-bar__comment-count_1-0") |> html_text(trim = TRUE)
  
  # Metadata
  labels <- page |> html_nodes("div.mm-recipes-details__label") |> html_text(trim = TRUE)
  values <- page |> html_nodes("div.mm-recipes-details__value") |> html_text(trim = TRUE)
  details <- tibble::tibble(label = labels, value = values) |> tidyr::pivot_wider(names_from = label, values_from = value)
  
  # Ingredients
  ingredients <- page |> html_nodes("ul.mm-recipes-structured-ingredients__list li") |> html_text(trim = TRUE)
  
  # Nutrition
  nutrition_names <- page |> html_nodes("td.mm-recipes-nutrition-facts-summary__table-cell.text-body-100") |> html_text(trim = TRUE)
  nutrition_values <- page |> html_nodes("td.mm-recipes-nutrition-facts-summary__table-cell.text-body-100-prominent") |> html_text(trim = TRUE)
  nutrition <- tibble::tibble(fact = nutrition_names, amount = nutrition_values)
  
  # Package as tibble row
  tibble::tibble(
    url = recipe_url,
    author = author,
    date_published = date,
    ratings = list(tibble::tibble(avg = average_rating, total = total_ratings, reviews = review_count)),
    details = list(details),
    ingredients = list(ingredients),
    nutrition = list(nutrition)
  )
}

# NOTE: THIS TOOK A VERY LONG TIME TO RUN. ~13hrs
with_progress({
  p <- progressor(steps = length(results$link))
  purrr::walk(results$link, function(link) {
    try({
      recipe_df <- scrape_recipe_details(link)
      mongo_conn$insert(recipe_df)
      p(message = paste("Inserted:", link))
    }, silent = TRUE)
  })
})



# Checkpoints - fetched scraped urls from the DB
# Load scraped URLs
# scraped <- readr::read_csv("C:/Users/owlzy/Desktop/Scraped/scraped.csv")
# scraped_2 <- readr::read_csv("C:/Users/owlzy/Desktop/scraped_urls_2.csv")
# scraped_3 <- readr::read_csv("C:/Users/owlzy/Desktop/scraped_urls_3.csv")
# scraped_4 <- readr::read_csv("C:/Users/owlzy/Desktop/scraped_urls_4.csv")
# all <- readr::read_csv("C:/Users/owlzy/Desktop/all_scraped.csv")

# Filter out already scraped links from results
unscraped_df <- results[!(results$link %in% scraped$url), ]

# with_progress({
#   p <- progressor(steps = length(unscraped_df$link))
#   purrr::walk(unscraped_df$link, function(link) {
#     try({
#       recipe_df <- scrape_recipe_details(link)
#       recipe_url <- recipe_df$url[1]
#       
#       # Check if this URL already exists in the collection
#       already_scraped <- mongo_conn$count(query = sprintf('{"url": "%s"}', recipe_url)) > 0
#       
#       if (!already_scraped) {
#         mongo_conn$insert(recipe_df)
#         p(message = paste("Inserted:", link))
#       } else {
#         p(message = paste("Already scraped:", link))
#       }
#     }, silent = TRUE)
#   })
# })



