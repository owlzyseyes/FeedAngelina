library(rvest)
library(polite)
library(dplyr)
library(purrr)
library(future)
library(furrr)
library(mongolite)
library(dotenv)

# Load environment variables
dotenv::load_dot_env()

session <- polite::bow("https://www.allrecipes.com/recipes-a-z-6735880")
page <- polite::scrape(session)

# Extract all category links
category_links <- page %>%
  rvest::html_nodes(".mntl-alphabetical-list__group a") %>%
  rvest::html_attr("href") %>%
  unique()

# Helper function to get recipes from each category
extract_recipes <- function(url) {
  cat("Scraping:", url, "\n")
  session <- polite::bow(url, user_agent = "rvest_bot/0.1")
  page <- try(polite::scrape(session), silent = TRUE)
  if (inherits(page, "try-error")) return(NULL)
  
  # Featured 3
  featured <- page %>%
    rvest::html_node("#mntl-three-post__inner_1-0") %>%
    rvest::html_nodes("a") %>%
    {
      tibble::tibble(
        name = rvest::html_text(., trim = TRUE),
        link = rvest::html_attr(., "href"),
        type = "Featured"
      )
    }
  
  # Full list
  more <- page %>%
    rvest::html_node("#mntl-taxonomysc-article-list-group_1-0") %>%
    rvest::html_nodes("a") %>%
    {
      tibble::tibble(
        name = rvest::html_text(., trim = TRUE),
        link = rvest::html_attr(., "href"),
        type = "List"
      )
    }
  
  dplyr::bind_rows(featured, more) %>%
    dplyr::mutate(category_url = url)
}

future::plan(multisession, workers = 8)
# Step 3: Scrape all categories
all_recipes <- future_map_dfr(
  category_links,
  extract_recipes,
  .progress = TRUE,
  .options = furrr_options(seed = TRUE)
)

# Preview
dplyr::glimpse(all_recipes)




# Now extract ingredients for each recipe
# Set up parallelism
plan(multisession, workers = 8)  # adjust based on your system's number of cores

mongo_uri <- Sys.getenv("MONGODB_URI")

mongo_conn <- mongo(
  collection = "Recipes",
  db = "Project",
  url = mongo_uri
)



# Run this if you intend to save data locally
extract_ingredients <- function(name, link, type, category_url) {
  cat("Scraping:", name, "\n")
  
  session <- bow(link, user_agent = "rvest_bot/0.1")
  page <- try(scrape(session), silent = TRUE)
  if (inherits(page, "try-error")) return(NULL)
  
  ingredients <- page %>%
    html_node("#mm-recipes-structured-ingredients_1-0") %>%
    html_nodes(".mm-recipes-structured-ingredients__list li") %>%
    html_text(trim = TRUE)
  
  if (length(ingredients) == 0) return(NULL)
  
  tibble(
    recipe_name = name,
    recipe_url = link,
    type = type,
    category_url = category_url,
    ingredients = list(ingredients)  # stores as list column
  )
}

ingredients <- all_recipes %>%
  future_pmap_dfr(extract_ingredients, .progress = TRUE, .options = furrr_options(seed = TRUE))



# Run this if intention is to stream to MongoDB
extract_ingredients_stream <- function(name, link, type, category_url) {
  cat("Scraping:", name, "\n")
  
  session <- bow(link, user_agent = "rvest_bot/0.1")
  page <- try(scrape(session), silent = TRUE)
  if (inherits(page, "try-error")) return(NULL)
  
  ingredients <- page %>%
    html_node("#mm-recipes-structured-ingredients_1-0") %>%
    html_nodes(".mm-recipes-structured-ingredients__list li") %>%
    html_text(trim = TRUE)
  
  if (length(ingredients) == 0) return(NULL)
  
  # Build record and insert it directly
  doc <- list(
    recipe_name = name,
    recipe_url = link,
    type = type,
    category_url = category_url,
    ingredients = ingredients  # already a vector, maps to array in BSON
  )
  
  mongo_conn$insert(doc)
  return(NULL)  # No need to return a tibble now
}

ingredients_stream <- all_recipes %>%
  future_pmap_dfr(extract_ingredients_stream, .progress = TRUE, .options = furrr_options(seed = TRUE))

