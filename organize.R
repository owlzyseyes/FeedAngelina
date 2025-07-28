library(jsonlite)
library(tidyverse)
library(purrr)
library(janitor)

# Fetched from DB
recipes <- jsonlite::fromJSON("data/Project.Njaa.json")

# Select necessary columns needed for join
recipe_names_links <- results |> 
  select(name, link) |> 
  rename(url = link)

# Supplement the recipes with their names
recipes_joined <- recipes |> 
  dplyr::left_join(recipe_names_links, by = "url") |> 
  dplyr::relocate(name, before = url) |> 
  dplyr::rename(url = before)


# Start by safely unpacking each rating
ratings_df <- recipes_joined |>
  select(url, ratings) |>
  mutate(
    ratings = map(
      ratings,
      ~ if (is.null(.x) || ncol(.x) == 0) {
        tibble(avg = NA, total = NA, reviews = NA)
      } else {
        .x
      }
    )
  ) |>
  unnest(ratings)

ratings_df <- ratings_df |>
  mutate(
    avg_rating = as.numeric(sub(" out of 5", "", avg)),  # extract number before text
    n_ratings = as.numeric(gsub("[^0-9]", "", total)),  # remove non-digits
    n_reviews = as.numeric(gsub("[^0-9]", "", reviews))  # same for reviews
  ) |>
  select(-avg, -total, -reviews)  # drop original columns


# Count using flexible matching
target_fields <- c("Prep Time", "Cook Time", "Total Time", "Servings")

# Helper to match roughly based on substring
has_field <- function(detail, keyword) {
  any(grepl(keyword, names(detail), ignore.case = TRUE)) &&
    detail[[grep(keyword, names(detail), ignore.case = TRUE)[1]]] != ""
}

# Count each timing field approximately
timing_counts <- sapply(target_fields, function(field) {
  sum(sapply(recipes_joined$details, function(detail) has_field(detail, field)))
})

names(timing_counts) <- target_fields
timing_counts

# Flexible value extractor
get_field_value <- function(detail, keyword) {
  idx <- grep(keyword, names(detail), ignore.case = TRUE)
  if (length(idx) > 0) {
    val <- detail[[idx[1]]]
    if (!is.null(val) && val != "") return(val)
  }
  return(NA)
}

# Build the updated frame
recipe_timings_df <- data.frame(
  name         = recipes_joined$name,
  url          = recipes_joined$url,
  prep_time    = sapply(recipes_joined$details, get_field_value, keyword = "Prep Time"),
  cook_time    = sapply(recipes_joined$details, get_field_value, keyword = "Cook Time"),
  total_time   = sapply(recipes_joined$details, get_field_value, keyword = "Total Time"),
  servings     = sapply(recipes_joined$details, get_field_value, keyword = "Servings"),
  stringsAsFactors = FALSE
)

# Tidyin up the ingredients
ingredients_flat <- map_chr(recipes_joined$ingredients, ~ paste(.x, collapse = ", "))

recipes_joined <- recipes_joined |>
  mutate(ingredients_text = ingredients_flat)

# Now dealing with the nutrition facts
nutrition_df <- map_dfr(recipes_joined$nutrition, function(x) {
  # Handle null or empty nutrition
  if (is.null(x) || nrow(x) == 0) {
    return(tibble(calories = NA, fat = NA, carbs = NA, protein = NA))
  }
  
  # Standardize and reshape wide
  x_wide <- x |>
    mutate(fact = tolower(fact)) |>
    pivot_wider(names_from = fact, values_from = amount)
  
  # Extract known fields
  tibble(
    calories = if ("calories" %in% names(x_wide)) x_wide$calories else NA,
    fat      = if ("fat"      %in% names(x_wide)) x_wide$fat      else NA,
    carbs    = if ("carbs"    %in% names(x_wide)) x_wide$carbs    else NA,
    protein  = if ("protein"  %in% names(x_wide)) x_wide$protein  else NA
  )
})

nutrition_df_clean <- nutrition_df |> 
  mutate(
    fat = as.numeric(str_remove_all(fat, "[^0-9\\.]+")),
    carbs = as.numeric(str_remove_all(carbs, "[^0-9\\.]+")),
    protein = as.numeric(str_remove_all(protein, "[^0-9\\.]+"))
  )

recipes_joined <- bind_cols(recipes_joined, nutrition_df_clean)

almost_there <- recipes_joined |>
  dplyr::select(name, url, author,
                date_published, ingredients_text, calories,
                fat, carbs, protein) |> 
  dplyr::rename(ingredients = ingredients_text)

almost_there <- almost_there |>
  left_join(ratings_df, by = "url")

almost_there <- almost_there |> 
  left_join(recipe_timings_df, by = "url")

# Standardizing times to minutes
convert_to_minutes <- function(time_str) {
  days <- as.numeric(str_extract(time_str, "\\d+(?=\\s*day)"))
  hrs  <- as.numeric(str_extract(time_str, "\\d+(?=\\s*hr)"))
  mins <- as.numeric(str_extract(time_str, "\\d+(?=\\s*min)"))
  
  days[is.na(days)] <- 0
  hrs[is.na(hrs)]   <- 0
  mins[is.na(mins)] <- 0
  
  total <- days * 24 * 60 + hrs * 60 + mins
  return(total)
}

almost_there <- almost_there |>
  mutate(
    prep_time = convert_to_minutes(prep_time),
    cook_time = convert_to_minutes(cook_time),
    total_time = convert_to_minutes(total_time)
  ) |> 
  select(-name.y) |> 
  rename(name = name.x)

# Dealin with date_published
clean_dates <- function(date_str) {
  # Extract the actual date portion
  raw_date <- str_extract(date_str, "\\w+ \\d{1,2}, \\d{4}")
  
  # Parse into Date format
  parsed_date <- mdy(raw_date)
  
  return(parsed_date)
}

almost_there <- almost_there |>
  mutate(date_published = clean_dates(date_published))

extract_servings <- function(serving_str) {
  # Extract the first numeric value
  as.numeric(str_extract(serving_str, "\\d+"))
}

there_we_go <- almost_there |> 
  mutate(servings = as.numeric(extract_servings(servings)),
         calories = as.numeric(calories))

readr::write_csv(there_we_go, "data/all_recipes.csv")

# Documentation notes
# Calories have no units. Proteins, Carbs, Fat are in g.
# Date is when the recipe was first published/updated.
# Avg rating is out of 5 stars
# N reviews as number of reviews left by users.
# The rest is self-explanatory