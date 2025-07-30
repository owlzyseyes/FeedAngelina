remDr$navigate("https://www.allrecipes.com/recipe/105016/simple-and-easy-stuffed-peppers/")

# Assuming you've navigated and read the page source
page <- read_html(remDr$getPageSource()[[1]])

# Ingredients
ingredients <- page |>
  html_nodes("ul.mm-recipes-structured-ingredients__list li") |>
  html_text(trim = TRUE)

# Author name
author <- page |>
  html_nodes(".mntl-attribution__item-name, 
              span.comp.mntl-bylines__item.mntl-attribution__item.mntl-attribution__item-name") |>
  html_text(trim = TRUE)

# Published date
date <- page |>
  html_node("div.mntl-attribution__item-date") |>
  html_text(trim = TRUE)

# Recipe metadata labels + values (e.g. servings, prep time)
labels <- page |> html_nodes("div.mm-recipes-details__label") |> html_text(trim = TRUE)
values <- page |> html_nodes("div.mm-recipes-details__value") |> html_text(trim = TRUE)
details <- tibble::tibble(label = labels, value = values)

# Nutrition facts
nutrition_names <- page |> html_nodes("td.mm-recipes-nutrition-facts-summary__table-cell.text-body-100") |> html_text(trim = TRUE)
nutrition_values <- page |> html_nodes("td.mm-recipes-nutrition-facts-summary__table-cell.text-body-100-prominent") |> html_text(trim = TRUE)
nutrition <- tibble::tibble(fact = nutrition_names, amount = nutrition_values)

# Ratings and Reviews
average_rating <- page |> html_node("#mm-recipes-review-bar__rating_1-0") |> html_text(trim = TRUE)
total_ratings  <- page |> html_node("#mm-recipes-review-bar__rating-count_1-0") |> html_text(trim = TRUE)
review_count   <- page |> html_node("#mm-recipes-review-bar__comment-count_1-0") |> html_text(trim = TRUE)

meta <- tibble::tibble(
  author = author,
  date_published = date,
  average_rating = average_rating,
  total_ratings = total_ratings,
  review_count = review_count
)

ingredients_tbl <- tibble::tibble(ingredient = ingredients)

nutrition_tbl <- tibble::tibble(
  fact = nutrition_names,
  amount = nutrition_values
)

details_tbl <- tibble::tibble(label = labels, value = values)

# Optionally: pivot wider
details_wide <- tidyr::pivot_wider(details_tbl, names_from = label, values_from = value)

recipe_data <- list(
  url = "https://www.allrecipes.com/air-fryer-honey-mustard-salmon-bites-recipe-11680825",
  author = author,
  date_published = date,
  ratings = list(
    average = average_rating,
    total = total_ratings,
    reviews = review_count
  ),
  details = details_wide,
  ingredients = ingredients_tbl,
  nutrition = nutrition_tbl
)

recipe_df <- tibble::tibble(
  url = "https://www.allrecipes.com/air-fryer-honey-mustard-salmon-bites-recipe-11680825",
  author = author,
  date_published = date,
  ratings = list(
    tibble::tibble(avg = average_rating, total = total_ratings, reviews = review_count)
  ),
  details = list(details_wide),
  ingredients = list(ingredients_tbl),
  nutrition = list(nutrition_tbl)
)

