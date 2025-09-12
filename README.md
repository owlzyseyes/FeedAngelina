# FeedAngelina

A comprehensive recipe data collection and processing pipeline that scrapes recipe information from AllRecipes.com and processes it into clean, structured datasets for analysis.

## What This Repository Does

This project is a data pipeline that:

1. **Web Scrapes Recipe Data**: Uses RSelenium to systematically scrape recipe information from AllRecipes.com, collecting data on ~14,000 recipes
2. **Extracts Comprehensive Information**: For each recipe, captures:
   - Ingredients lists
   - Nutrition facts (calories, protein, carbs, fat)
   - Recipe metadata (prep time, cook time, servings)
   - User ratings and review counts
   - Author and publication date
3. **Processes and Cleans Data**: Transforms raw scraped data into clean, analysis-ready CSV files
4. **Organizes by Categories**: Handles both general recipes and cuisine-specific recipe collections

## Repository Structure

- **`scrape.R`**: Main web scraping script that collects recipe data from AllRecipes.com (~13 hour runtime)
- **`organize.R`**: Data processing script that cleans and structures the scraped recipe data
- **`organize_cuisines.R`**: Similar processing for cuisine-specific recipe collections
- **`page_structure_test.R`**: Development script for testing web scraping selectors
- **`data/`**: Contains JSON exports from MongoDB and processed CSV files
  - `allrecipes.csv`: Clean dataset of all scraped recipes
  - `cuisines.csv`: Cuisine-specific recipe data
  - Various JSON files from database exports

## Data Structure

The final datasets include:
- Recipe names and URLs
- Ingredient lists
- Nutritional information (calories, protein, carbs, fat in grams)
- Timing data (prep time, cook time, total time in minutes)
- User ratings (average rating out of 5 stars)
- Review counts and metadata
- Author information and publication dates

## Future Plans

The long-term vision is to build a recommendation engine where users can input ingredients they have on hand and receive recipe suggestions based on this comprehensive dataset.
