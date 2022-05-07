netflix_df %>% 
  # Split the duration column into value and unit columns
  separate(duration, into = c('value', 'unit'), sep = " ", convert = TRUE)

phone_nr_df %>%
  # Unite the country_code and national_number columns
  unite("international_number", country_code, national_number, sep = " ")

tvshow_df %>% 
  # Separate the actors in the cast column over multiple rows
  separate_rows(cast, sep = ", ") %>% 
  rename(actor = cast) %>% 
  count(actor, sort = TRUE) %>% 
  head()

drink_df %>% 
  # Separate the ingredients over rows
  separate_rows(ingredients, sep = "; ") %>% 
  # Separate ingredients into three columns
  separate(
    ingredients, 
    into = c("ingredient", "quantity", "unit"), 
    sep = " ", 
    convert = TRUE
  ) %>% 
  # Group by ingredient and unit
  group_by(ingredient, unit) %>% 
  # Calculate the total quantity of each ingredient
  summarize(quantity = sum(quantity))

director_df %>% 
  # Drop rows with NA values in the director column
  drop_na(director) %>% 
  # Spread the director column over separate rows
  separate_rows(director, sep = ", ") %>% 
  # Count the number of movies per director
  count(director, sort = TRUE)