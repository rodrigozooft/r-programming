netflix_df %>% 
  # Split the duration column into value and unit columns
  separate(duration, into = c('value', 'unit'), sep = " ", convert = TRUE)

phone_nr_df %>%
  # Unite the country_code and national_number columns
  unite("international_number", country_code, national_number, sep = " ")