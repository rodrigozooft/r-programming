netflix_df %>% 
  # Split the duration column into value and unit columns
  separate(duration, into = c('value', 'unit'), sep = " ", convert = TRUE)