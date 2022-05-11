# Join both data frames
ilo_data <- ilo_hourly_compensation %>%
  inner_join(ilo_working_hours, by = c("country", "year"))

# Count the resulting rows
ilo_data  %>% 
    count()

# Examine ilo_data
ilo_data

# Turn year and country into a factor
ilo_data_corrected <- ilo_data %>%
  mutate(year = as.factor(as.numeric(year)),
        country = as.factor(country))

# See the results
ilo_data_corrected

# Examine the European countries vector
european_countries

# Only retain European countries
ilo_data <- ilo_data %>%
  filter(country %in% european_countries)

# Examine the structure of ilo_data
str(ilo_data)

# Group and summarize the data
ilo_data %>%
  group_by(year) %>%
  summarize(mean_hourly_compensation = mean(hourly_compensation),
            mean_working_hours = mean(working_hours))

# Filter for 2006
plot_data <- ilo_data %>%
  filter(year == 2006)
  
# Create the scatter plot
ggplot(plot_data) +
  geom_point(aes(x = working_hours, y = hourly_compensation))