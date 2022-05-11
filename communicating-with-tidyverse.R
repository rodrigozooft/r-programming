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

# Create the plot
ggplot(plot_data) +
  geom_point(aes(x = working_hours, y = hourly_compensation)) +
  # Add labels
  labs(
    x = "Working hours per week",
    y = "Hourly compensation",
    title = "The more people work, the less compensation they seem to receive",
    subtitle = "Working hours and hourly compensation in European countries, 2006",
    caption = "Data source: ILO, 2017"
  )

# From previous step
ilo_plot <- ggplot(plot_data) +
  geom_point(aes(x = working_hours, y = hourly_compensation)) +
  labs(
    x = "Working hours per week",
    y = "Hourly compensation",
    title = "The more people work, the less compensation they seem to receive",
    subtitle = "Working hours and hourly compensation in European countries, 2006",
    caption = "Data source: ILO, 2017"
  )

# Add a different theme
ilo_plot +
  theme_dark()

ilo_plot <- ilo_plot +
  theme_minimal() +
  # Customize the "minimal" theme with another custom "theme" call
  theme(
    text = element_text(family = 'Bookman'),
    title = element_text(color = 'gray25'),
    plot.caption = element_text(color = 'gray30'),
    plot.subtitle = element_text(size = 12)
  )

# Render the plot object
ilo_plot

ilo_plot +
  # "theme" calls can be stacked upon each other, so this is already the third call of "theme"
  theme(
    plot.background = element_rect(fill = 'gray95'),
    plot.margin = unit(c(5, 10, 5, 10), units = "mm")
  )

  # Filter ilo_data to retain the years 1996 and 2006
ilo_data <- ilo_data %>%
  filter(year %in% c(1996, 2006))

# Again, you save the plot object into a variable so you can save typing later on
ilo_plot <- ggplot(ilo_data, aes(x = working_hours, y = hourly_compensation)) +
  geom_point() +
   labs(
    x = "Working hours per week",
    y = "Hourly compensation",
    title = "The more people work, the less compensation they seem to receive",
    subtitle = "Working hours and hourly compensation in European countries, 2006",
    caption = "Data source: ILO, 2017"
  ) +
  # Add facets here
  facet_grid(facets = . ~ year)
 
ilo_plot

# For a starter, let's look at what you did before: adding various theme calls to your plot object
ilo_plot +
  theme_minimal() +
  theme(
    text = element_text(family = "Bookman", color = "gray25"),
    plot.subtitle = element_text(size = 12),
    plot.caption = element_text(color = "gray30"),
    plot.background = element_rect(fill = "gray95"),
    plot.margin = unit(c(5, 10, 5, 10), units = "mm")
  )
      
# Define your own theme function below
theme_ilo <- function() {
    theme_minimal() +
    theme(
            text = element_text(family = "Bookman", color = "gray25"),
    plot.subtitle = element_text(size = 12),
    plot.caption = element_text(color = "gray30"),
    plot.background = element_rect(fill = "gray95"),
    plot.margin = unit(c(5, 10, 5, 10), units = "mm")
    )
}