# Load packages
library(moderndive)
library(ggplot2)

# Plot the histogram
ggplot(evals, aes(x = age)) +
  geom_histogram(binwidth = 5) +
  labs(x = "age", y = "count")

# Load packages
library(moderndive)
library(dplyr)

# Compute summary stats
evals %>%
  summarize(mean_age = mean(age),
            median_age = median(age),
            sd_age = sd(age))

# Load packages
library(moderndive)
library(ggplot2)

# Plot the histogram
ggplot(house_prices, aes(x = sqft_living)) +
  geom_histogram() +
  labs(x = "Size (sq.feet)", y = "count")

# Load packages
library(moderndive)
library(dplyr)
library(ggplot2)

# Add log10_size
house_prices_2 <- house_prices %>%
  mutate(log10_size = log10(sqft_living))

# Plot the histogram  
ggplot(house_prices_2, aes(x = log10_size)) +
  geom_histogram() +
  labs(x = "log10 size", y = "count")

# Plot the histogram
ggplot(evals, aes(x = bty_avg)) +
  geom_histogram(binwidth = 0.5) +
  labs(x = "Beauty score", y = "count")

# Jitter plot
ggplot(evals, aes(x = bty_avg, y = score)) +
  geom_jitter() +
  labs(x = "beauty score", y = "teaching score")

# Compute correlation
evals %>%
  summarize(correlation = cor(score, bty_avg))

# View the structure of log10_price and waterfront
house_prices %>%
  select(log10_price, waterfront) %>%
  glimpse()

# Plot 
ggplot(house_prices, aes(x = waterfront, y = log10_price)) +
  geom_boxplot() +
  labs(x = "waterfront", y = "log10 price")

# Calculate stats
house_prices %>%
  group_by(waterfront) %>%
  summarize(mean_log10_price = mean(log10_price), n = n())
  
# Prediction of price for houses with view
10^(6.12)

# Prediction of price for houses without view
10^(5.66)

# Load packages
library(ggplot2)
library(dplyr)
library(moderndive)

# Plot 
ggplot(evals, aes(x = bty_avg, y = score)) +
  geom_point() +
  labs(x = "beauty score", y = "score") +
  geom_smooth(method = "lm", se = FALSE)

# Load package
library(moderndive)

# Fit model
model_score_2 <- lm(score ~ bty_avg, data = evals)

# Output regression table
get_regression_table(model_score_2)

# Use fitted intercept and slope to get a prediction
y_hat <- 3.88 + 5 * 0.0670
y_hat

# Compute residual y - y_hat
4.7 - y_hat

# Fit regression model
model_score_2 <- lm(score ~ bty_avg, data = evals)

# Get regression table
get_regression_table(model_score_2)

# Get all fitted/predicted values and residuals
get_regression_points(model_score_2) %>% 
  mutate(residual_2 = score - score_hat)

ggplot(evals, aes(x = rank, y = score)) +
  geom_boxplot() +
  labs(x = "rank", y = "score")

evals %>%
  group_by(rank) %>%
  summarize(n = n(), mean_score = mean(score), sd_score = sd(score))

# Fit regression model
model_score_4 <- lm(score ~ rank, data = evals)

# Get regression table
get_regression_table(model_score_4)

# teaching mean
teaching_mean <- 4.28

# tenure track mean
tenure_track_mean <- 4.28 - 0.13

# tenured mean
tenured_mean <- 4.28 - 0.145

# Calculate predictions and residuals
model_score_4_points <- get_regression_points(model_score_4)
model_score_4_points

# Plot residuals
ggplot(model_score_4_points, aes(x = residual)) +
  geom_histogram() +
  labs(x = "residuals", title = "Residuals from score ~ rank model")

# Remove outlier
house_prices_transform <- house_prices %>%
  filter(bedrooms < 30)

# Create scatterplot with regression line
ggplot(house_prices_transform, aes(x = bedrooms, y = log10_price)) +
  geom_point() +
  labs(x = "Number of bedrooms", y = "log10 price") +
  geom_smooth(method = "lm", se = FALSE)

# Fit model
model_price_2 <- lm(log10_price ~ log10_size + bedrooms, 
                    data = house_prices)

# Get regression table
get_regression_table(model_price_2)

# Make prediction in log10 dollars
2.69 + 0.941 * log10(1000) - 0.033 * 3

# Make prediction dollars
10^(5.414)

# Fit model
model_price_4 <- lm(log10_price ~ log10_size + waterfront, 
                    data = house_prices)

# Get regression table
get_regression_table(model_price_4)