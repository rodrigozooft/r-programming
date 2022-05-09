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