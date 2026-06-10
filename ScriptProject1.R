data <- read.csv("~/Documents/AGROPARISTECH/ErasmusBHT/DataVisualisation/Project/GlobalGeoTree-10kEval-90.csv", header=TRUE)

## Presentation of the data set
#... "insert text about the context of the data set"
# Number of observations
n_obs <- nrow(data)
n_obs

# Features
n_features <- ncol(data)
n_features

feat_names <- colnames(data)
feat_names

# Brief summary of the dataset
summary(data)

# ... "Explanation of each feature, its limites and what it means"

## Repartition of the data depending on the countries (number of data for each country)
# histogram ou boxplot
library(dplyr)
library(ggplot2)

# Comptage par pays
country_counts <- data %>%
  count(country_code, sort = TRUE)

head(country_counts)

#
ggplot(country_counts,
       aes(x = reorder(country_code, n),
           y = n)) +
  geom_col() +
  coord_flip() +
  labs(
    x = "Countrues",
    y = "Number of observations",
    title = "Number of observations for each country"
  )
  

# Only the first 20 countries
country_counts %>%
  slice_max(n, n = 20) %>%
  ggplot(aes(x = reorder(country_code, n), y = n)) +
  geom_col() +
  coord_flip() +
  labs(
    x = "Country",
    y = "Number of observations",
    title = "20 most represented contries"
  )
# add + theme.minimal() to have it in white

## Distribution of the data for each year and each country
top20_countries <- data %>%
  count(country_code, sort = TRUE) %>%
  slice_head(n = 20)

country_year_counts <- data %>%
  filter(country_code %in% top20_countries$country_code) %>%
  count(country_code, year)

ggplot(country_year_counts,
       aes(x = reorder(country_code, n, FUN = sum),
           y = n,
           fill = year)) +
  geom_col() +
  coord_flip() +
  scale_fill_gradientn(
    colours = c("white", "yellow", "red")
  ) +
  labs(
    x = "Country",
    y = "Number of observations",
    fill = "Year",
    title = "20 most represented countries"
  ) +
  theme_minimal()

ggplot(country_year_counts,
       aes(x = reorder(country_code, n, FUN = sum),
           y = n,
           fill = factor(year))) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(
    values = colorRampPalette(
      c("black", "lightgrey")
    )(length(unique(country_year_counts$year)))
  ) +
  labs(
    x = "Country",
    y = "Number of observations",
    fill = "Year",
    title = "20 most represented countries"
  ) +
  theme_minimal()








top20_countries <- data %>%
  count(country_code, sort = TRUE) %>%
  slice_head(n = 20)

# Comptage pays × année
country_year_counts <- data %>%
  filter(country_code %in% top20_countries$country_code) %>%
  count(country_code, year)

# Ordre des années : récente -> ancienne
country_year_counts$year <- factor(
  country_year_counts$year,
  levels = sort(unique(country_year_counts$year), decreasing = TRUE)
)

ggplot(country_year_counts,
       aes(x = reorder(country_code, n, FUN = sum),
           y = n,
           fill = year)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(
    values = colorRampPalette(
      c("darkblue", "grey")
    )(length(unique(country_year_counts$year)))
  ) +
  labs(
    x = "Country",
    y = "Number of observations",
    fill = "Year",
    title = "20 most represented countries"
  ) +
  theme_minimal()
