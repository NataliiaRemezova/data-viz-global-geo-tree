data <- read.csv("~/Documents/AGROPARISTECH/ErasmusBHT/DataVisualisation/Project/GlobalGeoTree-10kEval-90.csv", header=TRUE)

## Presentation of the data set============================================================================
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

# ... "Explanation of each feature, its boundaries and what it means"

## Repartition of the data depending on the countries (number of data for each country) ====================
# histogram or boxplot
library(dplyr)
library(ggplot2)

# Count for each country
country_counts <- data %>%
  count(country_code, sort = TRUE)

head(country_counts)

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
  

# same principle but only the first 20 countries
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

# Distribution of the data for each year and each country
top20_countries <- data %>%
  count(country_code, sort = TRUE) %>%
  slice_head(n = 20)

# Counting of every data for each country per year
country_year_counts <- data %>%
  filter(country_code %in% top20_countries$country_code) %>%
  count(country_code, year)

# Order of the years : recent -> old
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

## Repartition of the observations among the tree categories ans species ============================================
# Number of trees for each category
#count of the number of tree for each category
category_count <- data %>%
  count(Category, sort = TRUE)
ggplot(category_count,
       aes(x = reorder(Category, n),
           y = n)) +
       geom_col(fill="darkblue") +
         labs(
           x = "Category",
           y = "Number of observations",
           title = "Number of observations for each category of specie"
         ) +
       theme_minimal() +
  theme(
    axis.text.x = element_text(size = 14),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16)
  )

## Dispertion of the species by difficulty in every contry
top20_countries_category <- data %>%
  count(country_code, sort = TRUE) %>%
  slice_head(n = 20)

# Counting of every data for each country per category
country_category_counts <- data %>%
  filter(country_code %in% top20_countries_category$country_code) %>%
  count(country_code, Category)

# Order of the categories
country_category_counts$Category <- factor(
  country_category_counts$Category,
  levels = sort(unique(country_category_counts$Category))
)

ggplot(country_category_counts,
       aes(x = reorder(country_code, n, FUN = sum),
           y = n,
           fill = Category)) +
  geom_col(position = position_stack(reverse = TRUE)) +
  coord_flip() +
  scale_fill_manual(
    values = colorRampPalette(
      c("springgreen", "forestgreen", "darkgreen")
    )(length(unique(country_category_counts$Category)))
  ) +
  labs(
    x = "Country",
    y = "Number of observations",
    fill = "Category",
    title = "Proportion of each category for the 20 most represented countries"
  ) +
  theme_minimal()

## Description of the different species of trees
# Number of different broad vegetation category and dispertion within the categories
level0_count <- data %>%
  count(level0, sort = TRUE)

ggplot(level0_count,
       aes(x = reorder(level0, n),
           y = n)) +
  geom_col(fill = "forestgreen") +
  labs(
    x = "Plant category",
    y = "Number of observations",
    title = "Number of observations for each plant category"
  ) +
  theme_minimal()

# Dispersion of each rarety category depending on the the categiry specie
level0_category <- data %>%
  count(level0, Category)

ggplot(level0_category,
       aes(x = level0,
           y = n,
           fill = Category)) +
  geom_col() +
  scale_fill_manual(
    values = c("lightblue", "blue","darkblue"
    )
  ) +
  labs(
    x = "Plant category",
    y = "Number of observations",
    fill = "Difficulty category",
    title = "Proportion of rarety levels within the category"
  ) +
  theme_minimal()

# Dispersion of the plant category within the difficulty category
level0_category <- data %>%
  count(Category, level0)

ggplot(level0_category,
       aes(x = Category,
           y = n,
           fill = level0)) +
  geom_col(position = "stack") +
  coord_flip()+
  scale_fill_manual(
    values = c("lightblue", "blue", "darkblue")
  ) +
  labs(
    x = "Difficulty category",
    y = "Proportion",
    fill = "Plant category",
    title = "Proportion of plant categories within each difficulty level"
  ) +
  theme_minimal()


# Number of level1_categories
level1_count <- data %>%
  count(level1_family, sort=TRUE)

ggplot(level1_count,
       aes(x = reorder(level1_family, n),
           y = n)) +
  geom_col(fill = "brown") +
  labs(
    x = "Taxonomic family",
    y = "Number of observations",
    title = "Number of observations for each Taxonomic family"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )

# arbre hiérarchique présentant les catégories, espèces et taxon de chaque obervation possible
# chack whether the relationships are linear or not
data %>%
  distinct(level0, level1_family) %>%
  count(level1_family) %>%
  filter(n > 1)

data %>%
  distinct(level1_family, level2_genus) %>%
  count(level2_genus) %>%
  filter(n > 1)

data %>%
  distinct(level0, level1_family) %>%
  filter(level1_family %in%
           c("Ericaceae",
             "Euphorbiaceae",
             "Fabaceae",
             "Rhamnaceae",
             "Sapindaceae")) %>%
  arrange(level1_family)
## tree not possible with the three categories because the relationships are not linears



## How are the trees distributed depending on the latitude ? ===================

## distribution depending on the latitude

# histogramme
ggplot(data, aes(x = latitude)) +
  geom_histogram(binwidth = 2, fill = "forestgreen", color = "white") +
  labs(
    title = "Tree distrbution depending on the latitude",
    x = "Latitude (°)",
    y = "Number of observations"
  ) +
  theme_minimal()
# ajouter où est le nord et le sud et faire en sorte que la latitude 0 soit au milieu

# density curve
ggplot(data, aes(x = latitude)) +
  geom_density(fill = "forestgreen", alpha = 0.5) +
  labs(
    title = "Tree density depending on the latitude",
    x = "Latitude (°)",
    y = "Density"
  ) +
  theme_minimal()
# ajouter où est le nord et le sud et faire en sorte que la latitude 0 soit au milieu

## Number of tree in each longitudinal branch
library(dplyr)

lat_band <- data %>%
  mutate(lat_bin = floor(latitude / 5) * 5) %>%
  count(lat_bin)

ggplot(lat_band,
       aes(x = lat_bin, y = n)) +
  geom_col(fill = "darkgreen") +
  labs(
    title = "Nombre d'arbres par bande de 5° de latitude",
    x = "Latitude",
    y = "Nombre d'observations"
  ) +
  theme_minimal()
# comment sont définies chaque branche longitudinale ? est-ce qu'on peut rendre àa plus ou moins fin ?
# comment relier ça aux gradients climatiques ?

## Symmetrical distribution
ggplot(data, aes(x = abs(latitude))) +
  geom_histogram(binwidth = 2,
                 fill = "forestgreen",
                 color = "white") +
  labs(
    title = "Distance à l'équateur",
    x = "Latitude absolue (°)",
    y = "Nombre d'arbres"
  ) +
  theme_minimal()
# pas forcément le plus intéressant, est-ce qu'on peut vraiment en tirer de l'information au vu de la répartition des observations (à comparer avec le planisphère)

## Lattitude and specific richness
richness <- data %>%
  mutate(lat_bin = floor(latitude / 5) * 5) %>%
  group_by(lat_bin) %>%
  summarise(richesse = n_distinct(species_key))

ggplot(richness,
       aes(lat_bin, richesse)) +
  geom_line(linewidth = 1) +
  geom_point() +
  labs(
    title = "Richesse spécifique selon la latitude",
    x = "Latitude",
    y = "Nombre d'espèces"
  ) +
  theme_minimal()
# interpréter la différence de richesse spécifique en fonction de la latitude
