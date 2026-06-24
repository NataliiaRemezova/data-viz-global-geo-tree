data <- read.csv("~/Documents/AGROPARISTECH/ErasmusBHT/DataVisualisation/Project/GlobalGeoTree-10kEval-90.csv", header=TRUE)

## Data treatment ============================================================================
# looking for missing data
any(is.na(data))
sum(is.na(data))
# No missing data in this file (it's a refined dataset so it makes sens, to be clear of NA data)

##  ============================================================================
##                    Presentation of the data set
##  ============================================================================

## What is the gobal and evident structure of the dataset? =====================
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

## How are the data spread across the different countries (number of observations for each country) ?=======
library(dplyr)
library(ggplot2)
# Count for each country
country_counts <- data %>%
  count(country_code, sort = TRUE)
head(country_counts)
# Histogram plot
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

# Histogram of the 20 first countries only
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

## How are the data distributed troughout the years in different countries? Do we see an evoution? ==========
top20_countries <- data %>%
  count(country_code, sort = TRUE) %>%
  slice_head(n = 20)

country_year_counts <- data %>%
  filter(country_code %in% top20_countries$country_code) %>% # Counting of every data for each country per year
  count(country_code, year)

country_year_counts$year <- factor(
  country_year_counts$year,
  levels = sort(unique(country_year_counts$year), decreasing = TRUE) # Order of the years : recent -> old
)

ggplot(country_year_counts,
       aes(x = reorder(country_code, n, FUN = sum),
           y = n,
           fill = year)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(
    values = colorRampPalette(
      c("darkblue", "red")
    )(length(unique(country_year_counts$year)))
  ) +
  labs(
    x = "Country",
    y = "Number of observations",
    fill = "Year",
    title = "20 most represented countries"
  ) +
  theme_minimal()


## What are the different proportions of tree categories among the observations? =========
# Number of trees for each category

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
  ) #count of the number of tree for each category

## Dispertion of the species by difficulty in every contry
top20_countries_category <- data %>%
  count(country_code, sort = TRUE) %>%
  slice_head(n = 20)

country_category_counts <- data %>%
  filter(country_code %in% top20_countries_category$country_code) %>%
  count(country_code, Category) # Counting of every data for each country per category

country_category_counts$Category <- factor(
  country_category_counts$Category,
  levels = sort(unique(country_category_counts$Category))
) # Order of the categories

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
## This representation has to be taken carefully because some of the countries don't present any rare obervation
## This does NOT means that there are no rare trees in this area, it only means that they haven't been observed
## However, we can say that Common and Frequent treed are much more largely observed (which make sens because thay should appear more often)

## What are the different tree species referenced in thsi dataset? ================================

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
## chack again for missing values
## plot each category geographycally

# Definition of each tree category:
# Evergreen Needleleaf: An evergreen needleleaf refers to woody vegetation that retains needle-like or scale-like foliage year-round
# Evergreen Broadleaf: An evergreen broadleaf is a plant or tree that has wide, flat leaves (rather than needles or scales) and retains its foliage year-round
# Deciduous Broadleaf:  a diverse group of flowering plants (angiosperms) characterized by flat leaves that are shed annually before winter

# Dispersion of each rarety category depending on the the category of the specie
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

## What are the different taxonomic families represented in this dataset and in which proportion are they represented?

level1_count <- data %>%
  count(level1_family, sort=TRUE) # Number of level1_categories

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

## =============================================================================
## Geographical repartition of the dataset 
## =============================================================================

## How are the trees distributed depending on the latitude ? ===================

ggplot(data, aes(x = latitude)) +
  geom_histogram(binwidth = 2, fill = "forestgreen", color = "white") +
  scale_x_reverse() +
  labs(
    title = "Distribution of the observations depending on the latitude",
    x = "Latitude (°)",
    y = "Number of observations"
  ) +
  annotate(
    "text",
    x = min(data$latitude, na.rm = TRUE),
    y = -Inf,
    label = "South",
    vjust = 1,
    hjust = 0,
    col="darkgreen"
  ) +
  annotate(
    "text",
    x = max(data$latitude, na.rm = TRUE),
    y = -Inf,
    label = "North",
    vjust = 1,
    hjust = 1,
    col="darkgreen"
  ) +
  theme_minimal() +
  coord_cartesian(clip = "off") +
  theme(
    plot.margin = margin(10, 10, 30, 10)
  )

# density curve
ggplot(data, aes(x = latitude)) +
  geom_density(fill = "forestgreen", alpha = 0.5) +
  labs(
    title = "Tree density depending on the latitude",
    x = "Latitude (°)",
    y = "Density"
  ) +
  theme_minimal() +
  annotate(
    "text",
    x = min(data$latitude, na.rm = TRUE),
    y = -Inf,
    label = "South",
    vjust = 2,
    hjust = 0,
    col="darkgreen"
  ) +
  annotate(
    "text",
    x = max(data$latitude, na.rm = TRUE),
    y = -Inf,
    label = "North",
    vjust = 2,
    hjust = 1,
    col="darkgreen"
  ) +
  coord_cartesian(clip = "off") +
  theme(
    plot.margin = margin(10, 10, 30, 10)
  )
  
## What is the repartition of the trees among the latitudinal bands? ===========
#(Number of tree in each longitudinal branch)
library(dplyr)

lat_band <- data %>%
  mutate(lat_bin = floor(latitude / 5) * 5) %>%
  count(lat_bin)

ggplot(lat_band,
       aes(x = lat_bin, y = n)) +
  geom_col(fill = "darkgreen") +
  labs(
    title = "Number of trees for each band of 5° latitude",
    x = "Latitude",
    y = "Number of observations"
  ) +
  theme_minimal()
# We usually take 5° slices because it is small enough to find local tendencies 
# but also big enough to avoid noise and too much non useful information
# generally fine enough to reveal geographical patterns
# (climatic gradients, biodiversity, species distribution, etc.) without creating too much noise.

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

## How does the latitude influences the different species observed? (Lattitude and specific richness)=====
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
  theme_minimal()+
  scale_x_reverse()
# Species richness: simple count of the number of different species present in a 
# specific biological community, landscape, or region
# Much more important in the north hemisphere than in the south hemisphere
# more important in tempered areas, much lower at the equador and near the poles

## How does the longitude influences the diffrent species observed (longitude and specifi richness)=======
richness_long <- data %>%
  mutate(long_bin = floor(longitude / 5) * 5) %>%
  group_by(long_bin) %>%
  summarise(richesse_long = n_distinct(species_key))

ggplot(richness_long,
       aes(long_bin, richesse_long)) +
  geom_line(linewidth = 1) +
  geom_point() +
  labs(
    title = "Richesse spécifique selon la longitude",
    x = "Longitude",
    y = "Number of species observed"
  ) +
  theme_minimal()+
  scale_x_reverse()

## What is the local specific richness across the world? =======================
library(dplyr)

richness_grid <- data %>% # specie richness in each sqare of 5x5°
  mutate(
    lat_bin = floor(latitude / 1) * 1,
    long_bin = floor(longitude / 1) * 1
  ) %>%
  group_by(lat_bin, long_bin) %>%
  summarise(
    richness = n_distinct(species_key),
    .groups = "drop"
  )

library(ggplot2)
library(maps)

world <- map_data("world")

ggplot() + #pot of the map
  geom_tile(
    data = richness_grid,
    aes(
      x = long_bin + 2.5,
      y = lat_bin + 2.5,
      fill = richness
    )
  ) +
  geom_polygon(
    data = world,
    aes(long, lat, group = group),
    fill = NA,
    color = "black",
    linewidth = 0.2
  ) +
  scale_fill_gradientn(
    colours = c(
      "yellow",
      "orange",
      "darkred"
    ),
    name = "Species richness"
  ) +
  coord_fixed(1.3) +
  labs(
    title = "Global tree species richness",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal()

## Looking for correlations in the data ========================================
# Selection of the numerical values only
data_num <- data[sapply(data, is.numeric)]

# Correlation Matrix
cor_mat <- cor(data_num, use = "pairwise.complete.obs")

# Heatmap
library(corrplot)
corrplot(cor_mat, method = "color")
