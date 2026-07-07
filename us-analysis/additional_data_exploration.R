install.packages(c("jsonlite", "dplyr", "purrr", "tibble", "stringr"))

library(jsonlite)
library(dplyr)
library(purrr)
library(tibble)
library(stringr)

getwd()
list.files()
root <- "/home/nremezova/Dokumente/data_visualization/data-viz-final-proj/GGT-10kEval-90"
list.files(root)

aux_files <- list.files(
  root,
  pattern = "\\.auxiliary\\.json$",
  recursive = TRUE,
  full.names = TRUE
)

read_one <- function(aux_path) {
  key <- basename(aux_path) |>
    str_remove("\\.auxiliary\\.json$")
  
  text_path <- file.path(dirname(aux_path), paste0(key, ".text.json"))
  image_path <- file.path(dirname(aux_path), paste0(key, ".images.pth"))
  
  aux <- fromJSON(aux_path, simplifyVector = TRUE)
  label <- fromJSON(text_path, simplifyVector = TRUE)
  
  aux <- unlist(aux, recursive = TRUE)
  aux <- as.list(aux)
  
  tibble(
    key = key,
    image_path = image_path,
    level0 = label$level0,
    family = label$level1_family,
    genus = label$level2_genus,
    species = label$level3_species
  ) |>
    bind_cols(as_tibble_row(aux))
}

df <- map_dfr(aux_files, read_one)

glimpse(df)

table(df$species)
table(df$family)

aux_df <- df |>
  select(longitude:bio19)

head(aux_df)

plot(df$longitude, df$latitude, pch = 16, cex = 0.3)

colSums(is.na(df))

model_df <- df |>
  select(species, longitude:bio19) |>
  na.omit()

glimpse(model_df)

colSums(is.na(model_df))

library(ggplot2)

ggplot(df, aes(x = bio01 / 10)) +
  geom_histogram(bins = 40) +
  labs(x = "Annual mean temperature (C)")


df <- df |>
  rename(
    annual_mean_temp = bio01,
    mean_diurnal_temp_range = bio02,
    isothermality = bio03,
    temp_seasonality = bio04,
    max_temp_warmest_month = bio05,
    min_temp_coldest_month = bio06,
    temp_annual_range = bio07,
    mean_temp_wettest_quarter = bio08,
    mean_temp_driest_quarter = bio09,
    mean_temp_warmest_quarter = bio10,
    mean_temp_coldest_quarter = bio11,
    annual_precipitation = bio12,
    precip_wettest_month = bio13,
    precip_driest_month = bio14,
    precip_seasonality = bio15,
    precip_wettest_quarter = bio16,
    precip_driest_quarter = bio17,
    precip_warmest_quarter = bio18,
    precip_coldest_quarter = bio19
  )

colSums(is.na(df))


library(dplyr)
library(corrplot)

aux_corr <- df |>
  select(where(is.numeric)) |>
  select(-sample_id) |>
  na.omit()

cor_mat <- cor(aux_corr, method = "pearson")

corrplot(
  cor_mat,
  method = "color",
  type = "upper",
  tl.cex = 0.7,
  tl.col = "black"
)
  
cor_long <- as.data.frame(as.table(cor_mat)) |>
filter(Var1 != Var2) |>
mutate(abs_corr = abs(Freq)) |>
arrange(desc(abs_corr))

head(cor_long, 20)

cor_long <- as.data.frame(as.table(cor_mat)) |))>
  filter(as.character(Var1) < as.character(Var2)) |>
  mutate(abs_corr = abs(Freq)) |>
  arrange(desc(abs_corr))

head(cor_long, 20)

high_corr <- cor_long |>
  filter(abs_corr > 0.9)

high_corr

ggplot(df, aes(x = level0, y = annual_mean_temp / 10)) +
  geom_boxplot() +
  coord_flip()

ggplot(df, aes(x = level0, y = annual_precipitation)) +
  geom_boxplot() +
  coord_flip()

df_core <- df |>
  select(
    species,
    level0,
    family,
    genus,
    longitude,
    latitude,
    elevation,
    slope,
    aspect,
    annual_mean_temp,
    temp_seasonality,
    temp_annual_range,
    annual_precipitation,
    precip_seasonality,
    precip_driest_month,
    precip_wettest_month,
    soil_moisture_0_5cm
  )

aux_corr <- df_core |>
  select(where(is.numeric)) |>
  na.omit()

cor_mat <- cor(aux_corr, method = "pearson")

corrplot(
  cor_mat,
  method = "color",
  type = "upper",
  tl.cex = 0.7,
  tl.col = "black"
)

cor_long <- as.data.frame(as.table(cor_mat)) |>
  filter(as.character(Var1) < as.character(Var2)) |>
  mutate(abs_corr = abs(Freq)) |>
  arrange(desc(abs_corr))

head(cor_long, 20)

high_corr <- cor_long |>
  filter(abs_corr > 0.9)

high_corr

library(dplyr)
library(ggplot2)

pca_data <- df_core |>
  select(where(is.numeric)) |>
  na.omit()

pca_scaled <- scale(pca_data)

pca <- prcomp(pca_scaled, center = TRUE, scale. = TRUE)

summary(pca)

pca_loadings <- as.data.frame(pca$rotation[, 1:3])

pca_loadings |>
  mutate(variable = rownames(pca_loadings)) |>
  arrange(desc(abs(PC1)))

pca_scores <- as.data.frame(pca$x) |>
  bind_cols(df_core |> select(level0, family, genus, species) |> slice(as.numeric(rownames(pca_data))))

ggplot(pca_scores, aes(x = PC1, y = PC2, color = level0)) +
  geom_point(alpha = 0.5, size = 1) +
  theme_minimal() +
  labs(
    title = "PCA of auxiliary environmental variables",
    x = "PC1",
    y = "PC2",
    color = "Tree type"
  )

biplot(pca, scale = 0)

pca_loadings <- as.data.frame(pca$rotation[, 1:2])
pca_loadings$variable <- rownames(pca_loadings)

arrow_scale <- 4

ggplot(pca_scores, aes(PC1, PC2, color = level0)) +
  geom_point(alpha = 0.35, size = 1) +
  geom_segment(
    data = pca_loadings,
    aes(
      x = 0,
      y = 0,
      xend = PC1 * arrow_scale,
      yend = PC2 * arrow_scale
    ),
    inherit.aes = FALSE,
    arrow = arrow(length = unit(0.2, "cm")),
    color = "black"
  ) +
  geom_text(
    data = pca_loadings,
    aes(
      x = PC1 * arrow_scale * 1.15,
      y = PC2 * arrow_scale * 1.15,
      label = variable
    ),
    inherit.aes = FALSE,
    size = 3
  ) +
  theme_minimal() +
  labs(
    title = "PCA biplot",
    x = "PC1",
    y = "PC2",
    color = "Tree type"
  )

# Install Rtsne once if it is not installed yet
# install.packages("Rtsne")

library(Rtsne)
library(dplyr)
library(ggplot2)

# ------------------------------------------------------------
# Prepare numeric auxiliary data
# ------------------------------------------------------------

# Keep only numeric columns and remove rows with missing values
tsne_data <- df_core |>
  select(where(is.numeric)) |>
  na.omit()

# Scale variables so that large-value variables do not dominate
tsne_scaled <- scale(tsne_data)

# ------------------------------------------------------------
# Run t-SNE
# ------------------------------------------------------------

set.seed(123)

tsne <- Rtsne(
  tsne_scaled,
  dims = 2,
  perplexity = 30,
  theta = 0.5,
  pca = TRUE,
  check_duplicates = FALSE
)

# ------------------------------------------------------------
# Create plotting table
# ------------------------------------------------------------

# Extract t-SNE coordinates
tsne_plot <- as.data.frame(tsne$Y)
names(tsne_plot) <- c("TSNE1", "TSNE2")

# Add labels back to the t-SNE result
tsne_plot <- tsne_plot |>
  bind_cols(
    df_core |>
      select(level0, family, genus, species) |>
      slice(as.numeric(rownames(tsne_data)))
  )

# ------------------------------------------------------------
# Plot by broad tree type
# ------------------------------------------------------------

ggplot(tsne_plot, aes(TSNE1, TSNE2, color = level0)) +
  geom_point(alpha = 0.5, size = 1) +
  theme_minimal() +
  labs(
    title = "t-SNE of auxiliary environmental variables",
    color = "Tree type"
  )

# ------------------------------------------------------------
# Plot top 10 families
# ------------------------------------------------------------

top_families <- tsne_plot |>
  count(family, sort = TRUE) |>
  slice_head(n = 10) |>
  pull(family)

tsne_plot |>
  filter(family %in% top_families) |>
  ggplot(aes(TSNE1, TSNE2, color = family)) +
  geom_point(alpha = 0.6, size = 1) +
  theme_minimal() +
  labs(
    title = "t-SNE of auxiliary environmental variables: top families",
    color = "Family"
  )
