df <- read.csv("GlobalGeoTree-10kEval-90.csv")

df

summary(df)

library(sf)
library(ggplot2)
library(rnaturalearth)
library(dplyr)
library(tidyr)
library(knitr)
library(gt)


###### How many distinct species, genus and families are there in the dataset in total ? ###### 
resume <- df %>%
  summarise(
    Broad_vegetation = n_distinct(level0),
    Espèces  = n_distinct(level3_species),
    Genres   = n_distinct(level2_genus),
    Familles = n_distinct(level1_family)
  ) %>%
  pivot_longer(everything(), names_to = "Catégorie", values_to = "Nombre")


resume %>%
  gt() %>%
  tab_header(title = "Résumé taxonomique du dataset") %>%
  cols_label(Nombre = "Nombre distinct")




###### Which families are most commonly found in terms of the number of observations ? ######
top_familles <- df %>% 
  count(level1_family, name = "n_obs") %>%
  arrange(desc(n_obs)) %>%
  slice_max(n_obs, n = 15)   # top 15, ajustable

ggplot(top_familles, aes(x = reorder(level1_family, n_obs), y = n_obs)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Familles les plus représentées",
       x = "Famille", y = "Nombre d'observations") +
  theme_minimal()



###### How are the main vegetation categories (level 0) distributed amongst themselves ? ######
repartition_level0 <- df %>%
  count(level0, name = "n_obs") %>%
  mutate(pct = n_obs / sum(n_obs) * 100,
         label = paste0(level0, "\n", round(pct, 1), "%"))

ggplot(repartition_level0, aes(x = "", y = n_obs, fill = level0)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text(aes(label = paste0(round(pct, 1), "%")),
            position = position_stack(vjust = 0.5), size = 3) +
  labs(title = "Répartition des grandes catégories de végétation",
       fill = "Catégorie level0") +
  theme_void()


library(treemapify)
ggplot(repartition_level0, aes(area = n_obs, fill = level0,
                               label = paste0(level0, "\n", n_obs))) +
  geom_treemap() +
  geom_treemap_text(colour = "white", place = "centre",
                    grow = TRUE, reflow = TRUE) +
  labs(title = "Répartition des grandes catégories de végétation",
       fill = "Catégorie level0") +
  theme(legend.position = "none")



###### illustrate the complete taxonomic structure : level0 → family → genus → species ######
library(plotly)

df_clean <- df %>%
  filter(!is.na(level1_family), !is.na(level2_genus), !is.na(level3_species))

l0 <- df_clean %>% count(level0, name = "n") %>%
  mutate(id = level0, parent = "", label = level0)

fa <- df_clean %>% count(level0, level1_family, name = "n") %>%
  mutate(id = paste(level0, level1_family, sep = "/"), parent = level0, label = level1_family)

ge <- df_clean %>% count(level0, level1_family, level2_genus, name = "n") %>%
  mutate(id = paste(level0, level1_family, level2_genus, sep = "/"),
         parent = paste(level0, level1_family, sep = "/"), label = level2_genus)

sp <- df_clean %>% count(level0, level1_family, level2_genus, level3_species, name = "n") %>%
  mutate(id = paste(level0, level1_family, level2_genus, level3_species, sep = "/"),
         parent = paste(level0, level1_family, level2_genus, sep = "/"), label = level3_species)

hierarchie <- bind_rows(l0, fa, ge, sp) %>% select(id, parent, label, n)

plot_ly(data = hierarchie, type = "treemap",
        ids = ~id, labels = ~label, parents = ~parent, values = ~n,
        branchvalues = "total")


###### Country rankings ######

top_pays <- df %>%
  count(location, name = "n_obs") %>%
  arrange(desc(n_obs)) %>%
  slice_max(n_obs, n = 15)

ggplot(top_pays, aes(x = reorder(location, n_obs), y = n_obs)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Pays avec le plus d'observations",
       x = "Pays", y = "Nombre d'observations") +
  theme_minimal()

###### A choropleth map by country ######
# provides a more visual answer to the question “Which countries have the highest concentration?” #

library(rnaturalearth)
library(sf)

compte_pays <- df %>% count(country_code, name = "n_obs")

world <- ne_countries(scale = "medium", returnclass = "sf")
world_data <- world %>% left_join(compte_pays, by = c("iso_a2" = "country_code"))

# Log is essential here. Otherwise, one or two countries will "dominate" the entire colour palette

ggplot(world_data) +
  geom_sf(aes(fill = n_obs), color = "white", linewidth = 0.1) +
  scale_fill_viridis_c(option = "plasma", trans = "log10", na.value = "grey90") +
  labs(title = "Nombre d'observations par pays", fill = "Nb. obs. (log)") +
  theme_minimal()


###### Scatter map (for raw spatial density) ######
world <- ne_countries(scale = "medium", returnclass = "sf")
df_geo <- df %>% filter(!is.na(longitude), !is.na(latitude))

ggplot() +
  geom_sf(data = world, fill = "grey95", color = "grey80") +
  geom_point(data = df_geo, aes(x = longitude, y = latitude),
             color = "darkred", alpha = 0.25, size = 0.4) +
  coord_sf(expand = FALSE) +
  labs(title = "Répartition spatiale des observations") +
  theme_minimal()


###### ######


###### CREATE A PLANISPHERE ######
world <- ne_countries(scale = "medium", returnclass = "sf")

world_sans_antarctique <- world %>%
  filter(admin != "Antarctica")

ggplot(world_sans_antarctique) +
  geom_sf(fill = "lightblue", color = "white", linewidth = 0.2) +
  theme_minimal()

villes <- data.frame(
  nom = data$country_code,
  lon = data$longitude,
  lat = data$latitude
)

ggplot(world) +
  geom_sf(fill = "grey90") +
  geom_point(
    data = villes,
    aes(x = lon, y = lat),
    color = "red",
    size = 3
  ) +
  coord_sf()



