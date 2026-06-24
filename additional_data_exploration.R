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
