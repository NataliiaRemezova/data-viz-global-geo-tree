############################################################
# GlobalGeoTree 10kEval Environmental Data Extraction
############################################################

library(httr)
library(jsonlite)
library(dplyr)


# Correct TAR file path
url <- "https://huggingface.co/datasets/yann111/GlobalGeoTree/resolve/main/GlobalGeoTree-10kEval/GGT-10kEval-900.tar"


# Download tar
tar_file <- tempfile(fileext=".tar")

GET(
  url,
  write_disk(tar_file, overwrite=TRUE)
)


# Check archive content
files <- untar(
  tar_file,
  list=TRUE
)


# See first files
print(head(files,20))



############################################################
# Extract GlobalGeoTree Environmental JSON Data
############################################################

library(jsonlite)
library(dplyr)
library(ggplot2)
library(viridis)


# Select only auxiliary files
json_files <- files[
  grepl(
    "auxiliary.json",
    files
  )
]


print(length(json_files))


# Read JSON files
environment_data <- lapply(
  json_files,
  function(f){
    
    # extract one json file
    untar(
      tar_file,
      files=f,
      exdir=tempdir()
    )
    
    json_path <- file.path(
      tempdir(),
      f
    )
    
    fromJSON(json_path)
    
  }
)


# Combine all samples
environment_df <- bind_rows(environment_data)


############################################################
# Check columns
############################################################

print(names(environment_df))

head(environment_df)



############################################################
# Remove missing values
############################################################

environment_df <- environment_df %>%
  filter(
    !is.na(latitude),
    !is.na(longitude),
    !is.na(soil_moisture_0_5cm),
    !is.na(bio12)
  )



############################################################
# Germany filter
############################################################

germany <- environment_df %>%
  filter(
    longitude >= 5.8,
    longitude <= 15.1,
    latitude >= 47.2,
    latitude <= 55.1
  )


print(nrow(germany))



############################################################
# 1. SOIL MOISTURE HEATMAP
############################################################

ggplot(
  germany,
  aes(
    x=longitude,
    y=latitude,
    color=soil_moisture_0_5cm
  )
)+
  geom_point(
    size=3,
    alpha=0.8
  )+
  scale_color_viridis_c()+
  theme_minimal()+
  labs(
    title="Germany Soil Moisture Distribution",
    x="Longitude",
    y="Latitude",
    color="Soil Moisture"
  )



############################################################
# 2. RAINFALL HEATMAP
# bio12 = Annual precipitation
############################################################

ggplot(
  germany,
  aes(
    x=longitude,
    y=latitude,
    color=bio12
  )
)+
  geom_point(
    size=3,
    alpha=0.8
  )+
  scale_color_viridis_c()+
  theme_minimal()+
  labs(
    title="Germany Annual Rainfall Distribution",
    x="Longitude",
    y="Latitude",
    color="Precipitation"
  )




##agaer code
############################################################
# Extract auxiliary JSON files
############################################################

json_files <- files[
  grepl(
    "json",
    files,
    ignore.case=TRUE
  )
]


print(length(json_files))


environment_data <- lapply(
  json_files,
  function(f){
    
    untar(
      tar_file,
      files=f,
      exdir=tempdir()
    )
    
    json_path <- file.path(
      tempdir(),
      f
    )
    
    fromJSON(json_path)
    
  }
)


############################################################
# Create dataframe
############################################################

environment_df <- bind_rows(environment_data)


print(names(environment_df))

head(environment_df)











############################################################
# GlobalGeoTree Soil Moisture + Rainfall Visualization
# HuggingFace WebDataset streaming
############################################################

packages <- c(
  "httr",
  "jsonlite",
  "dplyr",
  "ggplot2",
  "viridis"
)

install.packages(
  setdiff(packages, installed.packages()[,"Package"])
)

library(httr)
library(jsonlite)
library(dplyr)
library(ggplot2)
library(viridis)


############################################################
# 1. DOWNLOAD DATA TAR FROM HUGGINGFACE
############################################################

url <- "https://huggingface.co/datasets/yann111/GlobalGeoTree/resolve/main/GlobalGeoTree-10kEval.tar"

tar_file <- tempfile(fileext=".tar")

GET(
  url,
  write_disk(tar_file, overwrite=TRUE)
)


############################################################
# 2. LIST JSON FILES INSIDE TAR
############################################################

files <- untar(
  tar_file,
  list=TRUE
)

json_files <- files[
  grepl(
    "json",
    files
  )
]


############################################################
# 3. EXTRACT JSON AND READ ENVIRONMENT DATA
############################################################

environment_data <- lapply(
  json_files,
  function(f){
    
    untar(
      tar_file,
      files=f,
      exdir=tempdir()
    )
    
    json_path <- file.path(
      tempdir(),
      f
    )
    
    fromJSON(json_path)
    
  }
)


environment_df <- bind_rows(environment_data)
names(environment_df)
str(environment_df)

############################################################
# 4. CHECK AVAILABLE VARIABLES
############################################################

print(names(environment_df))


############################################################
# 5. REMOVE EMPTY VALUES
############################################################

environment_df <- environment_df %>%
  filter(
    !is.na(latitude),
    !is.na(longitude),
    !is.na(soil_moisture_0_5cm),
    !is.na(bio12)
  )


############################################################
# 6. FILTER GERMANY
############################################################

germany <- environment_df %>%
  filter(
    longitude >= 5.8,
    longitude <= 15.1,
    latitude >= 47.2,
    latitude <= 55.1
  )


############################################################
# 7. SOIL MOISTURE HEATMAP
############################################################

ggplot(
  germany,
  aes(
    x=longitude,
    y=latitude,
    color=soil_moisture_0_5cm
  )
)+
  geom_point(
    size=3,
    alpha=0.8
  )+
  scale_color_viridis_c()+
  theme_minimal()+
  labs(
    title="Germany Soil Moisture Distribution",
    x="Longitude",
    y="Latitude",
    color="Soil Moisture (0-5cm)"
  )



############################################################
# 8. RAINFALL HEATMAP
# bio12 = Annual precipitation
############################################################

ggplot(
  germany,
  aes(
    x=longitude,
    y=latitude,
    color=bio12
  )
)+
  geom_point(
    size=3,
    alpha=0.8
  )+
  scale_color_viridis_c()+
  theme_minimal()+
  labs(
    title="Germany Annual Rainfall Distribution",
    x="Longitude",
    y="Latitude",
    color="Annual Rainfall"
  )

############################################################
# END
############################################################