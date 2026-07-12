install.packages(c("httr","jsonlite","dplyr","ggplot2","viridis","sf","rnaturalearth","rnaturalearthdata"), repos="https://cloud.r-project.org")

library(httr);library(jsonlite);library(dplyr);library(ggplot2);library(viridis);library(sf);library(rnaturalearth);library(rnaturalearthdata)

url <- "https://huggingface.co/datasets/yann111/GlobalGeoTree/resolve/main/GlobalGeoTree-10kEval/GGT-10kEval-900.tar"

tar_file <- tempfile(fileext=".tar")

GET(url,write_disk(tar_file,overwrite=TRUE))

files <- untar(tar_file,list=TRUE)

json_files <- files[grepl("auxiliary.json",files)]

environment_df <- bind_rows(lapply(json_files,function(f){untar(tar_file,files=f,exdir=tempdir());x<-fromJSON(file.path(tempdir(),f));if(x$longitude>5.8 && x$longitude<15.1 && x$latitude>47.2 && x$latitude<55.1)x else NULL}))

germany_map <- ne_countries(scale="medium",country="Germany",returnclass="sf")

ggplot()+
  geom_sf(data=germany_map,fill="grey90")+
  geom_point(data=environment_df,aes(longitude,latitude,color=soil_moisture_0_5cm),size=2)+
  scale_color_viridis_c()+
  theme_minimal()+
  labs(title="Germany Soil Moisture Map",color="Soil Moisture")

ggplot()+
  geom_sf(data=germany_map,fill="grey90")+
  geom_point(data=environment_df,aes(longitude,latitude,color=bio12),size=2)+
  scale_color_viridis_c()+
  theme_minimal()+
  labs(title="Germany Rainfall / Water Availability Map",color="Annual Rainfall")