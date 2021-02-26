library(raster)
library(here)
library(sf)
library(fasterize)
library(leaflet)
library(tidyverse)

corridor_raster <- raster(here('corridors.tif'))

plot(corridor_raster)

leaf <- leaflet() %>%
  setView(lng = 28.45861, lat = -3.05, zoom = 8.5) %>%
  addProviderTiles(providers$Esri.WorldTopoMap) %>%
  addRasterImage(corridor_raster)

region_shp <- st_read(here("study_region","study_region_merge.shp"))

plot(region_shp)

st_crs(region_shp)
# ID["EPSG",32735]]

region_transform <- region_shp %>%
  st_transform(crs('+proj=longlat +epsg=3857'))

st_crs(region_transform)

study_region_df <- read_sf(here("study_region"), layer = "study_region_merge") %>%
  clean_names() %>%
  as.data.frame()

region_extent <- extent(region_transform)
region_raster <- raster(region_extent)

leaflet() %>% addTiles() %>%
  addRasterImage(r, colors = pal, opacity = 0.8) %>%
  addLegend(pal = pal, values = values(r),
            title = "Surface temp")

leaflet() %>%
  setView(lng = 28.45861, lat = -3.05, zoom = 8.5) %>%
  addProviderTiles(providers$Esri.WorldTopoMap) %>%
  addRasterImage(region_raster)




sbc_rast <- raster(here('data/county.tif'))

cetacean_stack <- raster::stack(cetacean_files)
