library(rgdal)
library(sp)
library(leaflet)
library(tidyverse)
library(lubridate)

#### EXAMPLE OF HOW TO CONVERT COORDS FROM Massachusetts Mainland State Plane NAD 83 meters to WGS84 Latitude/Longitude
#
# nad83_coords <- data.frame(x = 243176.138,y =862022.725)
# coordinates(nad83_coords) <- c('x', 'y')
# proj4string(nad83_coords) <- CRS("+init=epsg:26986")
# spTransform(nad83_coords,CRS("+init=epsg:4326"))

##### Actual Data ##########

convertNAD83Coordinates <- function(df){
  nad83_coords <- SpatialPoints(data.frame(x = df$x_coordinate, y = df$y_coordinate))
  proj4string(nad83_coords) <- CRS("+init=epsg:26986")
  long_lat_coords <- spTransform(nad83_coords,CRS("+init=epsg:4326")) %>%
    as.tibble() %>%
    rename(
      latitude = y,
      longitude = x
    )

  df <- bind_cols(df,long_lat_coords)
  return(df)
}

df <- read_csv("data/2008/tblCrash.csv")
df <- df %>% janitor::clean_names(.)
df <- df %>%
  mutate(
    crash_date = dmy(crash_date)
  ) %>%
  dplyr::filter(!is.na(x_coordinate) & !is.na(y_coordinate)) %>%
  convertNAD83Coordinates()

df_sm <- df[1:100,]


# df_sm <- df_2010

# pal <- colorFactor(c("#FF4040", "#006400", "#104E8B", "#FFC125", "#68228B", "#838B8B"), unique(df_sm$manner_of_collision))
pal <- colorFactor(palette(),unique(df_sm$manner_of_collision))

m <- leaflet(df_sm) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~longitude, lat = ~latitude,
    radius = ~ (total_nonfatal_injuries*5 + 5),
    color = ~pal(manner_of_collision),
    stroke = FALSE, fillOpacity = 0.5
  ) %>%
  addLegend("bottomright", pal = pal, values = ~manner_of_collision, title = "Manner of Collision")

m

