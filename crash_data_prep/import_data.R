library(lubridate)
library(tidyverse)
library(raster)

years = 2008:2015
file_names <- (paste0("data/raw/",years, "/tblCrash.csv"))

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

# df <- read_csv(file_names[1])
# df <- df %>% janitor::clean_names(.)
# df <- df %>%
#   mutate(
#     crash_date = dmy(crash_date)
#   )

### NOTE: FOR NOW, DROPPING ALL OBS WITHOUT GEO-COORDINATES. MIGHT TRY TO IMPROVE THIS W/ ADDRESS LOOKUP LATER
load_and_clean_crash_data_file <- function(year){
  fileName <- (paste0("data/raw/",year, "/tblCrash.csv"))
  print(paste0("Parsing year: ", year))

  df <- read_csv(fileName)

  df <- df %>%
    janitor::clean_names(.) %>%
    mutate(
      crash_date = dmy(crash_date)
    ) %>%
    dplyr::filter(!is.na(x_coordinate) & !is.na(y_coordinate)) %>%
    convertNAD83Coordinates()

  if("rmv_crash_number" %in% colnames(df)){
    df <- df %>%
      dplyr::rename(
        crash_number = rmv_crash_number
      )
  }

  return(df)
}

df_2013 <- load_and_clean_crash_data_file(2013)

load_clean_and_extract_minimal_crash_data_file <- function(year){
  fileName <- (paste0("data/raw/",year, "/tblCrash.csv"))
  print(paste0("Parsing year: ", year))
  df <- read_csv(fileName)

  df <- df %>%
    janitor::clean_names(.) %>%
    mutate(
      crash_date = dmy(crash_date)
    ) %>%
    dplyr::filter(!is.na(x_coordinate) & !is.na(y_coordinate)) %>%
    convertNAD83Coordinates()

  if("rmv_crash_number" %in% colnames(df)){
    df <- df %>%
      dplyr::rename(
        crash_number = rmv_crash_number
      )
  }

  df <- df %>%
    dplyr::select(
      crash_number,
      crash_date,
      city_town_name,
      latitude,
      longitude
    )
  return(df)
}

df_2012_sm <- load_clean_and_extract_minimal_crash_data_file(2012)
df_2015_sm <- load_clean_and_extract_minimal_crash_data_file(2015)


minimal_crash_df <- years %>%
  map(~ load_clean_and_extract_minimal_crash_data_file(.)) %>%
  reduce(bind_rows) %>%
  distinct(.)

save(minimal_crash_df, file= "data/agg_crash_locations.rdata")

full_crash_df <- years %>%
  map(~ load_and_clean_crash_data_file(.)) %>%
  reduce(bind_rows) %>%
  distinct(.)

save(full_crash_df, file= "data/agg_crash_data.rdata")



