library(purrr)
library(tidyverse)
library(httr)
library(jsonlite)
library(ggplot2)
library(DescTools)
library(magrittr)
library(rlang)
library(lubridate)
library(anytime)

# Problem 2

transform_metadata_to_df <- function() {
  stations_metadata[[1]] %>% 
    map(as_tibble) %>% 
    list_rbind() %>% 
    mutate(latestData = map_chr(latestData, 1, .default = NA_character_)) %>% 
    mutate(latestData = as_datetime(latestData, tz = "Europe/Berlin")) %>% 
    mutate(latestData = force_tz(latestData, tzone = "UTC")) %>% 
    mutate(location = map(location, unlist)) |>  
    mutate(
      lat = map_dbl(location, "latLon.lat"),
      lon = map_dbl(location, "latLon.lon")
    ) %>% 
    select(-location)
}

# Problem 4a

to_iso8601 <- function(datetime, offset_in_days) {
  # Change date input
  
  datetime <- as.POSIXct(datetime, tz = "UTC")

  datetime <- datetime + offset_in_days * 24 * 60 * 60
  
  # Formatin the datetime
  iso8601_datetime <- format(datetime, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  
  return(iso8601_datetime)
  
}


#Problem 5

transform_volumes <- function(json) {
  
  #Transform the list into a tibble
  json[[1]][[1]][[1]]  %>%
    as_tibble() %>%
    
    #Unnest all the columns
    unnest_wider(edges) %>%
    unnest_wider(node) %>%
    unnest_wider(total) %>%
    unnest_wider(volumeNumbers) %>%
    
    #Create "from" and "to" columns using datetime and add the "volume" column as numeric
    mutate(from = ymd_hms(from), to = ymd_hms(to), volume = as.numeric(volume))
  
}

#P
