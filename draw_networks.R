# Draw the networks.

library(tidyverse)
library(rio)
library(glue)
library(lubridate)
library(stringr)
library(maps)
library(tidygraph)
library(ggraph)

subm <- rio::import("vfsg_submissions.csv") %>% 
  mutate(ProjectDate = format(lubridate::parse_date_time(`Date of project`, orders = c("my")), "%Y-%m"),
         Volunteer = as.factor(`Name of volunteer`),
         URL = ifelse(stringr::str_detect(string = URL, pattern = "http"), URL, NA),
         `Volunteer City` = ifelse(stringr::str_detect(string = `Volunteer City`, pattern = "nknown"), NA, `Volunteer City`),
         `Volunteer Country` = ifelse(stringr::str_detect(string = `Volunteer Country`, pattern = "nknown"), NA, `Volunteer Country`)
  )

# to avoid a more complicated geolocalization, we just use this dataset
world_cities <- maps::world.cities %>% rename(City = name, Country = country.etc)
#   note: in retrospect, it was probably more annoying and not the good solution


# --- Geolocalization and Cleaning

# we have to clean a bit the geolocalization data,
#   also so that we can use world_cities
vol_cities_geo <- subm %>%
  select(ID = ID,
         Project = `Name of charity/Project`, 
         Country = `Volunteer Country`,
         City = `Volunteer City`
  ) %>%
  drop_na(Country) %>% # some cases with Country but without cities, we try to keep them and default to capital city
  mutate( # general cleanings
    City = case_when(
      City == "Bengaluru" ~ "Bangalore",
      City == "Washington DC" ~ "Washington",
      City == "London," ~ "London",
      City == "Arizona" ~ "Phoenix",
      City == "Mumbai" ~ "Bombay", # sorry, the world_cities is old it seems...
      City == "Selangor" ~ "Kuala Lumpur", # Selangor is the state where Kuala Lumpur is
      City == "Dalston" ~ "London", # Dalston is in East London
      City == "Münster" ~ "Munster", 
      City == "Luxembourg" ~ "Luxemburg", 
      City == "Minnesota" ~ "Minneapolis",  # largest city of state
      City == "New Jersey" ~ "New York",  # sorry (but will look better on world map!)
      City == "Maryland" ~ "Washington", # approximattion
      City == "Derbyshire" ~ "Derby",
      City == "Shibuya" ~ "Tokyo", # neighborhood
      City == "Georgia" ~ "Atlanta",  # largest city of state
      City == "Kawasaki-shi" ~ "Kawasaki", 
      City == "München" ~ "Munich", 
      City == "Praha" ~ "Prague", 
      City == "Ahmedabad" ~ "Ahmadabad", 
      City == "Bali" ~ "Denpasar", # largest city on Bali
      City == "Irvine, CA" ~ "Los Angeles",
      TRUE ~ City
    ),
    Country = case_when(
      Country == "United States" ~ "USA",
      Country == "United Kingdom" ~ "UK",
      Country == "England" ~ "UK",
      Country == "US" ~ "USA",
      
      TRUE ~ Country
    )
  ) %>%
  mutate( # specific cleanings (e.g. several US town with same name, or unknown town)
    City = case_when(
      City == "Gig Harbor" ~ "Seattle", # approximattion (based on author's bio)
      City == "Grafton" ~ "Milwaukee", # approximattion (based on author's bio) 
      City == "Williamsport" ~ "State College", # approximattion (based on author's bio) 
      City == "Milledgeville" ~ "Atlanta", # approximattion (based on author's bio) 
      Country == "Japan" & City == "" ~ "Tokyo", 
      City == "Canonsburg" ~ "Pittsburgh", # approximattion (based on author's bio)
      Country == "UK" & is.na(City) ~ "London", 
      Country == "USA" & is.na(City) ~ "Washington", 
      Country == "Japan" & is.na(City) ~ "Tokyo", 
      Country == "Canada" & is.na(City) ~ "Ottawa", 
      City == "Springfield" ~ "Washington", # Sorry, too many Springfields in the US and cannot find author's bio
      TRUE ~ City
    ),
    Country = case_when(
      ID == 19 | ID == 57 ~ "USA",
      TRUE ~ Country
    )
  )

## --- geolocalize:

vol_cities <- vol_cities_geo %>% 
  left_join(world_cities, by = c('City', 'Country')) %>%
  filter( # the left join added some annoying ducplicates because a few cities have the same name and country...
    !(City == 'Munster' & lat == 53.00), # we assume the city and not small village
    !(City == 'Richmond' & lat == 37.95),  # we assume in Virginia
    !(City == 'Columbia' & lat == 38.95),  # we assume largest city
    !(City == 'Columbia' & lat == 39.20),  # we assume largest city
    !(City == 'Kawasaki' & lat == 33.58),  # we assume largest city
    !(City == 'Frankfurt' & lat == 52.34),  # we assume largest city
    !(City == 'Bloomington' & lat == 40.48),  # based on author's bio => Indiana
    !(City == 'Bloomington' & lat == 44.83),  # based on author's bio => Indiana
    !(City == 'Portland' & lat == 43.66),  # we assume largest city
    !(City == 'Jacksonville' & lat == 34.76),  # we assume largest city
    !(City == 'Roseville' & lat == 42.51),  # we assume largest city
    !(City == 'Jacksonville' & lat == 34.76),  # we assume largest city
    
  )

# Quality checks:
# vol_cities %>% filter(is.na(pop)) %>% View()
#vol_cities %>% group_by(ID) %>% filter(n()>1) %>% View()


# We now have a dataframe with all the volunteers city and their lat/lon data.



# --- Build graph



# --- Draw network







