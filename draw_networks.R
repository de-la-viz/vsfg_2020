# Draw the networks.

library(tidyverse)
library(rio)
library(glue)
library(lubridate)
library(stringr)
library(maps)
library(tidygraph)
library(ggraph)
library(assertthat)

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
  filter( # the left join added some annoying duplicates because a few cities have the same name and country...
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
  ) %>% 
  select(-pop, -capital)

# Quality checks:
# vol_cities %>% filter(is.na(pop)) %>% View()
#vol_cities %>% group_by(ID) %>% filter(n()>1) %>% View()


# We now have a dataframe with all the volunteers city and their lat/lon data.



# --- Build graph

current_proj <- vol_cities %>% filter(Project == "Kiron")

# nodes list:
city_count <- current_proj %>% group_by(City) %>% count()
nodes <- current_proj %>%
  left_join(city_count, by = 'City') %>%
  select(City, x = long, y = lat, weight = n) %>%
  unique()

# build edge list:
edges <- current_proj %>%
  mutate(to = paste(nodes$City, collapse = ',')) %>%
  separate_rows(to, sep = ',') %>%
  unique() %>% # because we want only one edge each, we will add the weight later with city_count
  select(from = City, to) %>%
  filter(from != to) # we are only interested in drawing edges between cities
  
graph <- as_tbl_graph(edges, directed = FALSE, vertices = nodes$City)

# create layout of graph to overaly on world map:
# lay <- ggraph::layout_tbl_graph_manual(graph, 'manual', x = nodes$x, y = nodes$y)
lay <- ggraph::create_layout(graph, layout = 'manual', x = nodes$x, y = nodes$y)

lay$weight <- nodes$weight

assert_that(nrow(lay) == nrow(nodes))


# --- Define world map

maptheme <- theme(panel.grid = element_blank()) +
  theme(axis.text = element_blank()) +
  theme(axis.ticks = element_blank()) +
  theme(axis.title = element_blank()) +
  theme(legend.position = "bottom") +
  theme(panel.grid = element_blank()) +
  theme(panel.background = element_rect(fill = "#596673")) +
  theme(plot.margin = unit(c(0, 0, 0.5, 0), 'cm'))

country_shapes <- geom_polygon(aes(x = long, y = lat, group = group),
                               data = map_data('world'),
                               fill = "#CECECE", color = "#515151",
                               size = 0.15)
mapcoords <- coord_fixed(xlim = c(-150, 180), ylim = c(-55, 80))


# --- Draw network


ggraph(lay) + 
  country_shapes + 
  geom_edge_link(alpha = 0.1) + 
  geom_node_point(aes(size = log(weight)+1), 
                  shape = 21,
                  fill = "white", color = "black") + 
  theme_graph() 

# TO DO:
#   - styling, look at my poster
#   - better map projection? 
#   - do it in a function, or better: try to facet it! 
#   - better scale for the weight. less difference between small and large
#   - update the data, use the project id



