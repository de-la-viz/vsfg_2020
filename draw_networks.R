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
         Project_ID = `Project ID`,
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
      Country == "USA" & City == "Cambridge" ~ "Boston", # Argh! But, so close and solve duplicate name issue for graph with Cambridge UK
      City == "Springfield" ~ "Washington", # Sorry, too many Springfields in the US and cannot find author's bio
      Country == "Maroeuil" ~ "Lens", # there is one inversion country/city, and Maroeuil is a small town near Lens
      TRUE ~ City
    ),
    Country = case_when(
      ID == 19 | ID == 57 ~ "USA",
      Country == "Maroeuil" ~ "France",
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
# vol_cities %>% group_by(ID) %>% filter(n()>1) %>% View()


# We now have a dataframe with all the volunteers city and their lat/lon data.



# --- Build graph

current_proj <- vol_cities %>% filter(Project == "Kiron")

# nodes list:
city_count <- current_proj # %>% group_by(City) %>% count()
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


# --- Define the theme for the plots (background and text)

light <- "white" # color
dark <- "#383e42" # color of background
vfsg_green <- "#51C2C8"
my_font <- "Libre Franklin" # warning: the font of the annotations is handled separatly in geom_text()

my_theme <- function() {
  theme_minimal() +
    theme(
      text = element_text(size = 4, color = light, family = my_font),
      plot.title = element_text(size = 10, color = light, family = my_font), 
      strip.text.x = element_text(size = 4, color = light, family = my_font), # titles for facet_wrap
      plot.background = element_rect(fill = dark, 
                                     color = dark),
      legend.position = "none",
      panel.grid = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank()
    )
}

# --- Define world map

world <- map_data("world")
world <- world[world$region != "Antarctica",]
country_shapes <- geom_polygon(aes(x = long, y = lat, group = group),
                               data = world,
                               fill = dark, color = light,
                               size = 0.05)
mapcoords <- coord_fixed(xlim = c(-150, 180), ylim = c(-55, 80))

# --- Draw network

ggraph(lay) + 
  country_shapes +
  coord_sf() +
  # mapcoords +
  geom_edge_link(alpha = 0.05, color = vfsg_green) + 
  geom_node_point(aes(size = weight), 
                  shape = 21,
                  fill = vfsg_green, color = light,
                  show.legend = FALSE) + 
  my_theme() +
  scale_size_continuous(range = c(3,5))


# TO DO:
#   - better map projection? -> cannot make Robinson work
#   - do it in a function, facetting not possible.
#   - update the data, use the project id
#   - plot in a for loop with project id, save each
#   - how to arrange all the plots? manually or better way?








# ------------------------------------------------
#  TRY TO PLOT WITH ROBINSON PROJECTION, BUT FAILING MISERABLY:

# 
# library(rgeos)    # package rgeos required for finding out which hole belongs to which exterior ring
# library(lwgeom)    # for st_transform_proj()
# library(rworldmap) # for getMap()
# library(sf)        # for manipulation of simple features objects
# 
# world_sf <- st_as_sf(getMap(resolution = "low"))
# crs_wintri <- "+proj=wintri +datum=WGS84 +no_defs +over"
# world_wintri <- st_transform_proj(world_sf, crs = crs_wintri)
# 
# ggplot() + 
#   geom_sf(data = world_wintri, color = "black", size = 0.5/.pt) +
#   geom_sf(data = graph %>% activate(edges) %>% as_tibble() %>% st_as_sf(), col = 'grey50') + 
#   geom_sf(data = graph %>% activate(nodes) %>% as_tibble() %>% st_as_sf(), aes(col = betweenness, size = betweenness))
#   coord_sf(datum = NULL) +
#   geom_edge_link(alpha = 0.1) + 
#   geom_node_point(aes(size = log(weight) + 1), 
#                   shape = 21,
#                   fill = "white", color = "black",
#                   show.legend = FALSE)
# 
# 
# 
# 
# 
#   # coord_map("albers", lat0=0, lat1=50)
#   # coord_map("mollweide", xlim = c(-160, 180), ylim = c(-55, 80))
#   # coord_map("gilbert", xlim = c(-160, 180), ylim = c(-55, 80)) # not good, too north european
#   # coord_map("gnomonic") # BAD
#   # coord_map("lambert", 0, 20, xlim = c(-160, 180), ylim = c(-45, 75)) # not good, too north european
#   # coord_fixed(xlim = c(-160, 180), ylim = c(-55, 80))
#   # coord_fixed()
#   # coord_proj("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
#   # coord_map("albers", lat0=30, lat1=40)
#   # coord_sf(crs = "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs ")
# 
# # TO DO:
# #   - styling, look at my poster
# #   - better map projection? 
# #   - do it in a function, or better: try to facet it! 
# #   - better scale for the weight. less difference between small and large
# #   - update the data, use the project id
# 
# 
# library(ggplot2)
# library(ggalt)
# library(ggthemes)
# # devtools::install_github("eliocamp/ggalt@new-coord-proj", force = TRUE)
# 
# wrld <- map_data("world")
# 
# gg <- ggplot()
# gg <- gg + geom_map(data=wrld, map=wrld,
#                     aes(x=long, y=lat, map_id=region),
#                     color="#2b2b2b", size=0.15, fill=NA)
# gg <- gg + coord_proj("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
# gg <- gg + theme_map()
# gg
# 
# 
# world <- map_data("world")
# ## 
# ## Attaching package: 'maps'
# ## The following object is masked from 'package:purrr':
# ## 
# ##     map
# world <- world[world$region != "Antarctica",]
# 
# gg <- ggplot()
# gg <- gg + geom_cartogram(data=world, map=world,
#                           aes(x=long, y=lat, map_id=region))
# gg <- gg + ggalt::coord_proj("+proj=wintri")
# gg
# 
# 
# library(sf)
# data(world, package = 'spData')
# 
# world %>%
#   ggplot() +
#   geom_sf(aes(geometry - geom)) +
#   scale_x_continuous(breaks = c(180, -180)) +
#   scale_y_continuous(breaks = c(89.99)) +
#   theme_void +
#   coord_sf(crs = "+proj=robin")
# 
# 
# library(sf)
# library(maps) 
# w = st_as_sf(map('world', plot = FALSE, fill = TRUE))
# w2 = (st_geometry(w) + c(360,90)) %% c(360) - c(0,90)
# plot(w, axes = TRUE)
# 
# library(sf)
# library(maptools)
# data("wrld_simpl")
# my_map_proj <- wrld_simpl %>%
#   st_as_sf() %>%
#   st_transform(crs = "+proj=robin")
# 
# ggraph(lay) + 
#   geom_sf(aes(geometry = geometry)) +
#   geom_edge_link(alpha = 0.1) + 
#   geom_node_point(aes(size = log(weight) + 1), 
#                   shape = 21,
#                   fill = "white", color = "black",
#                   show.legend = FALSE) + 
#   theme_graph() 
# 
# 
# 
# ggplot() +
#   geom_sf(aes(geometry = geometry), data=my_map_proj) +
#   geom_point(aes(x=LON, y=LAT), data = lay %>% select(LON = x, LAT = y)) +
#   coord_sf(expand=TRUE) +
#   # geom_sf(data = graph %>% activate(edges) %>% as_tibble() %>% st_as_sf(), col = 'grey50') + 
#   # geom_sf(data = graph %>% activate(nodes) %>% as_tibble() %>% st_as_sf()) +
#   geom_edge_link(alpha = 0.1, data = lay)
#   scale_colour_viridis_c(option = 'inferno') +
#   scale_size_continuous(range = c(0,4))

