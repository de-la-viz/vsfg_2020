# Draw the networks.

library(tidyverse) # data manipulation and plot
library(rio) # load data
library(maps) # world cities localization
library(tidygraph) # to create graph/network
library(ggraph) # to plot the graph/network
library(glue) # "paste character" for plot titles and saving
library(lubridate) # transform date to good format. In the end only used for sorting
library(stringr) # used to clean some character data
library(cowplot) # to display all the network plots in one page
library(ggtext) # add box of wrapped text on plot (title, description, call to action, credits)


# --- Data Loading

subm <- rio::import("data/vfsg_submissions.csv") %>% 
  rename(Project = `Name of charity/Project`,
         Project_ID = `Project ID`) %>%
  mutate(ProjectDate = format(lubridate::parse_date_time(`Date of project`, orders = c("my")), "%Y-%m"),
         Volunteer = as.factor(`Name of volunteer`),
         URL = ifelse(stringr::str_detect(string = URL, pattern = "http"), URL, NA),
         `Volunteer City` = ifelse(stringr::str_detect(string = `Volunteer City`, pattern = "nknown"), NA, `Volunteer City`),
         `Volunteer Country` = ifelse(stringr::str_detect(string = `Volunteer Country`, pattern = "nknown"), NA, `Volunteer Country`)
  ) %>% 
  arrange(ProjectDate) # projects are in date order, important for the viz.

# store projects infos:
projects_info <- subm %>% # handle the orgs with which 2 projects were made:
  group_by(Project_ID) %>% 
  summarise(n_submission = n()) %>% 
  left_join(subm %>% select(Project, `Date of project`, ProjectDate, Project_ID), by = "Project_ID") %>%
  unique() %>%
  arrange(ProjectDate) %>%
  mutate(
    Project = case_when(
      Project == "UNDP" & Project_ID == 10 ~ "UNDP (1)",
      Project == "UNDP" & Project_ID == 19 ~ "UNDP (2)",
      Project == "Kiron" & Project_ID == 24 ~ "Kiron (1)",
      Project == "Kiron" & Project_ID == 30 ~ "Kiron (2)",
      TRUE ~ Project
    )
  )

# to avoid a more complicated geolocalization, we just use this dataset
world_cities <- maps::world.cities %>% rename(City = name, Country = country.etc)
#   note: in retrospect, it was probably more annoying and not the good solution


# --- Geolocalization and Cleaning

# we have to clean a bit the geolocalization data,
#   also so that we can use world_cities
vol_cities_geo <- subm %>%
  select(ID = ID,
         Project_ID,
         Project, 
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
  ) %>% # handle the orgs with which 2 projects were made:
  mutate(
    Project = case_when(
      Project == "UNDP" & Project_ID == 10 ~ "UNDP (1)",
      Project == "UNDP" & Project_ID == 19 ~ "UNDP (2)",
      Project == "Kiron" & Project_ID == 24 ~ "Kiron (1)",
      Project == "Kiron" & Project_ID == 30 ~ "Kiron (2)",
      TRUE ~ Project
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


# some basic stats (used in the description);
n_cities <- vol_cities$City %>% n_distinct
n_countries <- vol_cities$Country %>% n_distinct
n_volunteers <- subm$Volunteer %>% n_distinct


# --- Define the theme for the plots (background and text)

light <- "white" # color of text and plot titles
dark <- "#383e42" # color of background
vfsg_green <- "#51C2C8"
my_font <- "Libre Franklin" 
min_dot <- 1 # min size of nodes
max_dot <- 3 # max size of nodes

my_theme <- function() {
  theme_minimal() +
    theme(
      text = element_text(size = 4, color = light, family = my_font),
      plot.title = element_text(size = 5, color = light, family = my_font), 
      plot.background = element_rect(fill = dark, 
                                     color = dark),
      legend.position = "none",
      panel.grid = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank()
    )
}


# --- Define world map

world <- map_data("world") %>% filter(region != "Antarctica")
world <- world[world$region != "Antarctica",]
mapcoords <- coord_fixed(xlim = c(-150, 180), ylim = c(-55, 80)) # crop the earth = larger map


# --- Draw network

draw_network <- function(project_name){
  # a fct to draw the network for one project.
  
  # get project info:
  n_submissions <- projects_info %>% filter(Project == project_name) %>% .$n_submission
  p_date <- projects_info %>% filter(Project == project_name) %>% 
    .$`Date of project` %>% str_replace("-", ". 20") # nicer display of date
  
  # --- Build graph
  current_proj <- vol_cities %>% filter(Project == project_name)
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
  # create graph from edges, add nodes info:
  graph <- as_tbl_graph(edges, directed = FALSE, vertices = nodes$City)
  # create layout of graph to overaly on world map:
  lay <- ggraph::create_layout(graph, layout = 'manual', x = nodes$x, y = nodes$y)
  lay$weight <- nodes$weight # add the weight aka number of submission from the city
  
  # --- Plot
  ggnet <- ggraph(lay) + 
    geom_polygon(aes(x = long, y = lat, group = group),
                 data = world,
                 fill = 'black', color = 'black', # fill = dark, color = light,
                 size = 0.05) +
    # coord_sf() +
    mapcoords +
    geom_edge_link(alpha = 0.06, color = vfsg_green) + 
    geom_node_point(aes(size = weight), 
                    shape = 21,
                    fill = vfsg_green, color = 'black',
                    show.legend = FALSE) + 
    my_theme() +
    scale_size_continuous(range = c(min_dot, max_dot)) + 
    labs(title = glue("{project_name}"),
         subtitle = glue("{n_submissions} visualizations designed in {p_date}"))
  
  return(ggnet) # returns the plot, a gg object
}

# to test:
# p1 <- draw_network("Stanford University")
# p6 <- draw_network("Bridges to Prosperity")


# --- Draw all plots:
all_proj_names <- vol_cities$Project %>% unique()
all_plots = list()
for (proj in all_proj_names) {
  # loop through all projects and store their plot in a lis
  ggnet <- draw_network(proj)
  all_plots <- append(all_plots, list(ggnet))
}
# arrange all the plots in a grid:
main_plot <- cowplot::plot_grid(plotlist = all_plots, ncol = 4)

# add some space around, where we will have the title and a description:
main_plot <- main_plot + 
  theme(
    plot.margin = margin(t = 45, r = 15, b = 15, l = 15, unit = "mm"),
    plot.background = element_rect(color = dark, fill = dark)
  )

# define title and description
title_label = "<span style='font-size:9.8pt;'>viz for social good — The Growth of a Network</span><br><br><br>Viz for Social Good empowers mission-driven organizations to harness the power of data visualization for social change. Since January 2017, passionate data professionals collaborated with 29 different social organizations on 31 data visualization projects to tackle critical social issues. For each project, <span style='color:#51C2C8;'>an international network of volunteers originating from numerous cities</span> around the globe is formed. Today, the  Viz for Social Good network consists of 324 volunteers from 120+ cities in 33 countries."
my_title <- geom_textbox(
  data = tibble(
    x = 0.005,
    y = 1.02,
    label = title_label
  ),
  aes(x = x, y = y, label = label),
  inherit.aes = F,
  family = my_font,
  color = light,
  size = 2.2,
  lineheight = 1.8,
  width = unit(180, "mm"),
  fill = NA,
  box.colour = NA,
  hjust = 0,
  vjust = 0
)
# define credentials
cred_label <- "Data: Viz for Social Good — Design: François Delavy — Contact: @f_delavy"
credentials <- geom_textbox(
  data = tibble(
    x = 0.5 - 30/210,
    y = 0,
    label = cred_label
  ),
  aes(x = x, y = y, label = label),
  inherit.aes = F,
  family = my_font,
  color = light,
  size = 1.5,
  lineheight = 1.7,
  width = unit(60, "mm"),
  fill = NA,
  box.colour = NA,
  hjust = 0,
  vjust = 2
)
# define call to acion
cta_label <- "... and the next project?<br>Join the network! <br>vizforsocialgood.com"
call_to_action <- geom_textbox(
  data = tibble(
    x = 0.8,
    y = 0.04,
    label = cta_label
  ),
  aes(x = x, y = y, label = label),
  inherit.aes = F,
  family = my_font,
  color = light,
  size = 2.2,
  lineheight = 2,
  width = unit(50, "mm"),
  fill = NA,
  box.colour = NA,
  hjust = 0,
  vjust = 0
)

# Draw them on the plot:
main_plot <- main_plot + my_title + credentials + call_to_action


# Save as PDF A4, portrait:
path <- "viz/vfsg_networks"
width = 210
height = 297
units = "mm"
ggsave(filename = glue::glue("{path}.pdf"), 
       plot = main_plot,
       device = cairo_pdf,
       width = width, 
       height = height, 
       units = units)
# embed the output font in the PDF:
extrafont::embed_fonts(glue::glue("{path}.pdf")) # takes time, so comment when testing

# Save as SVG:
ggsave(filename = glue::glue("{path}.svg"), 
       width = width,
       height = height, 
       units = units)

# Save as PNG:
ggsave(filename = glue::glue("{path}.png"), 
       width = width,
       height = height, 
       units = units,
       dpi = 400)



# --- Draw smaller version, with the 7 largest networks only:

# --- Select the 7 largest projects (in term of nb of submissions:
less_proj_names <- projects_info %>% arrange(-n_submission) %>% head(7) %>% 
  arrange(ProjectDate) %>% .$Project 
less_plots = list()
for (proj in less_proj_names) {
  # loop through all projects and store their plot in a list
  ggnet <- draw_network(proj)
  less_plots <- append(less_plots, list(ggnet))
}
# arrange all the plots in a grid:
smaller_plot <- cowplot::plot_grid(plotlist = less_plots, ncol = 4)

# add some space around, where we will have the title and a description:
smaller_plot <- smaller_plot + 
  theme(
    plot.margin = margin(t = 30, r = 5, b = 5, l = 10, unit = "mm"),
    plot.background = element_rect(color = dark, fill = dark)
  )



# --- we first focus the first viz on attracting volunteers:

# need to re-define title and description, to change the story, slightly:
title_label = "<span style='font-size:9.8pt;'>Viz for Social Good — An International Network</span><br><br>Viz for Social Good empowers mission-driven organizations to harness the power of data visualization for social change. Since January 2017, more than 320 passionate data professionals from 120+ cities in 33 countries collaborated with 29 different social organizations on 31 data visualization projects tackling critical social issues.<br>Participating in a project is joining <span style='color:#51C2C8;'>an international network of volunteers from numerous cities around the globe</span>. A selection:"

# Add title, description, cred and CTA on the plot:
volunteer_plot <- smaller_plot + # Title:
  geom_textbox(
    data = tibble(
      x = 0.006,
      y = 1.03,
      label = title_label
    ),
    aes(x = x, y = y, label = label),
    inherit.aes = F,
    family = my_font,
    color = light,
    size = 2.2,
    lineheight = 1.8,
    width = unit(190, "mm"),
    fill = NA,
    box.colour = NA,
    hjust = 0,
    vjust = 0
  ) + # Credentials:
  geom_textbox(
    data = tibble(
      x = 0.5 - 30/210,
      y = 0,
      label = cred_label
    ),
    aes(x = x, y = y, label = label),
    inherit.aes = F,
    family = my_font,
    color = light,
    size = 1.5,
    lineheight = 1.7,
    width = unit(60, "mm"),
    fill = NA,
    box.colour = NA,
    hjust = 0,
    vjust = 1
  ) + # CTA
  geom_textbox(
    data = tibble(
      x = 0.8,
      y = 0.2,
      label = cta_label
    ),
    aes(x = x, y = y, label = label),
    inherit.aes = F,
    family = my_font,
    color = light,
    size = 2.2,
    lineheight = 2,
    width = unit(50, "mm"),
    fill = NA,
    box.colour = NA,
    hjust = 0,
    vjust = 0
  )

# Save as PDF:
path <- "viz/vfsg_some_networks_for_volunteers"
width = 210
height = 100
units = "mm"
ggsave(filename = glue::glue("{path}.pdf"), 
       plot = volunteer_plot,
       device = cairo_pdf,
       width = width, 
       height = height, 
       units = units)
# embed the output font in the PDF:
extrafont::embed_fonts(glue::glue("{path}.pdf")) # takes time, so comment when testing

# Save as SVG:
ggsave(filename = glue::glue("{path}.svg"), 
       width = width,
       height = height, 
       units = units)

# Save as PNG:
ggsave(filename = glue::glue("{path}.png"), 
       width = width,
       height = height, 
       units = units,
       dpi = 400)


# --- ... and the second on attracting organizations:
# (only the description in 'title_label' is slighly modified)

# need to re-define title and description, to change the story, slightly:
title_label = "<span style='font-size:9.8pt;'>Viz for Social Good — An International Network</span><br><br>Viz for Social Good empowers mission-driven organizations to harness the power of data visualization for social change. Since January 2017, more than 320 passionate data professionals from 120+ cities in 33 countries collaborated with 29 different social organizations on 31 data visualization projects tackling critical social issues.<br>Collaborating with Viz for Social Good for a project is benefitting from <span style='color:#51C2C8;'>an international network of volunteers from numerous cities around the globe</span>. A selection:"
# and the Call to Action:
cta_label <- "... and the next project?<br>Contact us!<br>vizforsocialgood.com"

# Add title, description, cred and CTA on the plot:
orga_plot <- smaller_plot + # Title:
  geom_textbox(
    data = tibble(
      x = 0.006,
      y = 1.03,
      label = title_label
    ),
    aes(x = x, y = y, label = label),
    inherit.aes = F,
    family = my_font,
    color = light,
    size = 2.2,
    lineheight = 1.8,
    width = unit(190, "mm"),
    fill = NA,
    box.colour = NA,
    hjust = 0,
    vjust = 0
  ) + # Credentials:
  geom_textbox(
    data = tibble(
      x = 0.5 - 30/210,
      y = 0,
      label = cred_label
    ),
    aes(x = x, y = y, label = label),
    inherit.aes = F,
    family = my_font,
    color = light,
    size = 1.5,
    lineheight = 1.7,
    width = unit(60, "mm"),
    fill = NA,
    box.colour = NA,
    hjust = 0,
    vjust = 1
  ) + # CTA
  geom_textbox(
    data = tibble(
      x = 0.8,
      y = 0.2,
      label = cta_label
    ),
    aes(x = x, y = y, label = label),
    inherit.aes = F,
    family = my_font,
    color = light,
    size = 2.2,
    lineheight = 2,
    width = unit(50, "mm"),
    fill = NA,
    box.colour = NA,
    hjust = 0,
    vjust = 0
  )

# Save as PDF:
path <- "viz/vfsg_some_networks_for_organizations"
width = 210
height = 100
units = "mm"
ggsave(filename = glue::glue("{path}.pdf"), 
       plot = orga_plot,
       device = cairo_pdf,
       width = width, 
       height = height, 
       units = units)
# embed the output font in the PDF:
extrafont::embed_fonts(glue::glue("{path}.pdf")) # takes time, so comment when testing

# Save as SVG:
ggsave(filename = glue::glue("{path}.svg"), 
       width = width,
       height = height, 
       units = units)

# Save as PNG:
ggsave(filename = glue::glue("{path}.png"), 
       width = width,
       height = height, 
       units = units,
       dpi = 400)



# -------------- TRY TO PLOT WITH ROBINSON PROJECTION, BUT FAILING MISERABLY: ------
# All the code below is for my failed attempts at using a Robinson projection...
# So let's store them, even if it's a mess, because who knows...

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

# world_sf <- sf::st_as_sf(rworldmap::getMap(resolution = "low"))
# fribourg <- tibble(name = "Fribourg", lon = 7.1620, lat = 46.8065)
# 
# crs_robin <- "+proj=robin +lat_0=0 +lon_0=0 +x0=0 +y0=0"
# # projection outline in long-lat coordinates
# lats <- c(90:-90, -90:90, 90)
# longs <- c(rep(c(180, -180), each = 181), 180)
# robin_outline <- 
#   list(cbind(longs, lats)) %>%
#   st_polygon() %>%
#   st_sfc(
#     crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
#   ) %>% 
#   st_transform(crs = crs_robin)
# 
# # bounding box in transformed coordinates
# xlim <- c(-18494733, 18613795)
# ylim <- c(-9473396, 9188587)
# robin_bbox <- 
#   list(
#     cbind(
#       c(xlim[1], xlim[2], xlim[2], xlim[1], xlim[1]), 
#       c(ylim[1], ylim[1], ylim[2], ylim[2], ylim[1])
#     )
#   ) %>%
#   st_polygon() %>%
#   st_sfc(crs = crs_robin)
# # area outside the earth outline
# robin_without <- st_difference(robin_bbox, robin_outline)
# 
# fribourg_rob <- fribourg %>% 
#   st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
#   st_transform(crs = "+proj=robin")
# 
# lay_rob <- lay %>% 
#   st_as_sf(coords = c("x", "y"), crs = 4326) %>%
#   st_transform(crs = "+proj=robin")
# 
# 
# 
# ggplot(world_sf) + 
#   geom_sf(fill = "#E69F00B0", color = "black", size = 0.5/.pt) + 
#   geom_sf(data = robin_without, fill = "white", color = NA) +
#   geom_sf(data = robin_outline, fill = NA, color = "grey30", size = 0.5/.pt) +
#   scale_x_continuous(name = NULL, breaks = seq(-120, 120, by = 60)) +
#   scale_y_continuous(name = NULL, breaks = seq(-60, 60, by = 30)) +
#   coord_sf(xlim = 0.95*xlim, ylim = 0.95*ylim, expand = FALSE, crs = crs_robin, ndiscr = 1000) + 
#   ggtitle("Robinson") +
#   theme_minimal() +
#   theme(
#     panel.background = element_rect(fill = "#56B4E950", color = "white", size = 1),
#     panel.grid.major = element_line(color = "gray30", size = 0.25)
#   ) + 
#   geom_point(data = fribourg_rob, aes(
#     x = st_coordinates(fribourg_rob)[1], 
#     y = st_coordinates(fribourg_rob)[2]), 
#     col = 'red', size = 4) +
#   geom_node_point(data = lay_rob, 
#                   aes(x = st_coordinates(lay_rob)[,1],
#                       y = st_coordinates(lay_rob)[,2], 
#                       size = weight), 
#                   shape = 21,
#                   fill = vfsg_green, color = 'black',
#                   show.legend = FALSE) +
#   geom_edge_link(data = lay_rob, alpha = 0.05, color = vfsg_green)
#   





