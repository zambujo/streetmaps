library(tidyverse)
library(osmdata)
# available_features()


# Gather data -------------------------------------------------------------

my_place <- "Monchique Portugal"

# available_tags("highway")
streets <-
  my_place %>%
  getbb() %>%
  opq() %>%
  add_osm_feature(
    key = "highway",
    value = c(
      "motorway",
      "primary",
      "secondary",
      "tertiary")) %>%
  osmdata_sf()
small_streets <- 
  my_place %>%
  getbb() %>%
  opq() %>%
  add_osm_feature(
    key = "highway",
    value = c(
      "residential",
      "living_street",
      "unclassified",
      "service",
      "footway")) %>%
  osmdata_sf()

# available_tags("building")
houses <- getbb(my_place) %>%
  opq() %>%
  add_osm_feature(key = "building") %>%
  osmdata_sf()


# Plotting ----------------------------------------------------------------

xy_limits <-
  pluck(streets, "bbox") %>%
  str_split(",") %>%
  flatten_chr() %>%
  as.numeric()

ggplot() +
  geom_sf(
    data = streets$osm_lines,
    inherit.aes = FALSE,
    color = "#393b44",
    size = .8,
    alpha = .3
  ) +
  geom_sf(
    data = small_streets$osm_lines,
    inherit.aes = FALSE,
    color = "#d6e0f0",
    # "#ffbe7f",
    size = .8,
    alpha = .5
  ) +  geom_sf(
    data = houses$osm_polygons,
    inherit.aes = FALSE,
    color = "#8d93ab",
    size = .2,
    alpha = .3
  ) +  coord_sf(
    xlim = c(-8.565,-8.547),
    ylim = c(37.308, 37.325),
    expand = FALSE
  ) + theme_void() + theme(plot.background = element_rect(fill = "white"))


# Save as -----------------------------------------------------------------

ggsave("monchique_raw.svg", width = 5, height = 7)
