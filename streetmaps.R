library(tidyverse)
library(osmdata)
theme_set(theme_minimal())


# Gather data -------------------------------------------------------------

bb <- "Monchique, Portugal" %>%
  getbb()

main_highway_tags <- c(
  "motorway",
  "primary",
  "secondary",
  "tertiary")

other_highway_tags <- 
  available_tags("highway") %>%
  setdiff(main_highway_tags)

get_simple_features <- function(bounding_box,
                                feature,
                                tags,
                                what="osm_lines") {
  if (missing(feature)) 
    stop("key must be provided")
  if (missing(tags)) 
    tags <- NULL
  bounding_box %>%
    opq() %>%
    add_osm_feature(
      key = feature,
      value = tags) %>%
    osmdata_sf() %>%
    pluck(what)
}

## query the API
main_streets <- get_simple_features(bb, "highway", main_highway_tags)
other_streets <- get_simple_features(bb, "highway", other_highway_tags)
buildings <- get_simple_features(bb, "building", what = "osm_polygons")

## more features:  
## available_features()


# Plotting ----------------------------------------------------------------

p <- ggplot(data = main_streets)

p <- p +
  geom_sf(
    color = "#393b44",
    inherit.aes = FALSE,
    size = .5)

p <- p +
  geom_sf(
    color = "#393b44",
    size = .3,
    inherit.aes = FALSE,
    data = other_streets)

p <- p +
  geom_sf(
    color = "#8d93ab",
    size = .2,
    inherit.aes = FALSE,
    data = buildings)


p <- p + 
  coord_sf(
    xlim = c(-8.565,-8.547),
    ylim = c(37.308, 37.325),
    expand = FALSE) +
  theme_void() + 
  theme(plot.background = element_rect(fill = NA))


# Save as -----------------------------------------------------------------

ggsave("raw.svg", p, width = 5, height = 7)
