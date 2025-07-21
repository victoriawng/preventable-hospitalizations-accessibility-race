library(ggplot2)
library(sf)
library(tigris)    # for US shapefiles
library(dplyr)


# Get US counties shapefile
options(tigris_use_cache = TRUE)
counties_sf <- counties(cb = TRUE, resolution = "5m", year = 2024, class = "sf")

bubble_data = plot_df|>
  select(GEOID, geometry, v005_rawvalue, largest_race_label) |>
  filter(!if_all(c(v005_rawvalue, largest_race_label), ~is.na(.)))
view(bubble_data)

vis_miss(bubble_data)
  
view(plot_df)



ggplot() +
  # counties outline
  geom_sf(data = counties_sf, fill = "grey95") +
  
  # add bubbles
  geom_sf(
    data = bubble_data,
    aes(size = v005_rawvalue, fill = largest_race_label),
    shape = 21,
    color = "grey20",
    alpha = 0.8
    ) +
  # scale bubble sizes (min/max)
  scale_size(
    range = c(1,9), name = "Preventable Stays"
    ) +
  scale_fill_brewer(palette = "Set2", name = "Race Group") +
  
  coord_sf(xlim = c(-125, -66), ylim = c(24, 50)) +
  
  # makes lat/lon lines disappear
  theme_void() +
  # theme(legend.position = "bottom")
  guides(
    guide_legend(override.aes = list(size =5)))
  
  # color gradient for fill
  # Creates a diverging color gradient from red to blue ("RdBu").
  # Reversed (rev = TRUE) so higher values are red by default.
  # Slight transparency (alpha = 0.9) for smoother layering.
  scale_fill_gradientn(
    colours = hcl.colors(5, "RdBu", rev = TRUE, alpha = 0.9)
  ) +
  theme_void() +
  theme(legend.position = "bottom") +
  coord_sf(xlim = c(-125, -66), ylim = c(24, 50)) 
           
  ,guide = guide_legend(override.aes = list(size =5))


    # Customizes the size legend to be horizontal with labels on the bottom
    guide = guide_legend(override.aes = list(size =5))
      direction = ""
    )
    
  )
  coord_sf(xlim = c(-125, -66), ylim = c(24, 50) 

county_empty_map

  geom_sf (data = bubble_data, 
           pch =21)
  
  