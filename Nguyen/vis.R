library(dplyr)
library(tidyverse)
library(haven)

national_data = read_sas("data_SAS/analytic_data2025_v2.sas7bdat")

# ==============================================================================
# first vis: (map) top race and stay data of the county ------------------------
# or cluster

# notes:
# cannot do states bc no state has majority non white over 70%
# so have to do county


# to be able to view these df, run EDA_preventable_stays.R AND EDA_race.R
view(race_largest_70plus)
view(preventable_stays_county)

race_preventablestays_county = race_largest_70plus |>
  left_join(preventable_stays_county, by = c("fipscode","statecode","countycode","state","county"))
view(race_preventablestays_county)
# |>
#   arrange(complete_rate)
# view(clinical_care_county_level_skim)
race_preventablestays_county_plot = race_preventablestays_county |>
  select(fipscode,largest_race_label,v005_rawvalue)
view(race_preventablestays_county_plot)

# map now ======================================================================
# install.packages("sf")
library(sf)
library(dplyr)  
library(ggplot2)
library(tigris)

# loading US counties geometry
options(tigris_use_cache = TRUE)

# class = "sf"
counties_sf = counties(cb = TRUE, resolution = "5m", year = 2022) |>
  filter(!(STATEFP %in% c("60", "66", "69", "72", "78"))) |>  # remove territories
  st_transform(crs = 5070)  # Albers Equal Area for US mapping


    # sf = simple features object 
# view(counties_sf)

counties_sf = counties_sf |>
  # replaces the polygon shape with a single point at the center
  st_centroid() |>
  mutate (
    lon = st_coordinates(geometry)[,1],
    lat = st_coordinates(geometry)[,2]
  )
view(counties_sf)

# merge df so our df we want to plot has map data
plot_df = left_join(counties_sf, race_preventablestays_county_plot, by = c("GEOID" = "fipscode"))
plot_df = plot_df |> filter(!is.na(largest_race_label))
view(plot_df)

hist(plot_df$v005_rawvalue)


                            
# now mapping ==================================================
# starts new ggplot plot
ggplot() +
  # base map - plotting counties' shape
  # layering geometry and points 
  # , fill = "white"
  geom_point(data = plot_df,
             aes( x = lon, y = lat,
                  size = log(v005_rawvalue),
                  color = largest_race_label),
             alpha = 0.7) +
  geom_sf(data = counties_sf, fill = NA, color = "black", size = 0.4) +
  # controls the minimum and maximum bubble size in points.
  scale_size(range = c(0.1, 3), 
             name = "Bubble Variable") +
  # scale_size_continuous(range = c(1,10)) +
  # Use scale_color_viridis_d() for categorical (non-numeric) variables
  # aka d = discrete color scales
  scale_color_viridis_d(name = "Race Group", 
                        option = "D") +
  # theme_void() + # Removes all background gridlines, axis ticks, and text for a clean map layout.
  theme_minimal() #+
  # theme(legend.position = "right") + # Places the legends for size and color to the right of the map.
  # to only see US and not entire earth
  # coord_sf(xlim = c(-2500000, 2500000), ylim = c(-2000000, 1300000))
  # labs(size = "Bubble size (var1)", color = "Color intensity (var2)")

class(plot_df$v005_rawvalue)


bubble_data = counties_sf |>
  st_centroid() |>                         # get centroid for plotting points
  mutate(lon = st_coordinates(.)[,1],      # extract longitude
         lat = st_coordinates(.)[,2]) |>  # extract latitude
  as.data.frame()                          # convert sf to regular df for ggplot


str(bubble_data$var1)

bubble_data


map_data_bi <- bi_class(
  map_data,
  x = preventable_hospital_stays_raw_value,      # First variable
  y = percent_rural_raw_value_jit,                   # Second variable
  style = "quantile",                            # Categorizes by quantiles
  dim = 3                                        # 3x3 grid (9 bivariate classes)
)

# Replace 'state' with the actual column name if it's different
map_data_bi_continental <- map_data_bi %>%
  filter(!STATE_NAME %in% c("Hawaii", "Alaska"))


bimap <- ggplot() +
  geom_sf(
    data = map_data_bi_continental,
    aes(fill = bi_class),
    color = "white", size = 0.1, show.legend = FALSE
  ) +
  bi_scale_fill(pal = "GrPink", dim = 3) +
  labs(
    title = "Preventable Hospital Stays & Percent Rural (US Counties)"
  ) +
  bi_theme() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  ) +
  coord_sf(xlim = c(-125, -66), ylim = c(24, 50), expand = FALSE)  # Approximate bounds for continental US

legend <- bi_legend(
  pal = "GrPink",
  dim = 3,
  xlab = "Higher Preventable Hospital Stays",
  ylab = "Higher % Rural",
  size = 6
)

ggsave("bivariate_county_map.png", plot = last_plot(), width = 12, height = 8, dpi = 300)

ggdraw() +
  draw_plot(bimap, 0, 0, 1, 1) +
  draw_plot(legend, 0.7, 0.7, 0.2, 0.2) +
  theme(text = element_text(size = 10))




options(tigris_use_cache = TRUE)  # optional: cache downloaded shapefiles
counties_sf <- counties(cb = TRUE, year = 2020, class = "sf")

view(counties_sf)
plot_df = left_join(counties_sf, race_preventablestays_county, by = c("GEOID" = "fipscode"))
# view(plot_df)

# Static map
ggplot(race_preventablestays_county) +
  geom_sf(aes(fill = v005_rawvalue), color = NA) +
  scale_fill_viridis_c(option = "plasma", name = "Preventable Stays") +
  labs(title = "Counties where top race ≥ 70% of population",
       subtitle = "Colored by rate of preventable hospital stays") +
  theme_minimal()

# Confirm plot_df is still spatial
inherits(plot_df, "sf")         # Should return TRUE
names(plot_df)                  # Should include 'geometry'
st_geometry(plot_df)           # Should return the shape data





# view(preventable_stays_county_noNA)



# ---------------------------------------------------------


white_majority_70plus_statesonly = white_majority_70plus |>
  filter(countycode == "000", statecode != "00")
# view(white_majority_70plus_statesonly)

race_largest_noWhite_70plus_statesonly = race_largest_noWhite_70plus |>
  filter(countycode == "000", statecode != "00")
view(race_largest_noWhite_70plus_statesonly)

view(race_largest_noWhite_70plus)

view(race_rawvalue |>
  filter(countycode == "000", statecode != "00"))

view(race_largest |>
  filter(countycode == "000", statecode != "00"))

# white_majority_70plus
# -
# race_largest_noWhite_70plus ---
# black_majority_70plus
# native_majority_70plus
# hispanic_majority_70plus

view(black_majority_70plus)
