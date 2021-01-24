

library(tidyverse); library(sf); library(tidycensus); library(lubridate); library(showtext)


#######################################################################

# Set up fonts

font_add_google('Merriweather')
font_add_google('Source Sans Pro', 'ssp')

showtext_auto()

font_theme <- theme(
  plot.title = element_text(family = 'Merriweather', face = 'bold'),
  plot.subtitle = element_text(family = 'ssp'),
  axis.text = element_text(family = 'ssp'),
  axis.title = element_text(family = 'ssp'),
  plot.caption = element_text(family = 'ssp', color = 'darkgray')
)

#######################################################################

# Get census tracts and Azavea neighborhoods from opendataphilly
tracts <- st_read('http://data.phl.opendata.arcgis.com/datasets/8bc0786524a4486bb3cf0f9862ad0fbf_0.geojson')
neighborhoods <- st_read('https://raw.githubusercontent.com/azavea/geo-data/master/Neighborhoods_Philadelphia/Neighborhoods_Philadelphia.geojson')
city <- st_read('http://data.phl.opendata.arcgis.com/datasets/063f5f85ef17468ebfebc1d2498b7daf_0.geojson') %>%
  st_union()

# Run the race_ethnicity code to get get_race_ethnicity() function
source('./race_ethnicity/race_ethnicity.R')

# All single family homes with some year and value bounds
all_sfh <- st_read('https://phl.carto.com/api/v2/sql?filename=opa_properties_public&format=geojson&skipfields=cartodb_id&q=SELECT+*+FROM+opa_properties_public') %>%
  filter(category_code_description == 'Single Family') %>%
  mutate(has_fireplace = fireplaces > 0,
         year_built = as.numeric(year_built)) %>%
  filter(year_built >= 1600,
         year_built <= year(today()),
         market_value <= 4000000)

# race breakdowns of census block groups
race_bg <- get_race_ethnicity(geography = 'block group', geometry = T) %>%
  filter(! is.na(race_eth))

# # Generate dot map a la UVA map
# race_points <- NULL
# for (i in 1:nrow(race_bg)){
#   race_points <- rbind(
#     race_points,
#     st_sample(race_bg[i,], round(race_bg$n[i]/10)) %>%
#       st_sf() %>%
#       mutate(race_eth = race_bg$race_eth[i])
#   )
# }

# # Generate dot map like above
# This method seems to be slower than the for loop
# race_points <- map(unique(race_bg$race_eth), function(group) {
#   race_bg %>%
#     filter(race_eth == group) %>%
#     st_sample(., size = .$n) %>%
#     st_sf() %>%
#     mutate(group = group) 
# }) %>%
#   reduce(rbind) %>%
#   group_by(group) %>%
#   summarise()

race_points <- st_read('./race_ethnicity/race_points.shp')

my_cols <- c('#ff0202', '#aad44b', '#edb12c', '#e2c46e', '#86bbe3')

ggplot() + 
  geom_sf(data = race_points, aes(col = race_eth), shape = '.') + 
  # geom_sf(data = fireplaces, size = 0.5, col = 'black') +
  scale_color_manual(values = my_cols)

# side-by-side map: left, fireplaces as points 0.5, dark red. put a semi-transparent gray layer
# on philly suburbs


# Function to calculate % of houses with fireplaces
calc_p_fireplace <- function(df, col_of_interest){
  df %>%
    as_tibble() %>%
    select(-geometry) %>%
    group_by({{ col_of_interest }}, has_fireplace) %>%
    count() %>%
    ungroup() %>%
    complete({{ col_of_interest }}, has_fireplace = c(T, F), fill = list(n = 0)) %>%
    filter(! is.na(has_fireplace)) %>%
    group_by({{ col_of_interest }}) %>%
    summarise(n_sfh = sum(n),
              n_fireplace = n[has_fireplace],
              p_fireplace = n_fireplace/n_sfh)
}

# get median household income data from ACS and split into quantiles
med_income <- get_acs(geography = 'tract',
                      variables = 'B19013_001',
                      state = 'PA',
                      county = 'Philadelphia',
                      year = 2019,
                      geometry = T) %>%
  mutate(income_quantile = cut_number(estimate, 5),
         income_quantile = factor(income_quantile, levels = levels(income_quantile), ordered = TRUE),
         lowest_income = income_quantile == min(income_quantile, na.rm = T))

ggplot() +
  geom_sf(data = med_income, aes(fill = lowest_income)) +
  geom_sf(data = all_sfh %>%
               filter(has_fireplace),
             size = 0.5)

# Age vs market value plot. 8.11in wide x 4.6in tall (maybe taller)
ggplot() +
  geom_point(data = all_sfh %>%
               filter(! has_fireplace),
             aes(x = year_built, y = market_value), col = 'darkgray', alpha = 0.5) +
  # Plot as a separate object to make sure these are on top
  geom_point(data = all_sfh %>%
               filter(has_fireplace), 
             aes(x = year_built, y = market_value), col = 'darkred', alpha = 0.3) +
  labs(title = 'Inexpensive new houses with fireplaces are rare in Philadelphia',
       x = 'Construction Year', y = 'Assessed Market Value',
       subtitle = str_wrap('Each point on the graph below is a house; red points are houses with fireplaces and gray points are houses without fireplaces.')) +
  annotate('text', x = 2032, y = 130000, label = 'This house sold\nfor $425k but is\nassessed at $15k', 
           family = 'ssp') +
  geom_curve(aes(x = 2023, y = 130000, xend = 2017.8, yend = 24000),
             arrow = arrow(type = 'closed', length = unit(0.1, 'in'))) +
  expand_limits(x = 0, y = 0) +
  scale_y_continuous(expand = c(0, 0),
                     breaks = c(0, 200000, 400000, 600000), 
                     labels = c('$0', '$200k', '$400k', '$600k'),
                     limits = c(0, 600000)) +
  scale_x_continuous(limits = c(1921, 2045), expand = c(0, 0),
                     breaks = c(1925, 1950, 1975, 2000)) + 
  theme(panel.background = element_blank(),
        panel.grid = element_blank()) +
  font_theme


# number of and proportion of single family houses in each census tract with fireplaces recorded
n_per_tract <- tracts %>%
  st_join(all_sfh, join = st_intersects) %>%
  calc_p_fireplace(col_of_interest = GEOID10)

# re-join to tracts sf object bc i wrote the calc_p_..() function to remove the geometry
tracts_fireplaces <- tracts %>%
  left_join(n_per_tract, by = 'GEOID10')

# map of % of homes with fireplaces by census tract
ggplot() +
  geom_sf(data = tracts_fireplaces, aes(fill = p_fireplace))


# Look at prevalence of fireplaces in 1800s to compare to Penn master's thesis
fireplaces_1800s <- all_sfh %>%
  filter(year_built >= 1800,
         year_built < 1900) %>%
  mutate(decade = paste0(as.character(year_built - (year_built %% 10)), 's')) %>%
  calc_p_fireplace(col_of_interest = decade)


heat_1800s <- all_sfh %>%
  as_tibble() %>%
  select(-geometry) %>%
  filter(year_built >= 1800,
         year_built < 1900) %>%
  mutate(decade = paste0(as.character(year_built - (year_built %% 10)), 's'),
  fuel_type = case_when(
    fuel == 'A' ~ 'Natural gas',
    fuel == 'B' ~ 'Oil',
    fuel == 'C' ~ 'Electric',
    fuel == 'D' ~ 'Coal',
    fuel == 'E' ~ 'Solar',
    fuel == 'F' ~ 'Wood',
    fuel == 'G' ~ 'Other',
    fuel == 'H' ~ 'None',
    TRUE ~ NA_character_
  ),
  heater_type = case_when(
    type_heater == 'A' ~ 'Hot air',
    type_heater == 'B' ~ 'Hot water',
    type_heater == 'C' ~ 'Electric baseboard',
    type_heater == 'D' ~ 'Heat pump',
    type_heater == 'E' ~ 'Other',
    type_heater == 'G' ~ 'Radiant',
    TRUE ~ NA_character_
  )) %>%
  filter(! is.na(decade),
         ! is.na(fuel_type)) %>%
  group_by(decade) %>%
  count(fuel_type) %>%
  mutate(p = n/sum(n))
  
fireplaces_decades <- fireplaces %>%
  mutate(decade = year_built - (year_built %% 10))


# gganimate::shadow_mark() doesn't seem to be working with transition_manual()
# ggplot() +
#   geom_sf(data = fireplaces_decades, col = 'darkred') 
#   transition_manual(decade) + 
#   shadow_mark(col = 'darkgray')
  


decades <- unique(fireplaces_decades$decade)

year_label <- tibble(
  lat = rep(40.09, length(decades)),
  lng = rep(-75.13, length(decades)),
  label = paste0(decades, 's')
) %>%
  st_as_sf(coords = c('lng', 'lat'), crs = st_crs(city))

title_label <- tibble(
  lat = 40.128,
  lng = -75.2481,
  label = "Philadelphia Fireplaces"
) %>%
  st_as_sf(coords = c('lng', 'lat'), crs = st_crs(city))

for (i in 1:length(decades)){
  ggplot() +
    geom_sf(data = city, fill = 'lightgray', col = NA) +
    geom_sf_text(data = year_label[i,], aes(label = label), size = 14, family = 'ssp') +
    geom_sf_text(data = title_label, aes(label = label), hjust = 0, 
                 family = 'Merriweather', fontface = 'bold', size = 14) +
    geom_sf(data = fireplaces_decades %>%
              filter(decade < decades[i]),
            col = 'darkgray', size = 0.9) +
    geom_sf(data = fireplaces_decades %>%
              filter(decade == decades[i]),
            col = 'darkred', size = 0.9) +
    theme(
      panel.background = element_blank(),
      panel.grid = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank()
    )
  
  ggsave(filename = paste0('fireplace_map_', decades[i], '.png'))
}




# Get citywide % of homes w fireplaces by decade (to bind below)
citywide <- all_sfh %>%
  filter(year_built >= 1900,
         year_built <= 2019) %>%
  mutate(decade = year_built - (year_built %% 10),
         period = paste0(decade, 's')) %>%
  calc_p_fireplace(col_of_interest = period) %>%
  mutate(mapname = 'Citywide', .after = 'period') %>%
  group_by(mapname) %>%
  mutate(neigh_avg = mean(p_fireplace, na.rm = T))

# selected neighborhoods for heatmap below
selected_neighborhoods <- c('Upper Roxborough', 'Society Hill', 'Chestnut Hill', 'West Mount Airy',
                            'Passyunk Square', 'Northern Liberties', 'Queen Village',
                            'Pennsport', 'Roxborough', 'Fishtown - Lower Kensington',
                            'Graduate Hospital', 'Fairmount', 'Rittenhouse', 'Point Breeze', 'Brewerytown',
                            'Grays Ferry', 'Strawberry Mansion')

# for a selection of neighborhoods, what % of houses built each decade have fireplaces?
neighborhood_heatmap <- neighborhoods %>%
  st_join(all_sfh, join = st_intersects) %>%
  filter(year_built >= 1900,
         year_built <= 2019) %>%
  mutate(decade = year_built - (year_built %% 10),
         period = paste0(decade, 's')) %>%
  as_tibble() %>%
  select(-geometry) %>%
  group_by(period, mapname, has_fireplace) %>%
  count() %>%
  ungroup() %>%
  complete(period, mapname, has_fireplace = c(T, F), fill = list(n = 0)) %>%
  filter(! is.na(has_fireplace)) %>%
  group_by(period, mapname) %>%
  summarise(n_sfh = sum(n),
            n_fireplace = n[has_fireplace],
            p_fireplace = n_fireplace/n_sfh) %>%
  filter(mapname %in% selected_neighborhoods) %>%
  mutate(mapname = if_else(mapname == 'Fishtown - Lower Kensington',
                           'Fishtown',
                           mapname)) %>%
  group_by(mapname) %>%
  # Get an average % by neighborhood across time to arrange the heatmap by
  mutate(neigh_avg = mean(p_fireplace, na.rm = T)) %>%
  ungroup() %>%
  bind_rows(citywide)



bold_citywide <- c(rep('plain', 6), 'bold.italic', rep('plain', 11))

# Create a heatmap of % of homes with fireplaces by construction decade
neighborhood_heatmap %>%
  ggplot(aes(x = period, y = reorder(mapname, neigh_avg), fill = p_fireplace)) +
  geom_tile(col = 'white') +
  scale_fill_gradient(low = '#ededed', high = 'darkred') +
  coord_equal() +
  scale_x_discrete(position = 'top') +
  scale_y_discrete(position = 'right') +
  labs(title = 'A comparison of neighborhood fireplaces over time',
       x = '', y = '',
       subtitle = str_wrap('The heatmap below shows the prominence of fireplaces in different neighborhoods of Philadelphia visualized by decade of home construction. Darker-red squares signify that a higher proportion of homes built during that decade have fireplaces. A gray square means that there are currently no homes recorded that were built during that decade.')) +
  theme(
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.position = 'none',
    plot.title = element_text(size = 14),
    axis.text.y = element_text(face = bold_citywide)
  ) +
  font_theme


