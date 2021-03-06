

library(tidyverse); library(sf); library(tidycensus); library(lubridate); library(showtext)


#######################################################################

# Set up fonts

font_add_google('Merriweather')
font_add_google('Source Sans Pro', 'ssp')
font_add_google('Lora', 'old_serif')
font_add_google('Courier Prime', 'old_mono')

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
rivers <- st_read('https://opendata.arcgis.com/datasets/2b10034796f34c81a0eb44c676d86729_0.geojson') %>%
  filter(MUNI == 'Philadelphia',
         CREEK_NAME %in% c('Wissahickon Creek', 'Pennypack Creek', 'Cobbs Creek',
                           'Schuylkill River', 'Delaware River'))

# Run the race_ethnicity code to get get_race_ethnicity() function
source('./race_ethnicity/race_ethnicity.R')

load('./fireplaces/all_sfh.Rdata')
# Commenting this part out because my computer is slow and I can load faster with the RData loaded above
# # All single family homes with some year and value bounds
# all_sfh <- st_read('https://phl.carto.com/api/v2/sql?filename=opa_properties_public&format=geojson&skipfields=cartodb_id&q=SELECT+*+FROM+opa_properties_public') %>%
#   filter(category_code_description == 'Single Family') %>%
#   mutate(has_fireplace = fireplaces > 0,
#          year_built = as.numeric(year_built)) %>%
#   filter(year_built >= 1600,
#          year_built <= year(today()),
#          market_value <= 4000000)


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

# Map of fireplaces overlaid on lowest-income census tracts
ggplot() +
  geom_sf(data = city, fill = 'lightgray', col = NA) +
  geom_sf(data = med_income %>%
            filter(lowest_income),
          fill = '#b36f93', col = '#b36f93') +
  geom_sf(data = rivers, fill = 'darkgray', col = 'darkgray') +
  geom_sf(data = all_sfh %>%
            filter(has_fireplace),
          shape = '.', size = 2) +
  labs(title = "There are relatively few fireplaces in Philadelphia's poorest neighborhoods",
       subtitle = 'Red areas show the lowest-income census tracts in Philly, and black dots show all recorded fireplaces') +
  font_theme +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank())


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

# Load race_points shapefile (this can be made with the code directly above, but it takes awhile to run)
race_points <- st_read('./race_ethnicity/race_points.shp')

# Use colors that are similar to UVA's map
my_cols <- c('#ff0202', '#aad44b', '#edb12c', '#e2c46e', '#86bbe3')


# side-by-side map: left, fireplaces as points 0.5, dark red. put a semi-transparent gray layer
# on philly suburbs
# ggplot() + 
  # geom_sf(data = race_points, aes(col = race_eth), shape = '.') + 
  # geom_sf(data = fireplaces, size = 0.5, col = 'black') +
  # scale_color_manual(values = my_cols)




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



# Look at prevalence of fireplaces in 1800s to compare to Penn master's thesis
fireplaces_1800s <- all_sfh %>%
  filter(year_built >= 1800,
         year_built < 1900) %>%
  mutate(decade = paste0(as.character(year_built - (year_built %% 10)), 's')) %>%
  calc_p_fireplace(col_of_interest = decade)

fireplaces_1800s %>%
  ggplot() +
  labs(x = 'Decade', y = 'Fraction of Homes with Fireplaces',
       title = 'Prevalence of Fireplace by Construction Decade') +
  geom_bar(aes(x = decade, y = p_fireplace), stat = 'identity', fill = 'black', color = 'black') +
  scale_y_continuous(expand = c(0, 0), labels = scales::percent) +
  theme(panel.background = element_rect(fill = '#fdfae7'),
        plot.background = element_rect(fill = '#fdfae7'),
        panel.border = element_rect(color = 'black', fill = NA),
        panel.grid = element_line(linetype = 'dashed', color = 'black'),
        plot.title = element_text(family = 'old_mono'),
        axis.title = element_text(family = 'old_serif'),
        axis.text = element_text(family = 'old_serif'))


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
         ! is.na(heater_type)) %>%
  group_by(decade) %>%
  count(heater_type) %>%
  mutate(p = n/sum(n))

# 6.5w x 4.75tall
heat_1800s %>%
  filter(heater_type %in% c('Hot water', 'Hot air', 'Electric baseboard')) %>%
  mutate(heater_type = factor(heater_type, levels = c('Hot water', 'Hot air', 'Electric baseboard'))) %>%
  ggplot() +
  labs(x = 'Decade', y = 'Fraction of Homes',
       title = 'Primary Form of Heating By Decade Built',
       caption = 'Includes single family homes existing in 2021.') +
  geom_bar(aes(x = decade, y = p, fill = heater_type), 
           stat = 'identity', position = 'dodge', color = 'black') +
  scale_y_continuous(expand = c(0, 0), labels = scales::percent) +
  scale_fill_manual(values = c('#4a4a4a', 'black', '#fdfae7')) +
  guides(fill = guide_legend(nrow = 1)) +
  theme(panel.background = element_rect(fill = '#fdfae7'),
        plot.background = element_rect(fill = '#fdfae7'),
        panel.border = element_rect(color = 'black', fill = NA),
        panel.grid = element_line(linetype = 'dashed', color = 'black'),
        plot.title = element_text(family = 'old_mono'),
        axis.title = element_text(family = 'old_serif'),
        plot.caption = element_text(family = 'old_serif'),
        axis.text = element_text(family = 'old_serif'),
        legend.position = c(0.6, 0.938),
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.text = element_text(family = 'old_serif'),
        legend.box.background = element_rect(fill = '#fdfae7', color = 'black'))
  


# The chunk of code below creates the gif. I'm just going to load it in.
# 
# fireplaces_decades <- all_sfh %>%
#   filter(has_fireplace) %>%
#   mutate(decade = year_built - (year_built %% 10))
# 
# # gganimate::shadow_mark() doesn't seem to be working with transition_manual()
# # ggplot() +
# #   geom_sf(data = fireplaces_decades, col = 'darkred') 
# #   transition_manual(decade) + 
# #   shadow_mark(col = 'darkgray')
# 
# decades <- unique(fireplaces_decades$decade)
# 
# year_label <- tibble(
#   lat = rep(40.09, length(decades)),
#   lng = rep(-75.13, length(decades)),
#   label = paste0(decades, 's')
# ) %>%
#   st_as_sf(coords = c('lng', 'lat'), crs = st_crs(city))
# 
# title_label <- tibble(
#   lat = 40.128,
#   lng = -75.2481,
#   label = "Philadelphia Fireplaces"
# ) %>%
#   st_as_sf(coords = c('lng', 'lat'), crs = st_crs(city))
# 
# for (i in 1:length(decades)){
#   ggplot() +
#     geom_sf(data = city, fill = 'lightgray', col = NA) +
#     geom_sf_text(data = year_label[i,], aes(label = label), size = 14, family = 'ssp') +
#     geom_sf_text(data = title_label, aes(label = label), hjust = 0, 
#                  family = 'Merriweather', fontface = 'bold', size = 14) +
#     geom_sf(data = fireplaces_decades %>%
#               filter(decade < decades[i]),
#             col = 'darkgray', size = 0.9) +
#     geom_sf(data = fireplaces_decades %>%
#               filter(decade == decades[i]),
#             col = 'darkred', size = 0.9) +
#     theme(
#       panel.background = element_blank(),
#       panel.grid = element_blank(),
#       axis.text = element_blank(),
#       axis.ticks = element_blank(),
#       axis.title = element_blank()
#     )
#   
#   ggsave(filename = paste0('fireplace_map_', decades[i], '.png'))
# }




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
                           as.character(mapname))) %>%
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


