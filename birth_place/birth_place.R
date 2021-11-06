

library(tidyverse); library(tidycensus); library(sf); library(ggrepel)

census_api_key(Sys.getenv('census_api'))

library(showtext)

#########################################################################
# Set up fonts

font_add_google('Merriweather')
font_add_google('Source Sans Pro', 'ssp')

showtext_auto()

font_theme <- theme(
  plot.title = element_text(family = 'Merriweather', face = 'bold'),
  plot.subtitle = element_text(family = 'ssp'),
  axis.text = element_text(family = 'ssp'),
  axis.title = element_text(family = 'ssp'),
  legend.text = element_text(family = 'ssp'),
  plot.caption = element_text(family = 'ssp', color = 'darkgray')
)
#########################################################################



peer_cities <- 'baltimore city, maryland|philadelphia city, pennsylvania|seattle city, washington|new york city|chicago city, illinois|
                |los angeles city, california|phoenix city, arizona|boston city, massachusetts|minneapolis city, minnesota|
                |charlotte city, north carolina|denver city, colorado|portland city, oregon|
                |san francisco city, california|houston city, texas|austin city, texas|atlanta city, georgia|detroit city, michigan|
                |cleveland city, ohio|dallas city, texas|new orleans city, louisiana|indianapolis city, indiana'

peer_census_data <- get_acs(geography = 'place',
                            variables = c('B05002_001', 'B05002_003', 
                                          'B06012_001', 'B06012_002', 'B19013_001',
                                          'B02001_001', 'B02001_003'),
                            year = 2018) %>%
  filter(grepl(peer_cities, NAME, ignore.case = TRUE),
         # Get rid of directional "places"
         ! grepl('east|west|south|north chicago|lake dallas', NAME, ignore.case = TRUE)) %>%
  group_by(NAME) %>%
  summarise(p_born_in_state = sum(estimate[variable == 'B05002_003'])/sum(estimate[variable == 'B05002_001']),
            p_poverty = sum(estimate[variable == 'B06012_002'])/sum(estimate[variable == 'B06012_001']),
            p_black = sum(estimate[variable == 'B02001_003'])/sum(estimate[variable == 'B02001_001']),
            med_income = median(estimate[variable == 'B19013_001'])) %>%
  mutate(NAME = gsub(' city', '', NAME))

peer_census_data %>% 
  # remove state names from city labels
  mutate(NAME = gsub("(.*),.*", "\\1", NAME)) %>%
  ggplot(aes(x = med_income, y = p_born_in_state)) + 
  geom_point(aes(color = p_black), size = 3.5) + 
  geom_text_repel(aes(label = NAME), size = 3, fontface = 'italic', family = 'ssp') +
  annotate('text', x = 70000, y = 0.7,
           label = 'Almost 70% of Philadelphians\nwere born in PA, and Philly has\na higher poverty rate than\nmost large cities',
           size = 3.5, family = 'ssp') +
  geom_curve(aes(x = 60000, y = 0.72, xend = 44000, yend = 0.67), curvature = 0.3,
             arrow = arrow(type = 'closed', length = unit(0.1, 'in'))) +
  annotate('text', x = 95000, y = 0.5,
           label = 'Seattle and SF have higher\nmedian incomes, more transplants,\nand fewer Black residents\nthan most other cities',
           size = 3.5, family = 'ssp') +
  geom_curve(aes(x = 94000, y = 0.46, xend = 85562, yend = 0.375), curvature = 0.2,
             arrow = arrow(type = 'closed', length = unit(0.1, 'in'))) +
  geom_curve(aes(x = 96000, y = 0.46, xend = 104552, yend = 0.405), curvature = -0.2,
             arrow = arrow(type = 'closed', length = unit(0.1, 'in'))) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_continuous(labels = scales::dollar) +
  labs(title = 'Cities with lower median incomes tend to have fewer out-of-state transplants',
       subtitle = 'Darker purples represent cities with higher proportions of Black residents',
       x = 'Median Household Income',
       y = '% of Residents Born in State') +
  font_theme +
  theme(panel.grid = element_blank(),
        legend.position = 'none',
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))) +
  scale_color_gradient(low = 'white', high = '#8A1BD2')


cor.test(peer_census_data$born_in_state, peer_census_data$med_income)
summary(lm(born_in_state ~ med_income, data = peer_census_data))

census_data <- get_acs(geography = 'tract',
                       table = 'B05002',
                       state = 'PA',
                       county = 'Philadelphia',
                       year = 2018,
                       geometry = TRUE)

# For each census tract, get % born in PA vs other regions
birth_place <- census_data %>%
  group_by(GEOID) %>%
  summarise(p_born_pa = sum(estimate[variable == 'B05002_003'])/sum(estimate[variable == 'B05002_001']),
            p_born_other_state = sum(estimate[variable == 'B05002_004'])/sum(estimate[variable == 'B05002_001']),
            p_born_northeast = sum(estimate[variable == 'B05002_005'])/sum(estimate[variable == 'B05002_001']),
            p_born_midwest = sum(estimate[variable == 'B05002_006'])/sum(estimate[variable == 'B05002_001']),
            p_born_south = sum(estimate[variable == 'B05002_007'])/sum(estimate[variable == 'B05002_001']),
            p_born_west = sum(estimate[variable == 'B05002_008'])/sum(estimate[variable == 'B05002_001']),
            p_born_outside_us = sum(estimate[variable %in% c('B05002_009', 'B05002_012', 'B05002_013')])/
              sum(estimate[variable == 'B05002_001']),
            geometry = unique(geometry)) 

# Show diverging with midpoint at 50%
birth_place %>%
  ggplot(aes(fill = p_born_pa)) +
  geom_sf(col = 'white')

plurality_birthplace_no_pa <- birth_place %>%
  st_set_geometry(NULL) %>%
  select(-p_born_pa, -p_born_other_state) %>%
  pivot_longer(-GEOID, names_to = 'region_no_pa', values_to = 'p') %>%
  group_by(GEOID) %>%
  slice_max(p)

plurality_birthplace_incl_pa <- birth_place %>%
  st_set_geometry(NULL) %>%
  select(-p_born_other_state) %>%
  pivot_longer(-GEOID, names_to = 'region_incl_pa', values_to = 'p') %>%
  group_by(GEOID) %>%
  slice_max(p)

birth_place <- birth_place %>%
  left_join(plurality_birthplace_no_pa %>%
              select(-p)) %>%
  left_join(plurality_birthplace_incl_pa %>%
              select(-p))

# Show diverging with midpoint at 50%
birth_place %>%
  ggplot(aes(fill = region_no_pa)) +
  geom_sf(col = 'white')


county_acs <- get_acs(geography = 'county',
                      variables = c('B05002_001', 'B05002_003'),
                      year = 2018)
