

library(tidyverse); library(tidycensus); library(sf); library(USAboundaries)

census_api_key('851a34f48dcd85ed13788351d2b04e7cd00c84d5')

v17 <- load_variables(2018, "acs5", cache = TRUE)

View(v17)

peer_cities <- 'baltimore city, maryland|philadelphia city, pennsylvania|seattle city, washington|new york city|chicago city, illinois|
                |los angeles city, california|phoenix city, arizona|boston city, massachusetts|minneapolis city, minnesota|
                |charlotte city, north carolina|denver city, colorado|miami city, florida|portland city, oregon|
                |san francisco city, california|houston city, texas|austin city, texas|atlanta city, georgia'

peer_census_data <- get_acs(geography = 'place',
                            variables = c('B05002_001', 'B05002_003'),
                            year = 2018) %>%
  filter(grepl(peer_cities, NAME, ignore.case = TRUE),
         ! grepl('east|west|south|north chicago|north miami', NAME, ignore.case = TRUE)) %>%
  group_by(NAME) %>%
  summarise(born_in_state = sum(estimate[variable == 'B05002_003'])/sum(estimate[variable == 'B05002_001'])) %>%
  mutate(NAME = gsub(' city', '', NAME))



census_data <- get_acs(geography = 'tract',
                       table = 'B05002',
                       state = 'PA',
                       county = 'Philadelphia',
                       year = 2018,
                       geometry = TRUE)

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
  arrange(desc(p)) %>%
  slice(p, 1)

plurality_birthplace_incl_pa <- birth_place %>%
  st_set_geometry(NULL) %>%
  select(-p_born_other_state) %>%
  pivot_longer(-GEOID, names_to = 'region_incl_pa', values_to = 'p') %>%
  group_by(GEOID) %>%
  arrange(desc(p)) %>%
  slice(p, 1)

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
