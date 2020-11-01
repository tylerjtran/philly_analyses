

library(tidyverse); library(tidycensus); library(sf)

census_api_key('851a34f48dcd85ed13788351d2b04e7cd00c84d5')

v17 <- load_variables(2018, "acs5", cache = TRUE)

View(v17)


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
            p_born_outside_us = sum(estimate[variable == 'B05002_013'])/sum(estimate[variable == 'B05002_001']),
            geometry = unique(geometry)) 

plurality_birthplace <- birth_place %>%
  st_set_geometry(NULL) %>%
  select(-p_born_pa, -p_born_other_state) %>%
  pivot_longer(-GEOID, names_to = 'region', values_to = 'p') %>%
  group_by(GEOID) %>%
  arrange(desc(p)) %>%
  slice(p)

# Show diverging with midpoint at 50%
birth_place %>%
  ggplot(aes(fill = p_born_pa)) +
  geom_sf(col = 'white')
