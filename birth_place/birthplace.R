
library(tidycensus); library(sf)

census_api_key(Sys.getenv('census_api'))

source('./tools/side_by_side.R')

birthplace <- get_acs(geography = 'tract',
                      table = 'B05002',
                      county = 'Philadelphia',
                      state = 'PA', 
                      year = 2019,
                      geometry = T) %>%
  group_by(GEOID) %>%
  summarise(Pennsylvania = estimate[variable == 'B05002_003']/estimate[variable == 'B05002_001'],
            South = estimate[variable == 'B05002_007']/estimate[variable == 'B05002_001'],
            Northeast = estimate[variable == 'B05002_005']/estimate[variable == 'B05002_001'],
            West = estimate[variable == 'B05002_008']/estimate[variable == 'B05002_001'],
            Midwest = estimate[variable == 'B05002_006']/estimate[variable == 'B05002_001'],
            `Outside 50 states` = sum(estimate[variable %in% c('B05002_009', 'B05002_013')])/estimate[variable == 'B05002_001'])

common_birthplace <- birthplace %>%
  st_drop_geometry() %>%
  pivot_longer(-GEOID, names_to = 'region', values_to = 'p') %>%
  filter(region != 'Pennsylvania') %>%
  group_by(GEOID) %>%
  slice_max(order_by = p) %>%
  ungroup()


birthplace %>%
  left_join(common_birthplace, by = 'GEOID') %>%
  ggplot(aes(fill = region)) +
  geom_sf(color = 'white', lwd = 1) +
  theme(
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank()
  )


