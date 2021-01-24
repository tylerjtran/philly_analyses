
# Housing type
# Jan 2021 -- Re-writing code from March 2019

library(tidyverse); library(sf); library(tidycensus)


census_api_key(Sys.getenv('census_api'))


# Get Azavea neighborhoods
neighborhoods <- st_read('https://raw.githubusercontent.com/azavea/geo-data/master/Neighborhoods_Philadelphia/Neighborhoods_Philadelphia.geojson')


# Get housing type data from ACS
housing_type <- get_acs(geography = 'block group',
                        table = 'B25024',
                        state = 'PA',
                        county = 'Philadelphia',
                        year = 2019,
                        geometry = T) %>%
  mutate(label = case_when(
    variable == 'B25024_001' ~ 'Total',
    variable == 'B25024_002' ~ 'Single family detached',
    variable == 'B25024_003' ~ 'Single family attached',
    variable == 'B25024_004' ~ '2 units',
    variable %in% c('B25024_005', 'B25024_006') ~ '3 to 9 units',
    variable == 'B25024_007' ~ '10 to 19 units',
    variable == 'B25024_008' ~ '20 to 49 units',
    variable == 'B25024_009' ~ '50+ units',
    variable == 'B25024_010' ~ 'Other'
  )) %>%
  st_transform(crs = st_crs(neighborhoods)) %>%
  st_join(neighborhoods, join = st_intersects) %>%
  group_by(mapname) %>%
  summarise(n = sum(estimate[label == 'Total'], na.rm = T),
            `Single family detached` = sum(estimate[label == 'Single family detached'], na.rm = T)/n,
            `Single family attached` = sum(estimate[label == 'Single family attached'], na.rm = T)/n,
            `2 units` = sum(estimate[label == '2 units'], na.rm = T)/n,
            `3 to 9 units` = sum(estimate[label == '3 to 9 units'], na.rm = T)/n,
            `10 to 19 units` = sum(estimate[label == '10 to 19 units'], na.rm = T)/n,
            `20 to 49 units` = sum(estimate[label == '20 to 49 units'], na.rm = T)/n,
            `50+ units` = sum(estimate[label == '50+ units'], na.rm = T)/n,
            Other = sum(estimate[label == 'Other'], na.rm = T)/n) %>%
  as_tibble() %>%
  mutate(total_p_sfh = `Single family detached` + `Single family attached`)


sfh_20_most <- housing_type %>%
  top_n(20, total_p_sfh)

sfh_20_least <- housing_type %>%
  top_n(-20, total_p_sfh)

citywide <- get_acs(geography = 'county',
                    table = 'B25024',
                    state = 'PA',
                    county = 'Philadelphia',
                    year = 2019) %>%
  mutate(label = case_when(
    variable == 'B25024_001' ~ 'Total',
    variable == 'B25024_002' ~ 'Single family detached',
    variable == 'B25024_003' ~ 'Single family attached',
    variable == 'B25024_004' ~ '2 units',
    variable %in% c('B25024_005', 'B25024_006') ~ '3 to 9 units',
    variable == 'B25024_007' ~ '10 to 19 units',
    variable == 'B25024_008' ~ '20 to 49 units',
    variable == 'B25024_009' ~ '50+ units',
    variable == 'B25024_010' ~ 'Other'
  )) %>%
  summarise(mapname = 'Citywide',
            n = sum(estimate[label == 'Total'], na.rm = T),
            `Single family detached` = sum(estimate[label == 'Single family detached'], na.rm = T)/n,
            `Single family attached` = sum(estimate[label == 'Single family attached'], na.rm = T)/n,
            `2 units` = sum(estimate[label == '2 units'], na.rm = T)/n,
            `3 to 9 units` = sum(estimate[label == '3 to 9 units'], na.rm = T)/n,
            `10 to 19 units` = sum(estimate[label == '10 to 19 units'], na.rm = T)/n,
            `20 to 49 units` = sum(estimate[label == '20 to 49 units'], na.rm = T)/n,
            `50+ units` = sum(estimate[label == '50+ units'], na.rm = T)/n,
            Other = sum(estimate[label == 'Other'], na.rm = T)/n) %>%
  mutate(total_p_sfh = `Single family detached` + `Single family attached`)


p_housing_types <- bind_rows(
  sfh_20_least,
  sfh_20_most,
  citywide
) %>%
  select(-n, -geometry) %>%
  pivot_longer(c(-mapname, -total_p_sfh), names_to = 'category', values_to = 'p_category')


p_housing_types %>%
  ggplot(aes(fill = category, y = p_category, x = reorder(mapname, -total_p_sfh))) +
  geom_bar(position = 'stack', stat = 'identity') +
  coord_flip()
