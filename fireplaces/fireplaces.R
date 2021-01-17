

tracts <- st_read('http://data.phl.opendata.arcgis.com/datasets/8bc0786524a4486bb3cf0f9862ad0fbf_0.geojson')
neighborhoods <- st_read('https://raw.githubusercontent.com/azavea/geo-data/master/Neighborhoods_Philadelphia/Neighborhoods_Philadelphia.geojson')


all_sfh <- st_read('https://phl.carto.com/api/v2/sql?filename=opa_properties_public&format=geojson&skipfields=cartodb_id&q=SELECT+*+FROM+opa_properties_public') %>%
  filter(category_code_description == 'Single Family') %>%
  mutate(has_fireplace = fireplaces > 0,
         year_built = as.numeric(year_built)) %>%
  filter(year_built >= 1600,
         year_built <= year(today()),
         market_value <= 4000000)

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

n_per_tract <- tracts %>%
  st_join(all_sfh, join = st_intersects) %>%
  calc_p_fireplace(col_of_interest = GEOID10)

tracts_fireplaces <- tracts %>%
  left_join(n_per_tract, by = 'GEOID10')

ggplot() +
  geom_sf(data = tracts_fireplaces, aes(fill = p_fireplace))


n_per_neighborhood <- neighborhoods %>%
  st_join(all_sfh, join = st_intersects) %>%
  calc_p_fireplace(col_of_interest = mapname)

neighborhoods_fireplaces <- neighborhoods %>%
  left_join(n_per_neighborhood, by = 'mapname')


centuries_fireplaces <- all_sfh %>%
  mutate(century = paste0(substr(as.character(year_built), 1, 2), '00s')) %>%
  calc_p_fireplace(col_of_interest = century)

demicenturies_fireplaces <- all_sfh %>%
  filter(year_built >= 1700) %>%
  mutate(fifty_years = case_when(
    year_built < 1750 ~ '1700-1749',
    year_built < 1800 ~ '1750-1799',
    year_built < 1850 ~ '1800-1849',
    year_built < 1900 ~ '1850-1899',
    year_built < 1950 ~ '1900-1949',
    year_built < 2000 ~ '1950-1999',
    year_built < 2050 ~ '2000-2049'
  )) %>%
  calc_p_fireplace(col_of_interest = fifty_years)


fireplaces_1800s <- all_sfh %>%
  filter(year_built >= 1800) %>%
  mutate(decade = case_when(
    year_built < 1810 ~ '1800s',
    year_built < 1820 ~ '1810s',
    year_built < 1830 ~ '1820s',
    year_built < 1840 ~ '1830s',
    year_built < 1850 ~ '1840s',
    year_built < 1860 ~ '1850s',
    year_built < 1870 ~ '1860s',
    year_built < 1880 ~ '1870s',
    year_built < 1890 ~ '1880s',
    year_built < 1900 ~ '1890s',
    TRUE ~ NA_character_
  )) %>%
  filter(! is.na(decade)) %>%
  calc_p_fireplace(col_of_interest = decade)


heat_1800s <- all_sfh %>%
  as_tibble() %>%
  select(-geometry) %>%
  filter(year_built >= 1800) %>%
  mutate(decade = case_when(
    year_built < 1810 ~ '1800s',
    year_built < 1820 ~ '1810s',
    year_built < 1830 ~ '1820s',
    year_built < 1840 ~ '1830s',
    year_built < 1850 ~ '1840s',
    year_built < 1860 ~ '1850s',
    year_built < 1870 ~ '1860s',
    year_built < 1880 ~ '1870s',
    year_built < 1890 ~ '1880s',
    year_built < 1900 ~ '1890s',
    TRUE ~ NA_character_
  ),
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
  


