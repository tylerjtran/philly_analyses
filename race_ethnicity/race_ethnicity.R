


library(tidyverse); library(sf); library(tidycensus)


census_api_key(Sys.getenv('census_api'))


# This function pulls race and ethnicity data for each census tract (or other
# specified geography) in Philadelphia from the ACS. It creates a separate category
# for Latinx residents and subtracts those from other racial groups. So, the label "Black"
# actually means "Black and not Latinx" and "white" means "white and not Latinx"
get_race_ethnicity(geography = 'tract', geometry = FALSE){
  
  acs_descriptions <- load_variables(2019, "acs5", cache = TRUE)
  
  # Create a lookup table of races for each ACS variable
  lookup_race <- tibble(
    variable = c(paste0('B02001_0', str_pad(1:10, 2, pad = '0'))),
    race = c('Total', 'White', 'Black', 'Native American',
             'Asian', 'Native Hawaiian or Pacific Islander',
             'Other', 'Two or more races', NA, NA)
  )
  
  # Get % of population in each census tract (or other geography) that is Latinx
  latinx <- get_acs(geography = geography,
                    table = c(race = 'B03002'),
                    state = 'PA',
                    county = 'Philadelphia',
                    year = 2019) %>%
    # Join the ACS descriptions to more easily group by race
    left_join(acs_descriptions %>%
                filter(grepl('B03002', name),
                       ! grepl('Two races including|Two races excluding', label)) %>%
                select(-concept),
              by = c('variable' = 'name')) %>%
    # Create a flag "latinx" and a categorical variable for race based on ACS descriptions
    mutate(latinx = ! grepl('Not Hispanic', label),
           race = case_when(
             grepl('White', label) ~ 'White',
             grepl('Black', label) ~ 'Black',
             grepl('American Indian', label) ~ 'Native American',
             grepl('Asian', label) ~ 'Asian',
             grepl('Hawaii', label) ~ 'Native Hawaiian or Pacific Islander',
             grepl('Some other', label) ~ 'Other',
             grepl('Two', label) ~ 'Two or more races'
           )) %>%
    filter(! is.na(race)) %>%
    group_by(GEOID, race) %>%
    # % of people in each tract (or other geography) that are Latinx according to ACS
    summarise(p_latinx = estimate[latinx]/sum(estimate))
  
  # Get race data from ACS
  # Then join Latinx proportions for each racial group, and subtract numbers of Latinx
  # residents from other racial groups to make "Latinx" a separate group
  race <- get_acs(geography = geography,
                  table = c(race = 'B02001'),
                  state = 'PA',
                  county = 'Philadelphia',
                  year = 2019) %>%
    left_join(lookup_race, by = 'variable') %>%
    filter(! is.na(race)) %>%
    left_join(latinx, by = c('GEOID', 'race')) %>%
    # add column n_latinx with number of Latinx residents per tract per race
    # then, adjust the race numbers by subtracting Latinx residents
    mutate(n_latinx = if_else(! is.na(p_latinx), p_latinx*estimate, 0),
           estimate = estimate - n_latinx) %>%
    select(GEOID, race, estimate, n_latinx) %>%
    group_by(GEOID) %>%
    mutate(n_latinx = sum(n_latinx)) %>%
    ungroup() %>%
    rename(Latinx = n_latinx) %>%
    pivot_wider(id_cols = c(GEOID, race, Latinx), names_from = race, values_from = estimate) %>%
    pivot_longer(-GEOID, names_to = 'race_eth', values_to = 'n') %>%
    group_by(GEOID) %>%
    mutate(p = n/n[race_eth == 'Total']) %>%
    ungroup()
  
  # Separately pull geometry if geometry is TRUE so don't have to deal with that extra column
  # when manipulating data above. use tidycensus instead of tigris b/c same function for all
  # geographies
  if (geometry){
    race <- race %>%
      left_join(get_acs(geography = geography,
                        variables = 'B01001_001',
                        state = 'PA',
                        county = 'Philadelphia',
                        year = 2019,
                        geometry = T) %>%
                  select(GEOID, geometry),
                by = 'GEOID')
  }
  
  return(race)
}





