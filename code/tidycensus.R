# Required packages
pacman::p_load(tidyverse, tidycensus, sf, USAboundaries)

# Specify years we need
years <- 2009:2019

# Load in data from API
nc_pop <- 
  map_dfr(years, ~ get_acs(variables = "B01001_001", 
                           geography = "county", 
                           state = "NC",
                           year = .x) %>% 
            separate(NAME, into = c("county", "state"), sep = ", ") %>% 
            mutate(year = str_c("est_", .x)))  %>% 
  select(county, estimate, year) %>% 
  pivot_wider(names_from = year, values_from = estimate)


# We'ere gonna put a geometry upon it once (preferable to do it once than 11 times)

# Select states and counties
# For some reason this dataset has duplicate colnames, so we'll have to do it this way.
nc_geom <- us_counties()[,c(7, 13)] %>% rename(county = namelsad, state = state_name) %>% filter(state == "North Carolina")

nc_pop <- left_join(nc_pop, ncc_geom, by = "county")

# clean up


# Write data to disk
write_rds(nc_pop, file = "~/R/Projects/bike_crash/processed_data/ncpop_09-19.rds")
write_rds(nc_geom, file = "~/R/Projects/bike_crash/processed_data/nc_counties_geom.rds")

rm(ncc_geom, years)
