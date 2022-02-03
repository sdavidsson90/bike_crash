##########################
# WORKSPACE SETUP
pacman::p_load(tidyverse,
               tidylog,
               lubridate,
               collapse) 

##########################


# Read in data
bike_crash <- read_csv("~/R/Projects/bike_crash/raw_data/bike_crash.csv")

# Set all variable names to snake_case
colnames(bike_crash) <- janitor::make_clean_names(colnames(bike_crash))

# _ _ _ _
# Datacleaning

# Set all missing to NA's
bike_crash <- bike_crash %>% mutate(across(where(is_character), ~str_replace(., pattern = "\\.|Missing|Unknown|Unknown/Missing|missing|999", replacement = NA_character_)))

# Clean up crash_sevr
bike_crash$crash_sevr %>% unique() %>% sort() # before
bike_crash <- bike_crash %>% mutate(crash_sevr = str_sub(crash_sevr, 4),
                                    bike_injury = str_sub(bike_injury, 4),
                                    drvr_injury = str_sub(drvr_injury, 4))
bike_crash$crash_sevr %>% unique() %>% sort() # after

# Weird 70+ variable: we don't know the exact age, only age group
bike_crash %>% filter(bike_age == "70+") %>% select(bike_age, bike_age_grp) %>% print(n =100)
bike_crash$bike_age[bike_crash$bike_age == "70+"] <- NA
bike_crash$drvr_age[bike_crash$drvr_age == "70+"] <- NA

bike_crash$bike_age <- as.numeric(bike_crash$bike_age)
bike_crash$drvr_age <- as.numeric(bike_crash$drvr_age)

# - - - - - - -
# # RELEVELING
bike_lvl  <-  c("0-5", "6-10", "11-15", "16-19", "20-24", "25-29", "30-39", "40-49", "50-59", "60-69", "70+", NA)
drvr_lvl  <-  c("0-19", "20-24", "25-29", "30-39", "40-49", "50-59", "60-69", "70+", NA)
light_lvl <-  c("Dawn", "Daylight", "Dusk", "Dark - Lighted Roadway", "Dark - Unknown Lighting", "Dark - Roadway Not Lighted", "Other")
loc_lvl   <-  c("Rural (<30% Developed)", "Mixed (30% To 70% Developed)", "Urban (>70% Developed)")
speed_lvl <-  c("5 - 15 MPH", "20 - 25  MPH", "30 - 35  MPH" , "40 - 45  MPH", "50 - 55  MPH", "60 - 75 MPH")
sevr_lvl  <-  c("No Injury", "Possible Injury", "Suspected Minor Injury", "Suspected Serious Injury", "Killed")

bike_crash <- 
  bike_crash %>% 
  mutate(bike_age_grp = factor(bike_age_grp, levels = bike_lvl),
         drvr_age_grp = factor(drvr_age_grp, levels = drvr_lvl),
         light_cond   = factor(light_cond,   levels = light_lvl),
         locality     = factor(locality,     levels = loc_lvl  ),
         speed_limit  = factor(speed_limit,  levels = speed_lvl),
         crash_sevr   = factor(crash_sevr,   levels = sevr_lvl),
         ambulance_r  = factor(ambulance_r,  levels = c("Yes", "No")))

rm(bike_lvl, drvr_lvl, light_lvl, loc_lvl, speed_lvl, sevr_lvl)

# DATE / TIME VARIABLES
bike_crash$crash_year  <- factor(bike_crash$crash_year,  levels = sort(unique(bike_crash$crash_year)))
bike_crash$crash_month <- factor(bike_crash$crash_month, levels = unique(bike_crash$crash_month))
bike_crash$crash_day   <- factor(bike_crash$crash_day,   levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
bike_crash$crash_hour  <- factor(bike_crash$crash_hour,  levels = sort(unique(bike_crash$crash_hour)))

# - - - - - - -
# Simplification of select variables

# Drugs & alcohol
# These two are a little inconsistent. 
bike_crash %>% count(bike_alc_drg, bike_alc_flg)
bike_crash %>% count(drvr_alc_drg, drvr_alc_flg)

# Let's simplify and make more consistency:
bike_crash$bike_alc_drg[str_detect(bike_crash$bike_alc_drg, pattern = "suspected")] <- "Suspected"
bike_crash$bike_alc_drg[str_detect(bike_crash$bike_alc_drg, pattern = "detected")]  <- "Detected"
bike_crash$bike_alc_drg[is.na(bike_crash$bike_alc_drg)] <- "No"
bike_crash$bike_alc_drg <- as.factor(bike_crash$bike_alc_drg, levels = c("Suspected", "Detected", "No"))

bike_crash$drvr_alc_drg[str_detect(bike_crash$drvr_alc_drg, pattern = "suspected")] <- "Suspected"
bike_crash$drvr_alc_drg[str_detect(bike_crash$drvr_alc_drg, pattern = "detected")]  <- "Detected"
bike_crash$drvr_alc_drg[is.na(bike_crash$drvr_alc_drg)] <- "No"
bike_crash$drvr_alc_drg <- as.factor(bike_crash$drvr_alc_drg, levels = c("Suspected", "Detected", "No"))

# Simplification of lanes
bike_crash %>% count(num_lanes)
bike_crash <- bike_crash %>% mutate(num_lanes = as.numeric(str_sub(num_lanes, 0,1)))

# Is the road bent or connected to hill?
bike_crash <- bike_crash %>% 
  separate(rd_characte, into = c("rd_bent", "rd_hill"), sep = " - ") %>%
  mutate(rd_hill = if_else(rd_hill == "Level", "No", "Yes"))

bike_crash$rd_bent[bike_crash$rd_bent == "Other"] <- NA

# Is the road clean or dirty (wet, loose due to sand/gravel)
bike_crash <- bike_crash %>% mutate(rd_conditio = if_else(rd_conditio == "Dry", "Clean", "Dirty/wet"))

# Does the road have defects?
bike_crash <- bike_crash %>% mutate(rd_defects = if_else(rd_defects == "None", "No", "Yes")) 

# - - - - - - 

# Export final dataset
write_rds(bike_crash, file = "processed_data/bike_crash.rds")


