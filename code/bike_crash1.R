# OPSÆTNING AF WORKSPACE
pacman::p_load(tidyverse,   # 
               tidylog,    # 
               vroom,
               tidymodels,
               collapse,
               sf,
               tmap
) 

options(scipen = 999)

##########################
# Indlæsning af data 
bike_crash <- vroom("raw_data/bike_crash.csv") 
colnames(bike_crash) <- janitor::make_clean_names(colnames(bike_crash))

# https://opendata-townofchapelhill.hub.arcgis.com/datasets/NCDOT::bicycle-crashes-2007-to-2019/explore?location=35.130408%2C-79.817750%2C7.48

# https://opendata-townofchapelhill.hub.arcgis.com/search?tags=transportation%2Cmovement

# # # NA-ballade
# Vi laver forskellige typer missings om til NA (Det her må kunne gøres smartere på én sætning)
bike_crash <- bike_crash %>% mutate(across(where(is_character), ~na_if(., ".")))
bike_crash <- bike_crash %>% mutate(across(where(is_character), ~na_if(., "Missing")))
bike_crash <- bike_crash %>% mutate(across(where(is_character), ~na_if(., 999)))
bike_crash <- bike_crash %>% mutate(across(where(is_character), ~na_if(., "Unknown")))
bike_crash <- bike_crash %>% mutate(across(where(is_character), ~na_if(., "Unknown/Missing")))



# kontrol - Vi prøver på én sætning
str_count


??use_github

# Note:
# Man kan også bruge map_dfr, men den er dobbelt så langsom
# map_dfr(bike_crash, ~na_if(.x, ".")) 

# # # RELEVELING

# 1) 
# Her kan man godt vælge, at lave en generel funktion. Men en god programmør ved også at man nogle gange skal springe over hvor gærdet er lavest. TIME IS MONEY
bike_lvl  <- c("0-5", "6-10", "11-15", "16-19", "20-24", "25-29", "30-39", "40-49", "50-59", "60-69", "70+")
drvr_lvl  <- c("0-19", "20-24", "25-29", "30-39", "40-49", "50-59", "60-69", "70+")
light_lvl <-  c("Dawn", "Daylight", "Dusk", "Dark - Lighted Roadway", "Dark - Unknown Lighting", "Dark - Roadway Not Lighted", "Other")
loc_lvl   <- c("Rural (<30% Developed)", "Mixed (30% To 70% Developed)", "Urban (>70% Developed)")
speed_lvl <- c("5 - 15 MPH", "20 - 25  MPH", "30 - 35  MPH" , "40 - 45  MPH", "50 - 55  MPH", "60 - 75 MPH")

bike_crash %>% 
  mutate(bike_age_grp = factor(bike_age_grp, levels =  bike_lvl),
         drvr_age_grp = factor(drvr_age_grp, levels =  drvr_lvl),
         light_cond   = factor(light_cond,   levels = light_lvl),
         locality     = factor(locality,     levels = loc_lvl  ),
         speed_limit  = factor(speed_limit,  levels = speed_lvl))

rm(bike_lvl, drvr_lvl, light_lvl, loc_lvl, speed_lvl)

# 2)
# DATO / TID VARIABLE. 
bike_crash$crash_year <- factor(bike_crash$crash_year, levels = sort(unique(bike_crash$crash_year)))
bike_crash$crash_month <- factor(bike_crash$crash_month, levels = unique(bike_crash$crash_month))
bike_crash$crash_day <- factor(bike_crash$crash_day, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
bike_crash$crash_hour <- factor(bike_crash$crash_hour, levels = sort(unique(bike_crash$crash_hour)))

# # # Mærkeligt alder 70 + 
bike_crash %>% count(drvr_age, drvr_age_grp) %>% print(n = nrow(.))
funique(bike_crash$drvr_age) %>% system.time

# ______________________________________________
# 
# EDA

# glimpse(bike_crash)
# bike_crash %>% count(crash_year) %>% ggplot(aes(x = crash_year, y = n)) + geom_bar(stat = "identity")
# bike_crash %>% count(crash_month) %>% ggplot(aes(x = crash_month, y = n)) + geom_bar(stat = "identity")
# bike_crash %>% count(crash_hour) %>% ggplot(aes(x = crash_hour, y = n)) + geom_bar(stat = "identity")
# bike_crash %>% count(crash_day) %>% ggplot(aes(x = crash_day, y = n)) + geom_bar(stat = "identity")


# Modeling
colnames(bike_crash)

# Vi har brug for at outcome variabel er factor
bike_crash$ambulance_r <- factor(bike_crash$ambulance_r)

# Vi splitter
set.seed(666)
bike_split <- initial_split(bike_crash, .75, strata = ambulance_r) 
train_bike <- bike_split %>% training
test_bike <- bike_split %>% testing

# Logistic regression
logreg <- logistic_reg() %>% 
  set_engine("glm")

logreg_bike <- logreg %>% fit(ambulance_r ~ drvr_sex +  bike_sex +  bike_age, data = train_bike)
tidy(logreg_bike)

summary(logreg_bike)

# Random forest
rf_spec <- rand_forest() %>% 
  set_mode("classification") %>% 
  set_engine("ranger")

rf_bike <- rf_spec %>% fit(ambulance_r ~ drvr_sex +  bike_sex +  bike_age, data = train_bike)

# Mapping 

# Vi laver et SF-objekt
bike_crash <- st_as_sf(bike_crash, coords = c("x", "y"), crs = "WGS84")
# Quick thematic map
qtm(bike_crash)

glimpse(bike_crash)



