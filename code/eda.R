# WORKSPACE
pacman::p_load(tidyverse, tidymodels, tmap, sf, cowplot, ggeasy, hrbrthemes)
theme_set(theme_ipsum_ps())

# Read in data
bike_crash <- read_rds("~/Documents/ARKIV/R/Projects/bike_crash/processed_data/bike_crash.rds")

# _ _ _ _ _ _ _ _
# GEOGRAPHIC VIEWING
# tmap_mode("view")    # interactive viewing (with leaflet)
# tmap("plot")         # deafault (faster)

st_as_sf(bike_crash, coords = c(x = "x", y = "y"), crs = "NAD83") %>% 
  tm_shape() + 
  tm_dots("ambulance_r", palette = "Set1", alpha  = .25)

# We have 331 cities in North Carolina + Rural
sort(unique(bike_crash$city))

# How are the points dispersed?



# _ _ _ _ _ _ _ _
# CRASHES OVER TIME
plot_grid(
  bike_crash %>% count(crash_year) %>% ggplot(aes(x = crash_year, y = n)) + geom_bar(stat = "identity") + theme(axis.text.x=element_text(angle=90, hjust=1)),
  bike_crash %>% count(crash_month) %>% ggplot(aes(x = crash_month, y = n)) + geom_bar(stat = "identity") + theme(axis.text.x=element_text(angle=90, hjust=1)),
  bike_crash %>% count(crash_hour) %>% ggplot(aes(x = crash_hour, y = n)) + geom_bar(stat = "identity") + theme(axis.text.x=element_text(angle=90, hjust=1)),
  bike_crash %>% count(crash_day) %>% ggplot(aes(x = crash_day, y = n)) + geom_bar(stat = "identity") + theme(axis.text.x=element_text(angle=90, hjust=1)))



# _ _ _ _ _ _ _ _ _
# CRASH SEVERITY
bike_crash %>% 
  filter(!is.na(crash_sevr)) %>% 
  mutate(crash_sevr = str_sub(crash_sevr, 4)) %>% 
  group_by(crash_year) %>% 
  count(crash_sevr, ambulance_r) %>% 
  ungroup() %>% 
  ggplot(aes(x = crash_year, y = n, fill = ambulance_r)) + geom_bar(stat = "identity") + facet_wrap(vars(crash_sevr), scales="free_y") + 
  easy_rotate_x_labels() +
  labs(x = element_blank(), y = element_blank()) + 
  theme(legend.position = "bottom", 
        legend.title = element_blank())+
  scale_fill_discrete(labels = c("No ambulance required" ,"Ambulance required"))


# _ _ _ _ _ _
# Crash types: Who did what to cause the accident?
who_what <- 
bike_crash %>% 
  separate(crash_type, into = c("crash_type", "crash_subtype"), sep = " - ") %>% 
  separate(crash_type, into = c("who", "what"), sep = " ", extra = "merge") %>% 
  count(who, what, crash_subtype) %>% 
  filter(who %in% c("Bicyclist", "Motorist")) %>% 
  pivot_wider(names_from = who, values_from = n) %>% 
  arrange(-Bicyclist, -Motorist) %>% 
  print(n = 100)


