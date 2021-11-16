
# Initial Reviewing of datasets

# Bicyclist and pedestrian crashes that occurred in North Carolina between years 2007 through 2019

bc_kaggle <- vroom("raw_data/bike_crash (kaggle).csv", col_types = cols(.default = "c"))
bc_ch <- vroom("raw_data/bike_crash (ch open data hub).csv", col_types = cols(.default = "c"))
ped_ch <- vroom("raw_data/pedestrian_crashes (ch open data hub).csv", col_types = cols(.default = "c"))

# _ _ _ _ 
# # Er kaggle og ch de samme?
# Hvor mange ID'er svarer til hinanden? 
sum(bc_ch$CrashID == bc_kaggle$CrashID ) == nrow(bc_kaggle)
sum(colnames(bc_ch) == colnames(bc_kaggle)) == length(bc_kaggle)

intersect(bc_ch, bc_kaggle)
# Noget er galt

# Foreløbig konklusion er ja, det er det samme
# _ _ _ _ 
nrow(bc_ch)
nrow(ped_ch)
# ped indeholder flere rækker end bc
# Er nogle af rækkerne de samme som i bc_ch?
a <- colnames(ped_ch)
b <- colnames(bc_ch)

bc_kaggle %>% filter(CrashID %in% ped_ch$CrashID)
bc_ch %>% filter(CrashID %in% ped_ch$CrashID)

# Hvilke variabler er de samme?
same <- intersect(a, b)

p2 <- ped_ch %>% select(same)
b2 <- bc_ch %>% select(same)

b2 <- arrange(b2, by = CrashID)
p2 <- arrange(p2, by = CrashID)

intersect(p2, b2)
rm(a, b, b2, p2, same)


