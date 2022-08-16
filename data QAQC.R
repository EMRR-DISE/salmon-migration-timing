# Pete Nelson, PhD
# Department of Water Resources
# Salmon Phenology study

# purpose: QAQC for DJFMP trawl data

# focus on Chipps Island and Chinook (all runs)
# primary concerns are
# (in)consistency of sampling effort
# missing trawl volume data (means cpue can't be calculated)
# and extreme values for trawl volume
# select appropriate tow duration range
# identify appropriate upper limit for CHN size

# most of the above has already been addressed in 'data acquisition'; there may
# be a benefit for adding any useful code from below to the 'data acquisition'
# script and scrapping this one

rm(list = ls(all.names = TRUE)) # clear R environment

library(tidyverse)
library(readxl)
library(tsibble) # yearweek()
library(lubridate) # yday()

# read trawls data (unedited except that not all variables included)
trawls <- read_csv("data/trawls.csv")

# generate conversion table of dates
do_water_year <- tibble(date = seq(as.Date("2000/10/1"), as.Date("2001/9/30"), "days"))
do_water_year <- do_water_year %>% 
  mutate(doy = yday(date), # day of each Julian year
         wdoy = wy_day_new(date)) # day of each water year
write.csv(do_water_year, "do_water_year.csv")
print(do_water_year %>% filter(wdoy >= 200 & wdoy <= 250), n = 51)

# data quality ----
## volume (of tow) data ----
# number of records where volume = NA is highly variable
rec_NA <- trawls %>% 
  filter(is.na(volume)) %>% # select records where volume = NA
  mutate(year = year(sample_date)) %>% 
  group_by(year) %>% 
  summarise(n = n()) # count number of such records for each year

rec_all <- trawls %>% 
  mutate(year = year(sample_date)) %>% 
  group_by(year) %>% 
  summarise(n = n()) # count number all records for each year

rec_comp <- right_join(rec_NA, 
                      rec_all, 
                      by = "year") %>% 
  transmute(year = year,
          volume_NAs = n.x,
          total_records = n.y,
          pct_NAs = volume_NAs/total_records*100) %>% # calc percentage of records where volume = NA
  replace_na(list(volume_NAs = 0, pct_NAs = 0)) %>% # replace NAs w 0 (some years, no volume = NA)
  arrange(year)

print(rec_comp, n=Inf)

# how do years compare in terms of the number of records?
print(arrange(rec_all, n), n = Inf) 
# 1984, 1982 and (maybe) 1981 stand out...2021 is low too but it's incomplete

# how do years compare in terms of the number of NAs?
print(arrange(rec_comp, total_records), n = Inf) 
# 1984 is the one that stands out...

# view the 1984 data
View(trawls %>% 
       filter(year(sample_date) == "1984") %>% 
       select(station_code, sample_date, tow_duration, volume,
              common_name, fork_length, race_by_length, count))

# should re-do but using tidy data--each line here is a speciesxsize, so
# numbers of entries are not representative of the number of problematic trawls
no_vol <- trawls %>% 
  filter(is.na(volume)) %>% 
  select(sample_date, method_code, tow_number, common_name) %>% 
  mutate(year = year(sample_date))
print(no_vol %>% group_by(year) %>% summarise(n = n()), n = Inf)
# 1993 and 1994, 2013+ look potentially problematic
print(no_vol %>% 
        filter(year == 1993) %>% 
        group_by(month(sample_date)) %>% 
        summarise(n = n()))
print(no_vol %>% 
        filter(year == 1994) %>% 
        group_by(month(sample_date)) %>% 
        summarise(n = n()))
print(no_vol %>% 
        filter(year >= 2013 & year < 2020) %>% 
        group_by(month(sample_date)) %>% 
        summarise(n = n())) # what was going on?!

## tagged releases ####
# influence of hatchery releases; they used to release a lot at smaller sized 
# salmon earlier in the season, but concern over competition with natural 
# production and lower survival rates overall caused a shift to releasing only 
# larger and later. It would be difficult to account for this unless all the 
# releases were at least fractionally marked. Then we could subtract from the 
# catch density based on CWT tag.
trawls %>% 
  filter(iep_fish_code == "CHISAL") %>% 
  group_by(mark_code) %>% 
  summarise(n = n(),
            tag_code_NAs = sum(is.na(tag_code)))

trawls %>% 
  filter(iep_fish_code == "CHISAL") %>% 
  group_by(mark_code) %>% 
  summarise(tag_code_NAs = sum(is.na(tag_code)))

sum(!is.na(trawls$tag_code)) # number of records including a tag_code
sum(is.na(trawls$tag_code)) # number without a tag_code


# data files ---------------------------------------------------------------------



# clear R environment ####
rm(list = ls(all.names = TRUE)) 

# SCRATCH PAPER -----------------------------------------------------------


