# Pete Nelson, PhD
# Department of Water Resources
# Salmon Phenology study

# purpose: compare salmon abundance as it varies over the course of
# a year and among years

# comp years 76-21 ----

# goal: compare salmon abundance as it varies over the course of
# a year and among years

# note: all cpue calculations generate an estimate based on the 
# row unit, ie if each row is a day, then each cpue value is the
# cpue for that whole day (day's total catch/day's total volume);
# if each row is a week, then each cpue value is the cpue for 
# that whole week (week's total catch/week's total volume)

# library
library(tidyverse)
library(ggridges)
library(ggplot2)
library(viridis)
library(hrbrthemes)
library(forcats)
library(tsibble) # yearweek()

# data prep ---- 
# need df with standardized count per day (daily cpue=sum count/sum vol)
# may need to group_by and sum by week
# work from chipps

## daily summary ----
# tsbl w total fish & total volume for each sample date
daily <- chipps %>% 
  group_by(date) %>% 
  summarise(fish = sum(count),
            vol = sum(vol),
            cpue = fish/vol,
            n = n()) %>% 
  complete(date = seq.Date(min(date), 
                           max(date), 
                           by = "day")) %>% 
  mutate(year_wk = yearweek(date)) %>%  # week of the year (numeric)
  relocate("year_wk", .after = date)

## weekly summary ----
weekly <- daily %>% 
  group_by(year_wk) %>% 
  summarise(year = year(date),
            month = as.factor(month.abb[month(date)]),
            fish = sum(fish, na.rm = TRUE),
            vol = sum(vol, na.rm = TRUE),
            cpue = sum(cpue, na.rm = TRUE),
            n = sum(n, na.rm = TRUE)) %>% 
  arrange(year_wk) %>% 
  distinct(year_wk, .keep_all = TRUE)

ungroup(daily)

################
# problem: no "water_yr" in daily (yet)
daily2 <- chipps %>% 
  group_by(date) %>% 
  summarise(fish = sum(count),
            vol = sum(vol),
            cpue = fish/vol,
            n = n()) %>% 
  complete(date = seq.Date(min(date), 
                           max(date), 
                           by = "day")) %>% 
  mutate(Date = date, # req to addWaterYear()
         year_wk = yearweek(date)) %>%  
  addWaterYear() %>% 
  select(-Date) %>% 
  rename(water_yr = waterYear) 

daily2 <- left_join(daily2, yeartypes[,1:4],
                    by = c("water_yr" = "year")) %>% 
  mutate(doy = yday(date), # day of each Julian year
         wdoy = wy_day_new(date))

print(daily2, width = Inf)


########################

ungroup(chipps)

## monthly summary ----
### tsibble approach ----
#### daily ----
daily_tsbl <- tsibble(daily, index = date, regular = TRUE) %>% 
  fill_gaps()
daily_tsbl
write_csv(daily_tsbl, "data/daily_tsbl.csv")

daily2_tsbl <- tsibble(daily2, index = date, regular = TRUE) %>% 
  fill_gaps()
daily2_tsbl

#### weekly ----
weekly_tsbl <- daily_tsbl %>% 
  mutate(year = year(date),
         week = isoweek(date)) %>% # add numeric week column
  index_by(year) %>% 
  index_by(week) %>% 
  summarise(fish = sum(fish, na.rm = TRUE),
            vol = sum(vol, na.rm = TRUE),
            n = sum(n, na.rm = TRUE),
            cpue = sum(cpue, na.rm = TRUE))
# appears to work, still to be checked
weekly_tsbl

#### monthly ----
monthly_tsbl <- daily_tsbl %>% 
  mutate(year = year(date),
         month = month(date)) %>% 
  index_by(year) %>% 
  index_by(month) %>% 
  summarise(fish = sum(fish, na.rm = TRUE),
            vol = sum(vol, na.rm = TRUE),
            n = sum(n, na.rm = TRUE),
            cpue = sum(cpue, na.rm = TRUE))
# looks good; haven't verified this yet either
monthly_tsbl

### from daily -----
# summary data almost surely wrong, though month-year looks good
monthly <- daily %>% 
  group_by(month = month(date)) %>% 
  summarise(year = year(date),
            month = as.factor(month.abb[month(date)]),
            fish = sum(fish, na.rm = TRUE),
            vol = sum(vol, na.rm = TRUE),
            cpue = sum(cpue, na.rm = TRUE),
            n = sum(n, na.rm = TRUE)) %>% 
  distinct(paste(month, year), .keep_all = TRUE) %>% 
  arrange(year, month) %>% 
  select(-'paste(month, year)')

ungroup(weekly)

### from weekly ----
# summary data look right, but every year begins w May!
monthly2 <- weekly %>% 
  group_by(month, year) %>%
  summarise(year = year,
            month = as.factor(month),
            fish = sum(fish, na.rm = TRUE),
            vol = sum(vol, na.rm = TRUE),
            cpue = sum(cpue, na.rm = TRUE),
            n = sum(n, na.rm = TRUE)) %>% 
  distinct(paste(month, year), .keep_all = TRUE) %>% 
  arrange(year, month) %>% 
  select(-'paste(month, year)')

### from weekly w factors ----
# complete mess
monthly3 <- weekly %>% 
  group_by(month, year) %>%
  summarise(year = year,
            month = factor(month.name[month], # severely compromised!
                           levels = month.name),
            fish = sum(fish, na.rm = TRUE),
            vol = sum(vol, na.rm = TRUE),
            cpue = sum(cpue, na.rm = TRUE),
            n = sum(n, na.rm = TRUE)) %>% 
  distinct(paste(month, year), .keep_all = TRUE) %>% 
  arrange(year, month) %>% 
  select(-'paste(month, year)')

# barf
monthly4 <- daily %>% 
  group_by(year = year(date), year_wk = year_wk) %>% 
  summarise(year = year(date),
            month = as.factor(month.abb[month(date)]),
            year_mth = year(date) + month(date),
            fish = sum(fish, na.rm = TRUE),
            vol = sum(vol, na.rm = TRUE),
            cpue = sum(cpue, na.rm = TRUE),
            n = sum(n, na.rm = TRUE)) %>% 
  distinct(year_mth, .keep_all = TRUE)

# ???
monthly5 <- daily %>% 
  group_by(year = year(date), month(date)) %>% 
  summarise(year = year(date),
            month = as.factor(month.abb[month(date)]),
            year_mth = year(date) + month(date),
            fish = sum(fish, na.rm = TRUE),
            vol = sum(vol, na.rm = TRUE),
            cpue = sum(cpue, na.rm = TRUE),
            n = sum(n, na.rm = TRUE)) %>% 
  distinct()

## start example ##############
library(tidyverse)
library(lubridate)
library(tsibble)
date <- ymd(c("1976-05-18", "1976-05-19", "1976-05-24", "1976-06-01"))
fish <- c(203, 282, 301, 89)
volume <- c(210749, 287555, 378965, 308935)
n <- c(5, 7, 10, 8)
tbl <- tibble(date, fish, volume, n)
tsbl <- tsibble(tbl, index = date, regular = FALSE)
tsbl <- tsibble(tbl, index = date, regular = TRUE)
tbl_1 <- complete(tbl, date = seq.Date(min(date), max(date), by = "day"))



## end example ############

# df w total fish & total volume for each sample date
weekly <- chipps %>% 
  group_by(date, wdoy) %>% 
  summarise(cpue = sum(count)/sum(vol),
            n = n()) %>% 
  mutate(week = isoweek(date), # week of the year (numeric)
         month = month(date, label = TRUE, abbr = FALSE),
         year = year(date)) %>%
  relocate(c("week", "month", "year"), .after = wdoy) %>% 
  group_by(year, month, week) %>% 
  summarise(cpue_m = mean(cpue),# cpue for each week
            cpue_sd = sd(cpue),
            n = n()) %>% # total number of tows each week
  mutate(cpue = cpue_m) %>% 
  select(-cpue_m) %>% 
  relocate(cpue_sd, .after = cpue) %>% 
  relocate(n, .after = cpue_sd)

# regularize chipps
all_dates <- seq.Date(min(chipps$date),
                      max(chipps$date),
                      "day")
library(tsibble)
daily_tsbl <- daily %>% 
  as_tsibble(index = date, key = cpue) %>% 
  arrange(date) # otherwise sorted by key

# ridgeline plots ----
# colors from drought report
pal_drought <- c("D" = "#FDE333", "N" = "#53CC67","W" = "#00588B")
pal_yrtype <- c("Critical" = "#FDE333", "Dry" = "#53CC67", "Below Normal" = "#009B95","Above Normal" = "#00588B", "Wet" = "#4B0055")

# labels from Rosie's "Integrated data set.xlsx"
labels_yrtype <- c("Critical", "Dry", "Below Normal", "Above Normal", "Wet")
labels_drought <- c("Dry", "Neutral", "Wet")

# shows effort (crudely)
ggplot(daily, 
       aes(month, year, height = cpue, 
           group = year, 
           fill = factor(month))) +
  ggtitle(("Months with MWTR at Chipps Island")) +
  geom_ridgeline_gradient() +
  scale_fill_viridis_d(direction = -1, guide = "none") +
  theme(axis.text.x = element_text(angle = 90, hjust = 0))

# ridgeline of cpue by week
ggplot(weekly_tsbl, aes(week, 
                         year, 
                         height = cpue*500, # multiple to visualize some effect
                         group = year)) +
  geom_ridgeline(fill = "gray") +
  ggtitle("CPUE for Chinook Salmon at Chipps Island")

# ridgeline of cpue by day
ggplot(daily2_tsbl, aes(wdoy, 
                        water_yr, 
                        height = cpue*500, # multiple to visualize some effect
                        group = water_yr)) +
  geom_ridgeline(fill = "gray") +
  ggtitle("CPUE for Chinook Salmon at Chipps Island")

# example (https://r-graph-gallery.com/294-basic-ridgeline-plot.html#color)
ggplot(lincoln_weather, aes(x = `Mean Temperature [F]`, y = `Month`, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis(name = "Temp. [F]", option = "C") +
  labs(title = 'Temperatures in Lincoln NE in 2016') +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  )



# another (alternative) example
# w cpue on the wrong axis!
ggplot(daily, aes(x = cpue, y = as.factor(week))) +
  geom_density_ridges(rel_min_height = 0.005) +
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_x_continuous(expand = c(0.01, 0)) +
  theme_ridges()

ggplot(daily, aes(x = as.factor(week), y = cpue)) +
  geom_density_ridges(rel_min_height = 0.005) +
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_x_continuous(expand = c(0.01, 0)) +
  theme_ridges()

# code getting closer but I want response to be cpue and x=month
# and this really seems to be a histogram w response=frequency
ggplot(daily, aes(x = cpue, y = as.factor(year), fill = ..x..)) +
  geom_density_ridges_gradient(scale = 8, rel_min_height = 0.01) +
  scale_fill_viridis(name = "CPUE", option = "C") +
  labs(title = 'Daily CPUE, Chinook Salmon') +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  )
ggplot(daily, aes(x = cpue, y = month, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 8, rel_min_height = 0.01) +
  scale_fill_viridis(name = "CPUE", option = "C") +
  labs(title = 'Daily CPUE, Chinook Salmon') +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  )

# NOTES #################
months <- c("January", "February", "March", "April", "May")
d <- tibble(month = rep(factor(months, levels = month.name), 3), 
                year = c(rep("2020", 5), rep("2021", 5), rep("2022", 5)),
                height = c(0, 1, 3, 4, 0, 1, 2, 3, 5, 4, 0.5, 5, 4, 4, 1))

ggplot(d, aes(month, year, height = height, group = year)) + 
  geom_ridgeline(fill="yellow")

ggplot(monthly, aes(month, year, height = cpue, group = year)) +
  geom_ridgeline(fill = "gray") 

ggplot(monthly, aes(month, year, height = cpue,
                   group = year, fill = factor(month))) +
  geom_ridgeline() +
  scale_fill_viridis_d(direction = 1, guide = "none")

trial <- tibble(
  period = c(rep("A", 12), rep("B", 12), rep("C", 12)),
  month = rep(month.name, 3),
  cpue = c(0,0,3,5,4,1,0,0,0,0,0,0, # period A
           0,1,4,4,2,1,0,0,0,0,0,0, # period B
           0,2,5,4,1,0,0,0,0,0,0,0) # period C
) %>% mutate(month = as_factor(month))

ggplot(trial, 
       aes(period, # substitute for 'year' in the actual data
           month, 
           height = cpue, 
           group = month)) +
  geom_ridgeline(fill = "gray") +
  ggtitle("experimental ridgeline plot")
