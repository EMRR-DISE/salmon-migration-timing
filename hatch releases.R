# Pete Nelson, PhD
# Department of Water Resources
# Salmon Phenology study

# purpose: acquire hatchery release data from Sturrock et al. 2015

# DOI:10.1371/journal.pone.0122380
# obtained from the 'About' tab here: https://baydeltalive.com/fish/hatchery-releases

library(readxl)

hatch_rel <- read_xlsx("data/CVFRChin_RELEASE DB_v3_POSTED030419.xlsx", 
                       sheet = "DBTable1")
# remove data lacking "Avg_date", years prior to 1976, and group by year(Avg_date)
hr1 <- hatch_rel %>% drop_na(., Avg_date) %>% 
  filter(year(Avg_date) >= "1976") %>% 
  group_by(year = year(Avg_date)) %>% 
  summarise(release = sum(Total_N))
barplot(release ~ year, data = hr1, ylab = "# of fish", main = "Hatchery Chinook Releases")
# use julian day to determine median day of year for each year...
hr2 <- hatch_rel %>% drop_na(., Avg_date) %>% 
  filter(year(Avg_date) >= "1976") %>% 
  group_by(year = year(Avg_date)) %>% 
  summarise(release = sum(Total_N),
            median_doy = median(Jday_end))
plot(median_doy ~ year, data = hr2, ylab = "median Julian day of the year",
     main = "Timing of Hatchery Releases")

# fish from Coleman Hatchery only; limited release locations
coleman_rel <- read_xlsx("data/Coleman releases.xlsx")
cr1 <- coleman_rel %>% drop_na(., Avg_date)
boxplot(Jday_end ~ Year_end, data = cr1, ylab = "Julian Day",
        main = "Coleman Hatchery Fish Release Timing")  
cr2 <- coleman_rel %>% drop_na(., Avg_date) %>% 
  group_by(year = year(Avg_date)) %>% 
  summarise(release = sum(Total_N),
            median_doy = median(Jday_end))
plot(median_doy ~ year, data = cr2, ylab = "median Julian day of the year",
     main = "Timing of Coleman Hatchery Fish Releases")
