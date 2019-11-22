# Chicago Policy Review
# Author: Charmaine Runes
# Project: Explore the Cook County Regional Gang Intelligence Database
# Source URL: https://www.propublica.org/datastore/dataset/cook-county-regional-gang-intelligence-database

devtools::install_github("hrbrmstr/albersusa")

library(tidyverse)
library(tidyr)
library(dplyr)
library(readr)
library(ggplot2)
library(here)
library(stringr)

# For mapping
library(zipcode)
library(maps)
library(viridis)
library(urbnmapr)
library(choroplethr)

gang_database <- read_csv(here("Chicago Policy Review", "Cook County Gang Database", 
                               "propublica_gang_database.csv")) 
colnames(gang_database) <- c("ID", "sex", "city", "state", "zip", "gang_id", "height",
                             "weight", "felon_record", "on_probation", "admits_gang",
                             "wears_colors", "armed", "race", "eye_color", "hair_color",
                             "create_record", "approve_record", "deceased", "ever_arrested",
                             "id_other", "self_admit", "tattoos", "cop_suspicion", "age")

# Check column types
spec(gang_database)

# Clean data - 
gang_database <- gang_database %>% 
  mutate(zip = str_replace_all(zip, "NULL", "")) %>% 
  mutate(zip = clean.zipcodes(zip)) %>% 
  mutate(state = str_replace_all(state, "NULL", "")) %>% 
  mutate(city = str_replace_all(city, "NULL", "")) %>% 
  mutate(create_record = as.Date(create_record, format = "%m/%d/%y")) %>% 
  mutate(approve_record = as.Date(approve_record, format = "%m/%d/%y")) %>% 
  mutate(lapse_days = difftime(approve_record, create_record, units = c("days"))) %>% 
  mutate(lapse_months = lapse_days / 30) %>% 
  mutate(year = as.numeric(format(create_record, '%Y'))) %>% 
  mutate(month = month.abb[(as.factor(format(create_record, '%m')))])

# Summarize the data by ZIP
summary_by_zip <- gang_database %>% 
  group_by(zip) %>% 
  drop_na() %>% 
  filter(zip != "0") %>% 
  summarise(records = n())

# Clean ZIP code data
data(zipcode) 
summary_by_zip <- merge(summary_by_zip, zipcode, by = 'zip')

# Merge with a ZIP-to-county crosswalk to get county_data
zip_to_county <- read_csv(here("Chicago Policy Review", "Cook County Gang Database", 
                               "ZIP_COUNTY_092019.csv")) 

summary_by_zip <- summary_by_zip %>% 
  left_join(zip_to_county, by = "zip")

# Summarize data by county
summary_by_county <- summary_by_zip %>% 
  mutate(county = as.character(county)) %>% 
  group_by(county) %>% 
  summarise(n = n())

colnames(summary_by_county) <- c("county_fips", "records")

# Join with counties to filter by state abbv.
summary_by_county <- summary_by_county %>% 
  left_join(counties, by = "county_fips") 

# Map records by county in neighboring states 
chicago_area <- c("IL", "IN", "WI", "MI")
chicago_region <- states %>% 
  filter(state_abbv %in% chicago_area)

summary_by_county %>% 
  filter(state_abbv %in% chicago_area) %>% 
  ggplot(aes(long, lat, group = group, fill = records)) +
  geom_polygon(color = NA) +
  geom_polygon(data = chicago_region, mapping = aes(long, lat, group = group),
               fill = NA, color = "#ffffff") +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme(legend.title = element_text(),
        legend.key.width = unit(.5, "in")) +
  labs(fill = "Gang affiliation records") 

# ========================================================================================

# Map records by ZCTA in Cook County
# Import ZIP-to-ZCTA crosswalk
zip_to_zcta <- read_csv(here("Chicago Policy Review", "Cook County Gang Database", "zip_to_zcta_2019.csv"))

# Rename column ZIP_CODE to zip for the merge
zip_to_zcta <- zip_to_zcta %>% 
  mutate(zip = ZIP_CODE)

# Merge with summary_by_zip file to get ZCTA and make sure you have a ZCTA5CE10 col to merge
cook_cty_zctas <- summary_by_zip %>% 
  left_join(zip_to_zcta, by = "zip") %>% 
  filter(county == 17031) %>% 
  group_by(ZCTA) %>% 
  summarise(records = sum(records)) %>% 
  rename(ZCTA5CE10 = ZCTA)

# Read in all ZCTA shapefiles
library(sf)
all_zctas <- st_read(here("Chicago Policy Review", "Cook County Gang Database", "tl_2019_us_zcta510/tl_2019_us_zcta510.shp"))
chicago_zctas <- st_read(here("Chicago Policy Review", "Cook County Gang Database", "geo_export_33da2ed5-b802-440b-9407-ab1580bdf6d8.shp"))

# Join the shapefile to the gang affiliation records 
chi_joined <- inner_join(chicago_zctas, summary_by_zip, by = "zip")

chi_binned <- chi_joined %>% 
  mutate(bins = cut(chi_joined$records, breaks = 3,
                    labels = c("Low", "Medium", "High")))

# Plot choropleth of record creation in Chicago, IL by ZCTA
ggplot() +
  geom_sf(chicago_zctas, mapping = aes(), fill = "grey", color = "#ffffff") +
  geom_sf(chi_binned, mapping = aes(fill = bins),
          color = "#ffffff", size = 0.25) +
  scale_fill_brewer(type = "seq", palette = "Blues", direction = 1) +
  coord_sf(datum = NA) +
  labs(title = "More gang affiliation records associated with the South Side",
       fill = "Gang affiliation records created") +
  theme(panel.background = element_rect(fill = "white"))

# =======================================================================================

# Summarize data by year and month
summary_by_month <- gang_database %>% 
  group_by(year, month, state) %>% 
  filter(year %in% 2014:2017) %>% 
  drop_na() %>% 
  summarise(records = n()) 

summary_by_month$month <- factor(summary_by_month$month, levels = month.abb)
summary_by_month$year <- factor(summary_by_month$year, levels = c(2014, 2015, 2016, 2017))

ggplot(summary_by_month, aes(x = year, y = records)) +
  geom_bar(stat = "identity", fill = "#3182bd") +
  labs(title = "Total record creation increased from 2014 to 2017",
       x = "Year",
       y = "Number of records created") +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major.y = element_line(color = "grey", linetype = "dotted"))

ggplot(summary_by_month, aes(x = month, y = records, group = year, color = year)) +
  geom_line() +
  scale_color_brewer(type = "seq", palette = "Blues") +
  labs(title = "Gang affiliation record creation increased from 2013 to 2017",
       x = "Year",
       y = "Number of records created") +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major.y = element_line(color = "grey", linetype = "dotted"))

# See only for IL
IL_records <- summary_by_month %>% 
  filter(state == "IL") 

ggplot(IL_records, aes(x = year, y = records)) +
  geom_bar(stat = "identity", fill = "#9ecae1") +
  labs(title = "Total record creation increased from 2014 to 2017",
       x = "Year",
       y = "Number of records created") +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major.y = element_line(color = "grey", linetype = "dotted"))

ggplot(IL_records, aes(x = month, y = records, group = year, color = year)) +
  geom_line() +
  scale_color_brewer(type = "seq", palette = "Blues") +
  labs(title = "Gang affiliation record creation increased from 2013 to 2017",
       x = "Year",
       y = "Number of records created") +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major.y = element_line(color = "grey", linetype = "dotted"))

# Compare IL to all states included in the gang affiliation record database
IL_to_merge <- IL_records %>% 
  group_by(year) %>% 
  summarise(records = sum(records)) %>% 
  mutate(level = "IL")

total_to_merge <- summary_by_month %>% 
  group_by(year) %>% 
  summarise(records = sum(records)) %>% 
  mutate(level = "total")

compare_IL_tot <- rbind(total_to_merge, IL_to_merge)

ggplot(compare_IL_tot, aes(x = year, y = records, fill = level)) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_fill_manual(values = c("#3182bd", "#9ecae1")) +
  labs(title = "Total record creation increased from 2014 to 2017",
       x = "Year",
       y = "Number of records created") +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major.y = element_line(color = "grey", linetype = "dotted"))

# =======================================================================================
# Make a tile map of where the records are from
summary_by_state <- gang_database %>% 
  group_by(state, year) %>% 
  drop_na() %>% 
  summarise(records = n())

install.packages("statebins", repos = "https://cinc.rud.is")
library(statebins)
packageVersion("statebins")

# Test on 2017 data
summary_by_state17 <- summary_by_state %>% 
  filter(year == 2017)

full_state_list <- as_tibble(state.abb) %>% 
  rename(state = value)

merged <- left_join(full_state_list, summary_by_state17) 
merged[is.na(merged)] <- 0

merged <- merged %>% 
  mutate(year = 2017) %>% 
  mutate(in_database = records > 0)

statebins(merged, state_col = "state", value_col = "in_database", 
          ggplot2_scale_function = scale_fill_brewer,
          name = "Gang affiliation records created") +
  theme_statebins(legend_position = "right") +
  labs(title = "Gang affiliation records created in 2017")

# Create function to map tile maps by year
plot_tilemap_by_year <- function(y) {
  
  # Filter the record data by year
  summary_by_state <- summary_by_state %>% 
    filter(year == y)
  
  # Merge with full list of states
  merged <- left_join(full_state_list, summary_by_state) 
  merged[is.na(merged)] <- 0
  merged <- merged %>% 
    mutate(year = y) %>% 
    mutate(in_database = records > 0)
  
  # Plot tile map
  map <- statebins(merged, state_col = "state", value_col = "in_database", 
            ggplot2_scale_function = scale_fill_brewer,
            name = "Gang affiliation records created") +
    theme_statebins(legend_position = "right") +
    labs(title = str_c("Gang affiliation records created in ", y))
  
  return(map)
}

# Test function
plot_tilemap_by_year(2013)
plot_tilemap_by_year(2014)
plot_tilemap_by_year(2015)
plot_tilemap_by_year(2016)
plot_tilemap_by_year(2017)
plot_tilemap_by_year(2018)

# Run function - figure out how to save these??
for (year in 2013:2018) {
  plot_tilemap_by_year(year)
}
