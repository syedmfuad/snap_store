

rm(list=ls())

library(tigris)
options(tigris_use_cache = TRUE)
library(tidycensus)
library(dplyr)

setwd("C:/Users/syedm/Desktop/Virtualenv")

set.seed(12345)
library(caret)
data1<-read.csv("data.csv")

data1 = data1[data1$Latitude > 0, ] 
data1 = data1[data1$Longitude < 0, ]

unique(data1$Store.Type)

data <- subset(data1, Store.Type == "Large Grocery Store" | 
                 Store.Type == "Medium Grocery Store" | Store.Type == "Supermarket")

#data <- data1

library(stringr)
data$aut_date <- str_split_fixed(data$Authorization.Date, "/", 3)
data$end_date <- str_split_fixed(data$End.Date, "/", 3)

data <- subset(data, aut_date[,3] >= 1990)



pop00 <- get_decennial(
  geography = "county", 
  variables = "P001001",
  year = 2000,
  geometry=TRUE
) %>% shift_geometry() 

pop20 <- get_decennial(
  geography = "county",
  variables = "P1_001N",
  year = 2020
)


pop10 <- get_decennial(
  geography = "county", 
  variables = "P001001",
  year = 2010, 
  geometry = TRUE
) 


pop10$row <- as.numeric(rownames(pop10))

coord <- data.frame(lat = c(data$Latitude), long = c(data$Longitude)) %>%
  sf::st_as_sf(coords = c("long", "lat"),
               crs = sf::st_crs(pop10))

coord$row <- as.numeric(sf::st_within(coord, pop10))

data <- cbind(data, coord)
data$geometry <- NULL

data[ ,c('Record.ID', 'Store.Name', 'Store.Type', 'Street.Number',
         'Street.Name', 'Additional.Address', 'City', 'State', 'Zip.Code', 'Zip4', 'County')] <- list(NULL)

new_data <- merge(data, pop10, by="row", all.x = TRUE)


group_mean_aut <- new_data %>%
  group_by(aut_date[,3], NAME) %>%
  summarise(total_count=n(),
            .groups = 'drop')

group_mean_end <- new_data %>%
  group_by(end_date[,3], NAME) %>%
  summarise(total_count=n(),
            .groups = 'drop')

group_mean_aut$`aut_date[, 3]` <- as.factor(group_mean_aut$`aut_date[, 3]`)
group_mean_end$`end_date[, 3]` <- as.factor(group_mean_end$`end_date[, 3]`)

summary(group_mean_aut)
summary(group_mean_end)

names(group_mean_aut)[names(group_mean_aut) == "aut_date[, 3]"] <- 'date'
names(group_mean_end)[names(group_mean_end) == "end_date[, 3]"] <- 'date'

group_mean_aut$date <- sub("^$", 0, group_mean_aut$date)
group_mean_end$date <- sub("^$", 0, group_mean_end$date)

group_mean <- merge(group_mean_aut, group_mean_end, by=c("date", "NAME"), all.x=TRUE)
group_mean[is.na(group_mean)] <- 0

group_mean$change <- group_mean$total_count.x-group_mean$total_count.y

group_mean <- group_mean %>%
  arrange(NAME, date) %>% 
  group_by(NAME) %>%
  mutate(total_stores = cumsum(change)) %>%
  ungroup()


pop10 <- get_decennial(
  geography = "county", 
  variables = "P001001",
  year = 2010, 
  geometry = TRUE
) %>% shift_geometry()

newdata <- pop10 %>% 
  left_join(group_mean, by="NAME")


state_overlay <- states(
  cb = TRUE,
  resolution = "20m"
) %>%
  filter(GEOID != "72") %>%
  shift_geometry()


library(ggplot2)

ggplot() +
  geom_sf(data = newdata, aes(fill = total_stores, color = total_stores), size = 0.1) +
  geom_sf(data = state_overlay, fill = NA, color = "black", size = 0.1) +
  scale_fill_brewer(palette = "PuOr", direction = -1) +
  scale_color_brewer(palette = "PuOr", direction = -1, guide = FALSE) +
  coord_sf(datum = NA) +
  theme_minimal(base_family = "Roboto") +
  labs(title = "Net migration per 1000 residents by county",
       subtitle = "US Census Bureau 2022 Population Estimates",
       fill = "Rate",
       caption = "Data acquired with the R tidycensus package | @kyle_e_walker")



















library(tidycensus)
library(tidyr)
library(dplyr)

col_income <- get_acs(
  geography = "county",
  variables = c(poverty = "B19013_001"),
  year = 2010,
  geometry = TRUE) %>% shift_geometry()

ggplot(col_income) +
  geom_sf() +
  geom_sf(data = coord, aes(color = is.na(row)))

col_income$row <- as.numeric(rownames(col_income))

#boundary <- st_bbox(col_income)

#data <- subset(data, hlong >= boundary[1] & hlong <= boundary[3] & hlat >= boundary[2] & hlat <= boundary[4])

coord <- data.frame(lat = c(data$Latitude), long = c(data$Longitude)) %>%
  sf::st_as_sf(coords = c("long", "lat"),
           crs = sf::st_crs(col_income))

coord$row <- as.numeric(sf::st_within(coord, col_income))

data <- cbind(data, coord)
data$geometry <- NULL

#col_income_2 <- st_join(data, col_income, join = st_nearest_feature, by=row)

new_data <- merge(data, col_income, by="row", all.x = TRUE)


group_mean <- new_data %>%
  group_by(NAME) %>%
  summarise(total_count=n(),
            .groups = 'drop')

newdata <- merge(group_mean, col_income, by="NAME", all.x=TRUE, duplicateGeoms = T)

newdata <- col_income %>% 
  left_join(group_mean, by="NAME")


state_overlay <- states(
  cb = TRUE,
  resolution = "20m"
) %>%
  filter(GEOID != "72") %>%
  shift_geometry()


library(ggplot2)

ggplot() +
  geom_sf(data = newdata, aes(fill = total_count, color = total_count), size = 0.1) +
  geom_sf(data = state_overlay, fill = NA, color = "black", size = 0.1) +
  scale_fill_brewer(palette = "PuOr", direction = -1) +
  scale_color_brewer(palette = "PuOr", direction = -1, guide = FALSE) +
  coord_sf(datum = NA) +
  theme_minimal(base_family = "Roboto") +
  labs(title = "Net migration per 1000 residents by county",
       subtitle = "US Census Bureau 2022 Population Estimates",
       fill = "Rate",
       caption = "Data acquired with the R tidycensus package | @kyle_e_walker")








