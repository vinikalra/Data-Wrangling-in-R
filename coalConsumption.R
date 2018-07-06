#Coal Consumption Case Study

#Load Tidyverse package
library(tidyverse)

#Import coal consumption data set from the URL
coal <- read_csv('http://594442.youcanlearnit.net/coal.csv')

glimpse(coal)

# Ignore First 2 rows
coal <- read_csv('http://594442.youcanlearnit.net/coal.csv', skip = 2)

#Rename col X1 to region
colnames(coal)[1] <- 'region'

#Converting wide data to long data using the gather function
coal_long <- gather(coal, 'year', 'coal_consumption', -region)
glimpse(coal_long)

#Observing the data types of columns
summary(coal)

#Convert year from character to integer
coal_long$year <- as.integer(coal_long$year)
summary(coal_long)

#Convert coal_consumption from character to numeric
coal_long$coal_consumption <- as.numeric(coal_long$coal_consumption)
summary(coal_long)

#######################
#Segmenting the Dataset
#######################

unique(coal_long$region)
non_countries <- c("North America", "Central & South America", "Antarctica", "Europe", "Eurasia", "Middle East", "Africa", "Asia & Oceania", "World")

#Extract all the row numbers that have non-countries in them
matches <- which(!is.na(match(coal_long$region, non_countries)))

#Create a tibble of country values
coal_country <- coal_long[-matches,]

#Create a tibble of region values
coal_region <- coal_long[matches,]

unique(coal_region$region)

#########################
#Visulaizing the Dataset
#########################

ggplot(data=coal_region, mapping = aes(x=year, y=coal_consumption)) + 
  geom_line(mapping = aes(color=region))
