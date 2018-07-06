#Austin Water Quality Case Study

#Load Packages
library(tidyverse)
library(lubridate)
library(stringr)


#Read in the dataset 
water <- read_csv('http://594442.youcanlearnit.net/austinwater.csv')

#Take a look
glimpse(water)

#Overwriting water tibble with only columns required
water <- tibble('siteName' = water$SITE_NAME,
                'siteType' = water$SITE_TYPE,
                'sampleTime' = water$SAMPLE_DATE,
                'parameterType' = water$PARAM_TYPE,
                'parameter' = water$PARAMETER,
                'result' = water$RESULT,
                'unit' = water$UNIT)

glimpse(water)
# Too many rows

############################
#Filtering the Dataset
############################

#Taking a look at parameter column to see values of requirement
unique(water$parameter)

# We can see Alkalinity/Hardness/pH and Conventional are values of our interest
unique(water$parameterType)

#Subset water table, keep only rows where parameterType is Alkalinity/Hardness/pH or Conventional
filtered_water <- subset(water, (parameterType == 'Alkalinity/Hardness/pH') | parameterType == 'Conventionals') 
glimpse(filtered_water)

unique(filtered_water$parameter)

#Subsetting filteres_water table with only parameter values of PH and WATER TEMPERATURE
filtered_water <- subset(filtered_water, parameter == 'PH' | parameter == 'WATER TEMPERATURE')
glimpse(filtered_water)


summary(filtered_water)
##################################
# Correcting the Data Types
##################################

#Convert character types to Factors (Categorical variables)
filtered_water$siteType <- as.factor(filtered_water$siteType)
filtered_water$parameterType <- as.factor(filtered_water$parameterType)
filtered_water$parameter <- as.factor(filtered_water$parameter)
filtered_water$unit <- as.factor(filtered_water$unit)

#filtered_water$sampleTime
#Convert Sample Time to a Date Time Object
filtered_water$sampleTime <- mdy_hms(filtered_water$sampleTime)


#################################
#Correcting the data-entry Errors
#################################


summary(filtered_water)
#Checking the unit column, Has two strange unit measurements

subset(filtered_water, unit=='Feet')
convert <- which(filtered_water$unit=='Feet')
filtered_water$unit[convert] <- 'Deg. Fahrenheit'


glimpse(subset(filtered_water, unit=='MG/L' & parameter == 'PH'))
convert  <-  which(filtered_water$unit=='MG/L' & filtered_water$parameter == 'PH')
convert
filtered_water$unit[convert] <- 'Standard units'

glimpse(subset(filtered_water, unit=='MG/L'))
convert  <-  which(filtered_water$unit=='MG/L' & filtered_water$result > 70)
convert
filtered_water$unit[convert] <- 'Deg. Fahrenheit'

glimpse(subset(filtered_water, unit=='MG/L'))
convert <- which(filtered_water$unit=='MG/L')
filtered_water$unit[convert] <- 'Deg. Celsius'


summary(filtered_water)

######################################
#Identifying and removing the Outliers
######################################

ggplot(data = filtered_water, mapping = aes(x= sampleTime, y = result)) +
  geom_point()

glimpse(subset(filtered_water, result > 1000000))

remove <- which(filtered_water$result > 1000000 | is.na(filtered_water$result))

filtered_water <- filtered_water[-remove,]

summary(filtered_water)

glimpse(subset(filtered_water, result > 1000))

remove <- which(filtered_water$result > 1000)
filtered_water <- filtered_water[-remove,]

summary(filtered_water)

ggplot(data= filtered_water, mapping = aes(x=unit, y = result)) +
  geom_boxplot()

convert <- which(filtered_water$result > 60 & filtered_water$unit == 'Deg. Celsius')
filtered_water$unit[convert] <- 'Deg. Fahrenheit'


##################################################
#Converting temperature from Fahrenheit to Celsius
##################################################

fahrenheit <- which(filtered_water$unit == 'Deg. Fahrenheit')

filtered_water$result[fahrenheit] <- (filtered_water$result[fahrenheit] - 32) *(5.0/9.0)

filtered_water$unit[fahrenheit] <- 'Deg. Celsius'

summary(filtered_water)

#Removing the empty factor levels
filtered_water$unit <- droplevels(filtered_water$unit)



#############################################
#Widening the dataset
#############################################

filtered_water <- filtered_water[, -c(4,7)]
summary(filtered_water)


filtered_water_wide <- spread(filtered_water, parameter, result)
##error duplicate identifiers

dup_check <- filtered_water[,-5]
duplicates <- which(duplicated(dup_check))

#Removing the Duplicates
filtered_water <- filtered_water[-duplicates,]

filtered_water_wide <- spread(filtered_water, parameter, result)
glimpse(filtered_water_wide)
colnames(filtered_water_wide)[4] <- 'pH'
colnames(filtered_water_wide)[5] <- 'temperature'
