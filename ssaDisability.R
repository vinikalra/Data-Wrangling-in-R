#Social Security Disability Case Study

#####################
#Reading the dataset
#####################

library(tidyverse)
library(lubridate)
library(stringr)

ssa <- read_csv('http://594442.youcanlearnit.net/ssadisability.csv')

glimpse(ssa)

########################
#Making the dataset Long
########################

ssa_long <- gather(ssa, month, applications, -Fiscal_Year)

ssa_long

#########################
# Formatting the Dates
#########################

unique(ssa_long$month)

#Splitting the Column into 2 different variables
ssa_long <- separate(ssa_long, month, c("month", "application_method"), sep ='_')

ssa_long

unique(ssa_long$month)
#Abbreviating the month Column
ssa_long$month <- substr(ssa_long$month, 1,3)

#Formatting Fiscal Year column from FYXX format to 20XX format
unique(ssa_long$Fiscal_Year)
ssa_long$Fiscal_Year <- str_replace(ssa_long$Fiscal_Year, 'FY', '20')

#FOrmatting the Date from month and Fiscal Year column
ssa_long$Date <- dmy(paste('01', ssa_long$month, ssa_long$Fiscal_Year))
unique(ssa_long$Date)


###############################
# Handling the Fiscal year
###############################

advanced_dates <- which(month(ssa_long$Date)>=10)
year(ssa_long$Date[advanced_dates])<- year(ssa_long$Date[advanced_dates]) - 1



##################################
#Widening the Dataset
##################################

#Remove unwanted columns
ssa_long$Fiscal_Year <- NULL
ssa_long$month <- NULL

#Convert Application method column from char to factor
ssa_long$application_method <- as.factor(ssa_long$application_method)

#Widen the final Dataset
ssa_wide <- spread(ssa_long, application_method, applications)

ssa_wide


##############################
#Visualizing the Dataset
##############################


ssa_wide$online_percentage <- ssa_wide$Internet/ ssa_wide$Total * 100

ggplot(data = ssa_wide, mapping = aes(x= Date, y= online_percentage)) + geom_line()
