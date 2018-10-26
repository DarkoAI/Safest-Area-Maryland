#################################################
### Final project for DSS 665 - R Programming ###
### Student: Darko Stankovic ####################
#################################################


#######################
## Loading libraries ##
#######################

library(tidyverse)
library(ggmap)
library(caTools)
library(ggthemes)

#################
##Loading data ##
#################

crime.data <- read.csv(file.choose(), header = T)
head(crime.data)

###########################################
## Type of a data, variables and dataset ##
###########################################

glimpse(crime.data)

#################################################################
## Selecting variable that are going to be used in the project ##
#################################################################

crime.project <- crime.data[c(4, 6, 8, 10, 14,23,24)]

head(crime.project)
glimpse(crime.project)


##############################################
## Montgomary county Latitude and Longitude ##
##############################################

geocode("Montgomary County")

###        lon      lat
###    -77.24052 39.15474

##############################
## Mapping Montgomary County ##
##############################

mapMontCount <- get_map(location = c(-77.24052, 39.15474), source = "google", zoom = 10, maptype = "roadmap")
ggmap(mapMontCount)

##############################################
## Type of crimes in Montgomary County area ##
##############################################

table(crime.project $'Crime.Name2')

#####################################
## Sorting crimes highest to lowest##
#####################################

sort(table(crime.project$Crime.Name2), decreasing = T)


ggmap(mapMontCount) + geom_point(aes(x = Longitude, y = Latitude, colour = `Crime.Name2`),
                                 size = 1, data = crime.project)

######################################
## Filtering crimes for the project ##
######################################

filt.crime.project <- filter(crime.project,
                             Crime.Name2 == "Drug/Narcotic Violations" |
                                     Crime.Name2 == "Destruction/Damage/Vandalism of Property" |
                                     Crime.Name2 == "Burglary/Breaking and Entering" |
                                     Crime.Name2 == "Robbery")

head(filt.crime.project)
glimpse(filt.crime.project)

#########################
## Table filtered data ##
#########################

table(filt.crime.project$ 'Crime.Name2')
sort(table(filt.crime.project$Crime.Name2), decreasing = T)


#######################################################
### Mapping filtered crime data for Montgomary County ##
#######################################################

ggmap(mapMontCount) + geom_point(aes(x = Longitude, y = Latitude, colour = `Crime.Name2`),
                                 size = 1, data = filt.crime.project)

########################################
### Table filtered data for each city ##
########################################

table(crime.project $'Police.Distric.Name')
sort(table(filt.crime.project$Police.District.Name), decreasing = T)

#########################################
### Plots for crime, city and zip code ##
#########################################

crime.frequency <-  filt.crime.project [c(3,4,5)]  %>% 
        gather("var", "value") %>% 
        group_by(var) %>% 
        count(var, value) %>%
        mutate(prop = prop.table(n)) %>% 
        filter(prop > .002)

crime.plot <-
        ggplot(data = crime.frequency,
               aes(x = reorder(stringr::str_wrap(value, 10), prop),
                   y = prop)) +
        geom_bar(stat = "identity", fill = "lightblue") +
        coord_flip() +
        facet_wrap(~var, ncol = 3, scales = "free") +
        ggthemes::theme_fivethirtyeight()

crime.plot

###################################
### Maping data for Takoma Park ###
###################################

geocode("Takoma Park")
#      lon      lat
#    -77.00748, 38.97789

mapTakoma <- get_map(location = c(-77.00748, 38.97789), source = "google", zoom = 15, maptype = "roadmap")
ggmap(mapTakoma)

ggmap(mapTakoma) + geom_point(aes(x = Longitude, y = Latitude, colour = `Crime.Name2`),
                              size = 2, data = filt.crime.project)


#################################################
### Back to the original Montgomary county map ##
#################################################

ggmap(mapMontCount) + geom_point(aes(x = Longitude, y = Latitude, colour = `Crime.Name2`),
                                 size = 1, data = filt.crime.project)

####################################################################
### Zooming map to have better look to the middle part of the map ##
####################################################################

mapMontCount <- get_map(location = c(-77.24052, 39.15474), source = "google", zoom = 13, maptype = "roadmap")
ggmap(mapMontCount)

ggmap(mapMontCount) + geom_point(aes(x = Longitude, y = Latitude, colour = `Crime.Name2`),
                                 size = 1.5, data = filt.crime.project)

#######################################
### Checking the density of the data ##
#######################################

ggmap(mapMontCount) + geom_density2d (aes(x = Longitude, y = Latitude, colour = `Crime.Name2`),
                                      size = 1, data = filt.crime.project)

###################################
### Spliting map for every crime ##
###################################

ggmap(mapMontCount) + geom_point(aes(x = Longitude, y = Latitude, colour = `Crime.Name2`),
                                 size = 1, data = filt.crime.project) + 
        facet_wrap(~ `Crime.Name2`)

######################################
### Cheking density for every crime ##
######################################

ggmap(mapMontCount) + geom_density2d (aes(x = Longitude, y = Latitude, colour = `Crime.Name2`),
                                      size = 1, data = filt.crime.project)+
        facet_wrap(~ `Crime.Name2`)

#############################
### Choosing the Kentlands ##
#############################

geocode("Kentlands")
#   lon      lat
#  -77.24339 39.12178

mapKentlands <- get_map(location = c(-77.24339, 39.12178), source = "google", zoom = 15, maptype = "roadmap")
ggmap(mapKentlands)

#########################################
### Maping the crimes in the Kentlands ##
#########################################

ggmap(mapKentlands) + geom_point(aes(x = Longitude, y = Latitude, colour = `Crime.Name2`),
                                 size = 1.5, data = filt.crime.project)

#######################################################
### Spliting the Kentlands map for different cirmes ###
#######################################################

ggmap(mapKentlands) + geom_point(aes(x = Longitude, y = Latitude, colour = `Crime.Name2`),
                                 size = 2, data = filt.crime.project)+
        facet_wrap(~ `Crime.Name2`)

### End of the project ###


