library(gifski)
library(ggridges)
library(PerformanceAnalytics)
library(corrplot)
library(plotly)
library(ggpubr)
library(MASS)
library(reshape)
library(readxl)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(dplyr)
library(maps)
library(stringr)
library(tidyverse)
library(gganimate)
library(gifski)
library(png)
# 

population <- read_excel("melt.xls", skip = 2)
as.data.frame(population)

#Melting the dataframe population to get panel data of the years

population2<-reshape2::melt(population, id= c ("Country Name",  "Country Code", "Indicator Name" ,"Indicator Code"))

#Loading the second dataframe, which is our main dataframe: information was downloaded from the World Happiness Report, 2018

data<-read_excel("Chapter2OnlineData.xls")


#Loading the third dataframe: happy, that links countries with regions

happy <- read.csv("happy2015.csv")

# happy<-read_csv(url(urlfile))
# 
# setwd("C:/Users/user/Desktop/classes/sem4/reproducible_research/project")
# 
# 
# 
# #Loading the first dataframe: population from World Bankl 1960-2018
# 
# population <- read_excel("C:/Users/user/Desktop/classes/sem4/reproducible_research/project/melt.xls", skip = 2)
# as.data.frame(population)
# 
# #Melting the dataframe population to get panel data of the years
# 
# population2<-reshape2::melt(population, id= c ("Country Name",  "Country Code", "Indicator Name" ,"Indicator Code"))
# 
# #Loading the second dataframe, which is our main dataframe: information was downloaded from the World Happiness Report, 2018
# 
# data<-read_excel("C:/Users/user/Desktop/classes/sem4/reproducible_research/project/Chapter2OnlineData.xls")
# 
# 
# #Loading the third dataframe: happy, that links countries with regions
# happy <- read.csv("C:/Users/user/Desktop/classes/sem4/reproducible_research/project/happy2015.csv")


#Renaming the heterogeneous names in both dataframes and ordering by country name 

population2 <-population2[order(population2$`Country Name`),]
names(population2)[names(population2) == "Country Name"] <- "Country_name"
names(data)[names(data) == "Country name"] <- "Country_name"
names(population2)[names(population2) == "variable"] <- "Year"
names(population2)[names(population2) == "value"] <- "Population"
names(happy)[names(happy) == "Country"] <- "Country_name"
happy <-happy[order(happy$Country_name),]


#Finding the differences between data and population dataframes: the following command shows all the names existing in the first dataframe, but not in the second. 

setdiff(data$Country_name, population2$Country_name)

#Renamed the countries for population2 

population2$Country_name<-recode(population2$Country_name, 
                                 "Russian Federation"= "Russia",
                                 "Iran, Islamic Rep."= "Iran",
                                 "Egypt, Arab Rep."= "Egypt",
                                 "Gambia, The" = "Gambia",
                                 "Kyrgyz Republic" = "Kyrgyzstan",
                                 "Lao PDR" = "Laos",
                                 "Slovak Republic" = "Slovakia",
                                 "Syrian Arab Republic" = "Syria",
                                 "Yemen, Rep."= "Yemen",
                                 "Venezuela, RB" = "Venezuela",
                                 "Cote d'Ivoire"= "Ivory Coast",
                                 "Hong Kong SAR, China" = "Hong Kong S.A.R. of China",
                                 "Congo, Rep."  =  "Congo (Brazzaville)",
                                 "Congo, Dem. Rep."=  "Congo (Kinshasa)" ,
                                 "Korea, Rep." = "South Korea",
                                 "North Macedonia" = "Macedonia",
                                 "Cyprus"= "North Cyprus", 
                                 "Somalia" = "Somaliland region")




#Merge the main dataframe and population2 by country name

total <- merge(data, population2, by=c("Country_name", 'Year'))

#Delete the columns that we do not need using select function

total=subset(total,select=-c(17,20:26,28,29))

setdiff(total$Country_name, happy$Country_name)

happy$Country_name<-recode(happy$Country_name, 
                           "Hong Kong"= "Hong Kong S.A.R. of China",
                           "Sudan" ="South Sudan")

#Selecting only regions from "happy" dataframe
regions<-happy[0:2]


#Merge total and regions dataframes by country name

total <- merge(total, regions, by=c("Country_name"))



#Uploading the world map
world <- map_data("world")

#region is the country name, so lets rename it
names(world)[names(world) == "region"] <- "Country_name"


#Finding the differences between total and world:

setdiff(total$Country_name, world$Country_name)

#And changing in world


world$Country_name<-recode(world$Country_name, 
                           "USA" = "United States",
                           "UK"= "United Kingdom",
                           "Cyprus" ="North Cyprus", 
                           "Republic of Congo" = "Congo (Brazzaville)",
                           "Democratic Republic of the Congo"  = "Congo (Kinshasa)",
                           "Hong Kong" = "Hong Kong S.A.R. of China",
                           "Somalia"= "Somaliland region" ,
                           "Trinidad" = "Trinidad and Tobago")

#Visualizing the happiest regions in the complete time scope



table_1<-total %>% group_by(Region) %>% 
  summarise(Mean.response = mean(`Life Ladder`))

as.data.frame.matrix(table_1) 

#From the dataframe "total" we take only life ladder, country name of the year 2018 and creating a new data frame happy_2017 and then join it with the "world" dataframe


happy_2017<-subset(total, total$Year==2017)


happy_2017 <- happy_2017 %>%
  dplyr::select("Country_name", "Life Ladder", "Country Code", "Region", "Year", "Population")



happySubset_17 <- inner_join(world, happy_2017, by = "Country_name")

# Creating the mean coordinates to get one coordinate of latitude and longitude per country

test <- 
  happySubset_17 %>%
  group_by(Country_name) %>%
  summarise(mean_long = mean(long), mean_lat = mean(lat))  



happySubset_17test <- merge(happySubset_17, test, by = "Country_name")

# Plotting

plain <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank(),
  panel.background = element_rect(fill = "white"),
  plot.title = element_text(hjust = 0.5)
)


missing_countries<-world %>% filter(world$Country_name== c('Sudan', 'Angola', 'Namibia' ,'Cuba', 'Taiwan', 'Eritrea', 'Gambia', 'Guam', 'Malaysia', 'Oman', 'Qatar', 'Swaziland', 'Syria'))

all_countries <- bind_rows(happySubset_17,missing_countries)

ggplot(data = all_countries, mapping = aes(x = long, y = lat, group = group)) + 
  geom_polygon(aes(fill = `Life Ladder`), colour = "black") +
  scale_fill_distiller(palette ="YlOrRd", direction = -1) + 
  ggtitle("Life Ladder Indext (LLI) for 2017") +
  plain +
  geom_text(data= happySubset_17test,aes(x= mean_long, y= mean_lat, label= Country_name),
            color = "black", check_overlap = TRUE, size = 3, hjust = 0) 

head(all_countries)
summary(all_countries)
all_countries$Region

ch = unique(all_countries$Region)
ch1 = unique(all_countries$Country_name)
ch2 = unique(total$Year)
ch3 = colnames(total)
ch3
ch4 = unique(total$Year)

ch<-unique(regions$Region)

#total[ch3[1]]
#total$Year
#ggplot(data = all_countries %>% filter(Region %in% "Western Europe"), mapping = aes(x = long, y = lat, group = group)) + 
 # geom_polygon(aes(fill = `Life Ladder`), colour = "black") +
  #scale_fill_distiller(palette ="YlOrRd", direction = -1) + 
  #ggtitle("Life Ladder Indext (LLI) for 2017") +
  #plain +
  #geom_text(data= happySubset_17test %>% filter(Region %in% "Western Europe"),aes(x= mean_long, y= mean_lat, label= Country_name),
   #         color = "black", check_overlap = TRUE, size = 3, hjust = 0) 


