library(tidyverse)
library (ggplot2)
library(dplyr)
library(maps)
library(usmap)


incarceration_trends<- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

# what is the mean value of my variable across all the counties in 2018
mean_latinx_jp<- incarceration_trends %>% filter (year == 2018) %>% select(latinx_jail_pop, county_name) %>%
summarise(latinx_jail_pop = mean(latinx_jail_pop, na.rm = T)) %>%
  pull(latinx_jail_pop)
  

#where is my variable the highest
latinx_jp_max<- incarceration_trends %>% select (year, latinx_jail_pop, state) %>%
  group_by(state) %>%
summarise(latinx_jail_pop = max(latinx_jail_pop, na.rm = T)) %>%
  filter(latinx_jail_pop == max(latinx_jail_pop, na.rm = T)) %>%
  pull(state)

#What is the latinx jail pop in Autauga county in the year 2000
auatauga_latinx<- incarceration_trends %>%  filter (year == 2000) %>% 
  filter(county_name == "Autauga County") %>%
pull(latinx_jail_pop)

# What is the county with the highest Latinx Jail Pop in Washington state in 2018
wa_highest<- incarceration_trends %>% filter (year == 2018) %>% filter(state == "WA") %>%
  select(latinx_jail_pop, county_name) %>%
  filter(latinx_jail_pop == max(latinx_jail_pop, na.rm = T)) %>%
pull(county_name)

# In Yakima County in 2018 what was the latinx jail pop?
yakima_max<- incarceration_trends %>% filter (year == 2018) %>% filter(county_name == "Yakima County") %>%
  filter(latinx_jail_pop == max(latinx_jail_pop, na.rm = T)) %>%
  pull(latinx_jail_pop)


latinx_country<- incarceration_trends %>% select(year, latinx_jail_pop) %>%
group_by(year) %>% 
  summarise(latinx_jail_pop = sum(latinx_jail_pop, na.rm= T))
country_map<- ggplot(latinx_country, aes(x = year, y= latinx_jail_pop)) + geom_bar(stat = "identity") +
  ggtitle("Latinx Jail Population From 1970 to 2017")

  
black_jail_pop<- incarceration_trends %>% select(year, black_jail_pop) %>%
  group_by(year) %>% 
  summarise(black_jail_pop = sum(black_jail_pop, na.rm= T))

latinx_plus_black<- left_join(latinx_country, black_jail_pop)


black_latinx<- ggplot(latinx_plus_black, aes(x = year)) + geom_line(aes(y = latinx_jail_pop )) +
  geom_line(aes(y = black_jail_pop)) + ggtitle("Black and Latinx Jail Pop From 1970 to 2018")



all_maps<- incarceration_trends %>% filter (year == 2018) %>% select(latinx_jail_pop, state) %>%
  group_by(state) %>%
  summarise(latinx_jail_pop = mean(latinx_jail_pop, na.rm = T))

country_map<- plot_usmap(data = all_maps, values = "latinx_jail_pop") +
  scale_fill_continuous(low = "white", high = "blue", name = "state") +
ggtitle ("Mean Latinx Jail Pop Across the U.S")
  
