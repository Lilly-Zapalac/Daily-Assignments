# Daily-Assignments
Day-08

#Lilly Zapalac, 02/22/25, making a faceted plot to covid cases and deaths

library(tidyverse)
library(dplyr)
library(ggplot2)
url <- 'https://www2.census.gov/programs-surveys/popest/datasets/2020-2024/state/totals/NST-EST2024-ALLDATA.csv'

read_csv(url)

covid_data <- read.csv(url)

covid <- covid_data_region %>%

filter(region %in% 
         c("Northest", "South", "North Central", "West")) %>%
  select(region, fips, deaths, cases) %>%
  pivot_longer(cols = c("deaths", "cases")) %>%
  
ggplot(aes(x = date, y = count)) +
  geom_line(col = "gray80") +
  geom_point(aes(col = region)) +
  facet_grid(name~region, scales = "free_y") +
  labs(x = "date", y = "cases",
       title = "Cumulative Cases and Deaths: Region",
       subtitle = "Covid-19 Data: NY-Times",
       caption = "Daily Exercise 8") +
  theme_linedraw() +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 90, face = "bold")) +
  theme(plot.subtitle = element_text(color = "navy", face = "bold")) +
  theme(plot.caption = element_text(color = "gray50", face = "italic"))
       
print(plot)

