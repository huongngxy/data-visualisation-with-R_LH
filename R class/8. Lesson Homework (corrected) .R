rm(list=ls())

library(ggplot2)
library(dplyr)
library(tidyverse)
library(gapminder)

years <- unique(gapminder$year) 
cols <- c("Median" = "steelblue",
          "Minimum" = "darkorange",
          "Maximum" = "darkred",
          "China" = "black")
View(gapminder)

Maximum <- gapminder %>%
  group_by(year) %>%
  summarise_at(vars(lifeExp),
               list(Maximum = max)) %>%
  select(Maximum)

Minimum <- gapminder %>%
  group_by(year) %>%
  summarise_at(vars(lifeExp),
               list(Minimum = min)) %>% select(Minimum)

Median <- gapminder %>%
  group_by(year) %>%
  summarise_at(vars(lifeExp),
               list(Median = median)) %>% select(Median)

China <- gapminder %>%
  filter(country %in% c("China")) %>%
  select(lifeExp) %>%
  rename(China = lifeExp)

semifinal <- data.frame (years,Maximum, Minimum, Median, China)
final <- semifinal %>% pivot_longer(cols = c('Minimum', 'Maximum', 'Median', 'China'), names_to = 'Category', values_to = 'lifeExp')

plot <- ggplot(final, aes(x = years, 
                          y = lifeExp,
                          group = Category, 
                          color = Category)) + 
  geom_line(aes(linetype = Category), size = 1)+
  scale_color_manual(values = cols) + 
  theme_bw() +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  scale_linetype_manual(values = c("China" = "solid", "Minimum" = "dashed","Maximum" = "dashed","Median" = "dashed"))+
  labs(y = "Life expectancy at birth", 
       x = "Year",
       caption = "Source: Gapminder")
plot

