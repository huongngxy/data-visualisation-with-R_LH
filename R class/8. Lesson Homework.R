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

Maximum <- vector("numeric",length(years))
for (a in 1: length(years)) {
  (Maximum[a] <- max(gapminder$lifeExp[gapminder$year == years[a]]))
}

Minimum <- vector("numeric",length(years))
for(l in 1: length(years)) {
  (Minimum[l] <- min(gapminder$lifeExp[gapminder$year == years[l]]))
} 

Median <- vector("numeric", length(years))
for(b in 1: length(years)) {
  (Median[b] <- median(gapminder$lifeExp[gapminder$year == years[b]]))
}

China <- gapminder %>%
  filter(country %in% c("China")) %>%
  select(lifeExp) %>%
  rename(China = lifeExp)

semifinal <- data.frame(years, Maximum, Minimum, Median, China)
final <- semifinal %>%
  select(years, Maximum, Minimum, Median, China) %>%
  gather(key = "Category", value = "Life_expectancy", -years) %>%
  mutate(Life_expectancy = as.numeric(Life_expectancy)) %>%
  group_by(Category)
  
plot <- ggplot(final, aes(x = years, 
                          y = Life_expectancy,
                          group = Category, 
                          color = Category)) + 
  geom_line(aes(linetype = Category), size = 1) +
  theme_bw() + 
  scale_color_manual(values = cols) + 
  theme(legend.position = "bottom", legend.title = element_blank()) +
  scale_linetype_manual(values = c("China" = "solid", "Minimum" = "dashed","Maximum" = "dashed","Median" = "dashed"))+
  labs(y = "Life expectancy at birth", 
       x = "Year",
       caption = "Source: Gapminder")
plot




