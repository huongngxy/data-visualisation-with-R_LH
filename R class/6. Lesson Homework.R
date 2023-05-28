rm(list =ls())
library(ggplot2)
library(dplyr)
library(rdbnomics) 

ctrycode <- c("FRA","DEU","D_W","ITA","NLD","ESP","USA")
countries <- paste0("AMECO/ZUTN/", ctrycode, ".1.0.0.0.ZUTN")
df <- rdb(ids = countries) %>%
  select(Country, value, original_period) %>%
  filter(original_period <= 2022) %>%
  rename(Year = original_period) %>%
  mutate(Year = as.numeric(Year))
df_W_D <- df %>%
  filter(Country == "West Germany" & Year < 1991)
df_DE <- df %>%
  filter(Country == "Germany" & Year >= 1991)
df_DE1 <- df_W_D%>%
  bind_rows(df_DE) %>%
  mutate(Country = recode(Country, 'West Germany' = "Germany"))
df_new <- df %>%
  filter(!(Country %in% c("West Germany","Germany"))) %>%
  bind_rows(df_DE1)
View(df_new)
cols <- c("France" = "blue",
          "Germany" = "red",
          "Italy" = "black",
          "Netherlands" = "violet",
          "Spain" = "orange",
          "United States" = "green")
pl <- ggplot(df_new, aes(x = Year,
                         y = value,
                         group = Country,
                         color = Country)) +
  scale_color_manual(values = cols) + 
  geom_line() +
  theme_bw() +
  labs(y = "Percent") +
  facet_wrap(~ df_new$Country, scales = "free") +
  theme(strip.background = element_blank(),
    axis.text.x = element_text(angle = 45,
                               hjust = 1)) + 
  scale_x_continuous(breaks = seq(min(df_new$Year), max(df_new$Year),5))
pl


  