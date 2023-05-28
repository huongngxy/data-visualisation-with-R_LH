rm(list = ls())

library(ggplot2)
library(dplyr)
library(rdbnomics)

# 1 single plot for one country ---- 
df_usa <- rdb(ids = "AMECO/ZUTN/USA.1.0.0.0.ZUTN") %>%
select(Country, value, original_period) %>%
filter(original_period <= 2022) %>%
rename(Year = original_period) %>%
mutate(Year = as.numeric(Year))

#saveRDS(df_usa,file = "6.Lesson_df_usa.rds")
readRDS(file = "6.Lesson_df_usa.rds") 

df_usa <- readRDS(file = "6.Lesson_df_usa.rds")
pl <- ggplot(df_usa, aes(x = Year,
                         y = value,
                         group = Country,
                         color = Country)) +
  geom_line() +
  theme_bw() +
  labs(y = "Percent",
       title = paste0("Unemployment Rate", min(df_usa$Year), "-", max(df_usa$Year)),
       subtitle = "Definition EUROSTAT (ZUTN)",
       caption = "Source: AMECO data from dbnomics.") +

  scale_x_continuous(breaks = seq(min(df_usa$Year), max(df_usa$Year),5)) +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1))
pl

# 2 multiple countries in one single plot ----

country_code <- c("D_W", "DEU", "ESP", "FRA","ITA", "NLD", "USA")
countries <- paste0("AMECO/ZUTN/", country_code, ".1.0.0.0.ZUTN")
df <- rdb(ids = countries) %>%
  select(Country, value, original_period) %>%
  filter(original_period <= 2022) %>%
  rename(Year = original_period) %>%
  mutate(Year = as.numeric(Year)) 
df_west_germany <- df  %>%
  filter(Country == "West Germany" & Year < 1991 )
df_germany <- df %>%
  filter(Country == "Germany" & Year >= 1991)
df_germany_together <- df_west_germany %>%
  bind_rows(df_germany) %>%
  mutate(Country = recode(Country, 'West Germany' = "Germany" )) 

df_final <- df %>%
  filter(!(Country %in% c("West Germany", "Germany"))) %>%
  bind_rows(df_germany_together)
cols <- c("Germany" ="blue",
          "Spain"= "red",
          "France" = "green",
          "United States" = "orange",
          "Italy" = "black",
          "Netherlandds" ="purple")
p2 <- ggplot(df_final, aes(x = Year,
                           y = value, 
                           group = Country,
                           color = Country)) +  
  scale_color_manual(values = cols) + 
  geom_line() +
  theme_bw() +
  labs(y = "Percent",
       title = paste0("Unemployment rate", min(df_final$Year), "-", max(df_final$Year)),
       subtitle = "Definition EUROSTAT (ZUTN)",
       caption = "Source: AMECO data from dbnomics.") +
 theme(axis.text.x = element_text(angle = 45,
                                  hjust = 1)) + 
  scale_x_continuous(breaks = seq(min(df_final$Year), max(df_final$Year),5))
p2


