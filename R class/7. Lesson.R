#1----
rm(list=ls())

library(here)
library(tidyr)
library(dplyr) 
library(ggplot2)
library(rdbnomics)

cols <- c("Germany" ="blue",
"Spain"= "red",
"France" = "green",
"United States" = "orange",
"Italy" = "black",
"Netherlandds" ="purple")
source("R class/functions.r")
 
country_code <- c("D_W", "DEU", "ESP", "FRA","ITA", "NLD", "USA")
countries <- paste0("AMECO/OVGD/", country_code, ".1.1.0.0.OVGD")
df_rgdp <- rdb(ids = countries) %>%
  filter_function() %>%
  germany_function()
 
df_unemp <- readRDS("6.Lesson_df_final.rds")
df_final <- df_rgdp %>%
  inner_join(df_unemp, by = c("Country","Year")) %>%
  rename(rgdp = value.x,
         unemp = value.y) %>%
  relocate(Year, Country, rgdp, unemp) %>%
  group_by(Country) %>%
  mutate(dunemp = c(NA,diff(unemp)),#different between unemployment rate this year and last year
         rgdp_gr = c(NA, diff(rgdp))/lag(rgdp)*100, digits = 2) %>% #"lag" refers to the previous year
  drop_na() #cancel out NA in the table 
df_final_modified <- df_final %>%
  filter(!(Year %in% c(1991,2020:2022)))
p1 <- ggplot(df_final_modified, aes(x= rgdp_gr,
                                    y = dunemp,
                                    group= Country,
                                    color = Country)) +
  geom_point() +
  theme_bw() +
  geom_smooth(method = lm,
              se = FALSE,
              fullrange = TRUE) +
  scale_color_manual(values = cols) + 
  theme(legend.position = "bottom") +
  labs(color = NULL,
       y = "Diff, unemployment rate, \n percentage points",
       x = "Growth rate real gdp, \n percent",
       title = paste0("Okun´s Law,", min(df_final_modified$Year), "-", max(df_final_modified$Year)),
       caption = "Source; AMECO data from DBnomics")
p1


#2----

rm(list=ls())

library(here)
library(tidyr)
library(dplyr) 
library(ggplot2) 
library(rdbnomics)

cols <- c("Germany" ="blue",
          "Spain"= "red",
          "France" = "green",
          "United States" = "orange",
          "Italy" = "black",
          "Netherlandds" ="purple")
source("R class/functions.r")

country_code <- c("D_W", "DEU", "ESP", "FRA","ITA", "NLD", "USA")
countries <- paste0("AMECO/OVGD/", country_code, ".1.1.0.0.OVGD")
df_rgdp <- rdb(ids = countries) %>%
  filter_function() %>%
  germany_function()

df_unemp <- readRDS("6.Lesson_df_final.rds")
df_final <- df_rgdp %>%
  inner_join(df_unemp, by = c("Country","Year")) %>%
  rename(rgdp = value.x,
         unemp = value.y) %>%
  relocate(Year, Country, rgdp, unemp) %>%
  group_by(Country) %>%
  mutate(dunemp = c(NA,diff(unemp)),#different between unemployment rate this year and last year
         rgdp_gr = c(NA, diff(rgdp))/lag(rgdp)*100, digits = 2) %>% #"lag" refers to the previous year
  drop_na() #cancel out NA in the table 
df_final_modified <- df_final %>%
  filter(!(Year %in% c(1991,2020:2022)))
p2 <- ggplot(df_final_modified, aes(x= rgdp_gr,
                                    y = dunemp,
                                    group= Country,
                                    color = Country)) +
  geom_point() +
  theme_bw() +
  geom_smooth(method = lm,
              se = FALSE,
              fullrange = TRUE) +
  scale_color_manual(values = cols) + 
  theme(strip.background = element_blank(),
              legend.position = "bottom") +
  facet_wrap(~ Country, scales = "free") +
  labs(color = NULL,
       y = "Diff, unemployment rate, \n percentage points",
       x = "Growth rate real gdp, \n percent",
       title = paste0("Okun´s Law,", min(df_final_modified$Year), "-", max(df_final_modified$Year)),
       caption = "Source; AMECO data from DBnomics")
p2
