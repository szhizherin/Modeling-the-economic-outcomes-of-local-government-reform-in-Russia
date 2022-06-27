# Purpose: create big_cities_with_openness table for further analysis
# Inputs:  final_data/big_cities.csv
#          information_openness/openness.xlsx
#          information_openness/matches.xlsx
# Outputs: final_data/big_cities_with_openness.csv


library(dplyr)
library(readxl)
library(readr)




openness <- read_excel("information_openness/openness.xlsx", 
                       col_types = c("skip", "text", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "text"))

matches <- read_excel("information_openness/matches.xlsx", 
                      col_types = c("skip", "text", "text", 
                                    "text", "text", "text", "numeric", 
                                    "numeric"))

openness_oktmo <- openness %>% 
  left_join(matches, by = c("municipality" = "mun_original", 
                            "region" = "reg_original"),
            suffix = c("_original", "_matched")) %>% 
  select(!c("results_count", "region", "municipality", "mun_type"))


big_cities <- read_csv("final_data/big_cities.csv") %>% select(-c(1))


big_cities_with_openness <- openness_oktmo %>% 
  inner_join(big_cities, by = c("oktmo" = "oktmo", 
                               "year" = "year"))

# first year of treatment for each municipality
big_cities_with_openness$first.treat = big_cities_with_openness$treatment * big_cities_with_openness$year

big_cities_with_openness <- big_cities_with_openness %>% 
  mutate(first.treat = case_when(first.treat == 0 ~ Inf,
                                 first.treat != 0 ~ first.treat))

big_cities_with_openness <- big_cities_with_openness %>% 
  group_by(oktmo) %>% 
  mutate(first.treat = min(first.treat, na.rm = T)) %>% 
  ungroup()

big_cities_with_openness <- big_cities_with_openness %>% 
  mutate(first.treat = case_when(first.treat == Inf ~ 0,
                                 first.treat != Inf ~ first.treat))

big_cities$oktmo %>% unique() %>% length()
big_cities_with_openness$oktmo %>% unique() %>% length()

big_cities_with_openness %>% write.csv("final_data/big_cities_with_openness.csv", 
                                       fileEncoding = "UTF-8")

