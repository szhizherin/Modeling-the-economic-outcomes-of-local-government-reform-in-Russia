# Purpose: create municipalities table for further analysis
# Inputs:  intermediate_data/BDMO_panel.csv
#          intermediate_data/goods_and_services_prices.csv
#          intermediate_data/CPI.csv
# Outputs: final_data/municipalities.csv




library(dplyr)
library(readr)




BDMO_panel <- read_csv("intermediate_data/BDMO_panel.csv") %>% 
  select(-c(1)) %>% 
  mutate(model = recode(model, `Сити-менджер` = "Сити-менеджер"))


# create treatment variables
municipalities <- BDMO_panel %>% 
  mutate(treatment = case_when(model == "Избираемый мэр" ~ 0,
                               model == "Сити-менеджер" ~ 1,
                               model == "Назначаемый мэр" ~ 1))


# first year of treatment for each municipality
municipalities$first.treat = municipalities$treatment * municipalities$year

municipalities <- municipalities %>% 
  mutate(first.treat = case_when(first.treat == 0 ~ Inf,
                                 first.treat != 0 ~ first.treat))

municipalities <- municipalities %>% 
  group_by(oktmo) %>% 
  mutate(first.treat = min(first.treat, na.rm = T)) %>% 
  ungroup()

municipalities <- municipalities %>% 
  mutate(first.treat = case_when(first.treat == Inf ~ 0,
                                 first.treat != Inf ~ first.treat))


# create an index to multiply nominal variables by
prices <- read_csv("intermediate_data/goods_and_services_prices.csv") %>% 
  select(-c(1))

CPI <- read_csv("intermediate_data/CPI.csv") %>% 
  select(-c(1))

multiplier <- prices %>% merge(CPI, by = c("oktmo", "year")) 
multiplier$index <- multiplier$index.x / multiplier$index.y
multiplier <- multiplier %>% select(oktmo, year, index) %>% 
  rename(region_oktmo = oktmo)


get_region_oktmo <- function(oktmo) {
  return(substr(oktmo, 1, nchar(oktmo)-6))
}

municipalities <- municipalities %>% 
  mutate(region_oktmo = get_region_oktmo(oktmo))

# left join
municipalities <- municipalities %>% 
  merge(multiplier, by = c("region_oktmo", "year"), all.x = T)

municipalities %>% write.csv("final_data/municipalities.csv", 
                             fileEncoding = "UTF-8")

