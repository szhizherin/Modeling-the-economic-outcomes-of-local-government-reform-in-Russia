# Purpose: reproduce CAG reaults on municipalities data
# Inputs:  intermediate_data/BDMO_id_name.csv
#          final_data/municipalities.csv
# Outputs: -




library(data.table)
library(dplyr)
library(fixest)
library(readr)
library(tidyr)




municipalities <- read_csv("final_data/municipalities.csv") %>% select(-c(1))
BDMO_id_name <- read_csv("intermediate_data/BDMO_id_name.csv") %>% select(-c(1))


municipalities <- municipalities %>% 
  mutate(treatment_status = case_when(model == "Избираемый мэр" ~ 0,
                                      model == "Сити-менеджер" ~ 1,
                                      model == "Назначаемый мэр" ~ 2))
municipalities <- municipalities %>% filter(!is.na(treatment_status)) %>% as.data.table()

municipalities[, treat := ifelse(first.treat == 0, 0, 1)]
municipalities[, time_to_treat := ifelse(treat==1, year - first.treat, 0)]

get_group <- function(treatment_history) {
  # 0 ~ "Избираемый мэр"
  # 1 ~ "Сити-менеджер"
  # 2 ~ "Назаначаемый мэр"
  # the expected behavior would be either 0 -> 2 or 0 -> 1 -> 2 or 0 -> 1 or
  # 1 -> 2 or constant treatment status
  
  if (!((treatment_history == cummax(treatment_history)) %>% all())) {
    return("unexpected")
  }
  
  if ((c(0, 1, 2) %in% treatment_history %>% all())) {
    return("0 -> 1 -> 2")
  }
  else if ((c(0, 1) %in% treatment_history %>% all())) {
    return("0 -> 1")
  } 
  else if ((c(0, 2) %in% treatment_history %>% all())) {
    return("0 -> 2")
  } 
  else if ((c(1, 2) %in% treatment_history %>% all())) {
    return("1 -> 2")
  }
  else if ((treatment_history == rep(0, length(treatment_history))) %>% all()) {
    return("0")
  }
  else if ((treatment_history == rep(1, length(treatment_history))) %>% all()) {
    return("1")
  }
  else if ((treatment_history == rep(2, length(treatment_history))) %>% all()) {
    return("2")
  }
}


municipalities <- municipalities %>% 
  group_by(oktmo) %>% mutate(group = get_group(treatment_status)) %>% ungroup()


# "c" stands for inflation- and regional prices-corrected
municipalities$wage_c <- municipalities$wage * municipalities$index
municipalities$investment_c <- municipalities$investment * municipalities$index
municipalities$volume_electr_c <- municipalities$volume_electr * municipalities$index
municipalities$volume_manufact_c <- municipalities$volume_manufact * municipalities$index
municipalities$t8013002_1_c <- municipalities$t8013002_1 * municipalities$index
municipalities$t8013002_212_c <- municipalities$t8013002_212 * municipalities$index
municipalities$t8013002_220_c <- municipalities$t8013002_220 * municipalities$index
municipalities$t8013002_221_c <- municipalities$t8013002_221 * municipalities$index
municipalities$t8013002_229_c <- municipalities$t8013002_229 * municipalities$index
municipalities$t8013002_234_c <- municipalities$t8013002_234 * municipalities$index
municipalities["t8013002_220_t8013002_1"] <- municipalities$t8013002_220 / municipalities$t8013002_1 # доля расходов на правоохранителей
municipalities$log_population <- log(municipalities$population)
municipalities$log_wage <- log(municipalities$wage)
municipalities["t8008008_t8008007"] <- municipalities$t8008008 / municipalities$t8008007 # доля водопроводной сети, нуждающейся в замене
municipalities["t8008025_t8008008"] <- municipalities$t8008025 / municipalities$t8008008 # доля отремонтированной от нуждающейся
municipalities["t8006003_t8006007"] <- municipalities$t8008025 / municipalities$t8008008 # доля освещенных частей улиц
municipalities$t8013001_1_c <- municipalities$t8013001_1 * municipalities$index
municipalities$t8013001_89_c <- municipalities$t8013001_89 * municipalities$index
municipalities["t8013001_89_t8013001_1"] <- municipalities$t8013001_89 / municipalities$t8013001_1 # доля собственных доходов
municipalities$t8013001_34_c <- municipalities$t8013001_34 * municipalities$index
municipalities$t8013001_36_c <- municipalities$t8013001_36 * municipalities$index
municipalities$t8013001_27_c <- municipalities$t8013001_27 * municipalities$index
municipalities["t8013001_34_t8013001_1"] <- municipalities$t8013001_34 / municipalities$t8013001_1 # доля безвозмездных поступлений в доходах
municipalities$log_per_capita_assets <- log(municipalities$assets / municipalities$population)



# "log_population", <- t8112013 или t8112027_11
# "log_per_capita_assets", <- t8045002_0 или t8045002_21 t8045002_26 t8045002_27
# "log_wage", <- avg_wage
# "share_profitable_firms", 
# "school", <- t8015001
# "school_child", <- t8015002
# "log_new_housing", <- t8010001
# "t8006007", 
# "t8011011_0"


#
#
#



