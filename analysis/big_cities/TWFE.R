# Purpose: estimate TWFE models on big_cities data
# Inputs:  intermediate_data/BDMO_id_name.csv
#          final_data/big_cities.csv
# Outputs: -




library(data.table)
library(dplyr)
library(fixest)
library(readr)
library(tidyr)




big_cities <- read_csv("final_data/big_cities.csv") %>% select(-c(1))
BDMO_id_name <- read_csv("intermediate_data/BDMO_id_name.csv") %>% select(-c(1))

big_cities <- big_cities %>% 
  mutate(treatment_status = case_when(model == "Избираемый мэр" ~ 0,
                                      model == "Сити-менеджер" ~ 1,
                                      model == "Назначаемый мэр" ~ 2))
big_cities <- big_cities %>% filter(!is.na(treatment_status)) %>% as.data.table()

big_cities[, treat := ifelse(first.treat == 0, 0, 1)]
big_cities[, time_to_treat := ifelse(treat==1, year - first.treat, 0)]

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


big_cities <- big_cities %>% 
  group_by(oktmo) %>% mutate(group = get_group(treatment_status)) %>% ungroup()

big_cities[big_cities$first.treat == 0,]$big_cities <- 99999 # для корректной работы SA оценки


# "c" stands for inflation- and regional prices-corrected
big_cities$catering_c <- big_cities$catering * big_cities$index
big_cities$construction_c <- big_cities$construction * big_cities$index
big_cities$pension_c <- big_cities$pension * big_cities$index
big_cities$retail_c <- big_cities$retail * big_cities$index
big_cities$wage_c <- big_cities$wage * big_cities$index
big_cities$investment_c <- big_cities$investment * big_cities$index
big_cities$volume_electr_c <- big_cities$volume_electr * big_cities$index
big_cities$volume_manufact_c <- big_cities$volume_manufact * big_cities$index

# расходы
big_cities$t8013002_1_c <- big_cities$t8013002_1 * big_cities$index
big_cities$t8013002_212_c <- big_cities$t8013002_212 * big_cities$index
big_cities$t8013002_220_c <- big_cities$t8013002_220 * big_cities$index
big_cities$t8013002_221_c <- big_cities$t8013002_221 * big_cities$index
big_cities$t8013002_229_c <- big_cities$t8013002_229 * big_cities$index
big_cities$t8013002_234_c <- big_cities$t8013002_234 * big_cities$index
big_cities$t8013002_239_c <- big_cities$t8013002_239 * big_cities$index # культура, кинематография
big_cities$t8013002_285_c <- big_cities$t8013002_285 * big_cities$index # дороги
big_cities$t8013002_434_c <- big_cities$t8013002_434 * big_cities$index # физкультура и спорт

# per capita
big_cities$t8013002_1_c_pc <- big_cities$t8013002_1_c / big_cities$population
big_cities$t8013002_212_c_pc <- big_cities$t8013002_212_c / big_cities$population
big_cities$t8013002_220_c_pc <- big_cities$t8013002_220_c / big_cities$population
big_cities$t8013002_221_c_pc <- big_cities$t8013002_221_c / big_cities$population
big_cities$t8013002_229_c_pc <- big_cities$t8013002_229_c / big_cities$population
big_cities$t8013002_234_c_pc <- big_cities$t8013002_234_c / big_cities$population
big_cities$t8013002_239_c_pc <- big_cities$t8013002_239_c / big_cities$population
big_cities$t8013002_285_c_pc <- big_cities$t8013002_285_c / big_cities$population
big_cities$t8013002_434_c_pc <- big_cities$t8013002_434_c / big_cities$population

big_cities$t8013002_220_c_per_crime <- big_cities$t8013002_220_c / big_cities$crimes

big_cities["t8013002_220_t8013002_1"] <- big_cities$t8013002_220 / big_cities$t8013002_1 # доля расходов на правоохранителей
big_cities$log_population <- log1p(big_cities$population)
big_cities$log_wage <- log1p(big_cities$wage_c)
big_cities["t8008008_t8008007"] <- big_cities$t8008008 / big_cities$t8008007 # доля водопроводной сети, нуждающейся в замене
big_cities["t8008025_t8008008"] <- big_cities$t8008025 / big_cities$t8008008 # доля отремонтированной от нуждающейся
big_cities["t8006003_t8006007"] <- big_cities$t8008025 / big_cities$t8008008 # доля освещенных частей улиц

big_cities$t8013001_1_c <- big_cities$t8013001_1 * big_cities$index
big_cities$t8013001_89_c <- big_cities$t8013001_89 * big_cities$index
big_cities["t8013001_89_t8013001_1"] <- big_cities$t8013001_89 / big_cities$t8013001_1 # доля собственных доходов
big_cities$t8013001_34_c <- big_cities$t8013001_34 * big_cities$index
big_cities$t8013001_36_c <- big_cities$t8013001_36 * big_cities$index
big_cities$t8013001_27_c <- big_cities$t8013001_27 * big_cities$index
big_cities["t8013001_34_t8013001_1"] <- big_cities$t8013001_34 / big_cities$t8013001_1 # доля безвозмездных поступлений в доходах
big_cities["t8013001_36_t8013001_1"] <- big_cities$t8013001_36 / big_cities$t8013001_1 # доля безвозмездных поступлений от других бюджетов

# per capita
big_cities$t8013001_1_c_pc <- big_cities$t8013001_1_c / big_cities$population
big_cities$t8013001_89_c_pc <- big_cities$t8013001_89_c / big_cities$population
big_cities$t8013001_34_c_pc <- big_cities$t8013001_34_c / big_cities$population
big_cities$t8013001_36_c_pc <- big_cities$t8013001_36_c / big_cities$population
big_cities$t8013001_27_c_pc <- big_cities$t8013001_27_c / big_cities$population

big_cities$budget_prof_c <- big_cities$t8013001_1_c - big_cities$t8013002_1_c # профицит бюджета
big_cities$budget_prof_c_pc <- big_cities$budget_prof_c / big_cities$population # профицит бюджета на душу

big_cities$log_per_capita_assets <- log1p(big_cities$assets * big_cities$index / big_cities$population)

big_cities$schools_per_1000 <- big_cities$t8015001 / big_cities$population
big_cities$pupils_per_1000 <- big_cities$t8015002 / big_cities$population
big_cities$log_new_housing <- log1p(big_cities$t8010001)
big_cities$log_build_flat <- log1p(big_cities$build_flat)

# per capita
big_cities$catering_c_pc <- big_cities$catering_c / big_cities$population
big_cities$construction_c_pc <- big_cities$construction_c / big_cities$population
big_cities$retail_c_pc <- big_cities$retail_c / big_cities$population
big_cities$investment_c_pc <- big_cities$investment_c / big_cities$population
big_cities$volume_electr_c_pc <- big_cities$volume_electr_c / big_cities$population
big_cities$volume_manufact_c_pc <- big_cities$volume_manufact_c / big_cities$population

big_cities$t8109002_c <- big_cities$t8109002 * big_cities$index # Инвестиции в основной капитал организаций муниципальной формы собственности
big_cities$t8109002_c_pc <- big_cities$t8109002_c / big_cities$population
big_cities$t8009001_c <- big_cities$t8009001 * big_cities$index # Инвестиции в основной капитал за счет средств бюджета муниципального образования
big_cities$t8009001_c_pc <- big_cities$t8009001_c / big_cities$population

big_cities$t8013004_c <- big_cities$t8013004 * big_cities$index # Расходы на содержание работников местного СУ на душу
big_cities$t8123017_12_c <- big_cities$t8123017_12 * big_cities$index # Зарплата работников госуправления

big_cities$share_culture_workers <- big_cities$t8016002 / big_cities$workers # доля работников культуры

big_cities$log_per_capita_assets %>% is.na() %>% sum()

big_cities %>% filter(competitive == 0) %>% select(settlement, year, treatment, treatment_status, group) %>% View()
# schools_per_1000 pupils_per_1000 t8006007 t8010001 pension_c

# share_profitable_firms (588), t8011011_0 (398) - число нуждающихся в жилье семей,
# log_per_capita_assets (311)

# c("log_build_flat", "log_new_housing", "catering_c_pc", "construction_c_pc", 
#   "retail_c_pc", "volume_electr_c_pc", "volume_manufact_c_pc", "doctors_per10",
#   "living_space", "n_companies", "pop_work", "log_population", "log_wage",
#   "workers", "t8006003", "pension_c") - почти не добавляют пропусков

# c("log_build_flat", "log_new_housing", "catering_c_pc", "construction_c_pc",
#   "retail_c_pc", "volume_electr_c_pc", "volume_manufact_c_pc", "doctors_per10",
#   "living_space", "n_companies", "pop_work", "log_population", "log_wage",
#   "workers", "t8006003", "pension_c", "schools_per_1000", "pupils_per_1000",
#   "t8006007") с добавлением школ и дорог (по 200 пропусков)

# c("log_build_flat", "log_new_housing", "catering_c_pc", "construction_c_pc",
#   "retail_c_pc", "volume_electr_c_pc", "volume_manufact_c_pc", "doctors_per10",
#   "living_space", "n_companies", "pop_work", "log_population", "log_wage",
#   "workers", "t8006003", "pension_c", "schools_per_1000", "pupils_per_1000",
#   "t8006007", "log_per_capita_assets", "t8011011_0") с добавлением активов и нуждающихся семей

# c("log_build_flat", "log_new_housing", "catering_c_pc", "construction_c_pc",
#   "retail_c_pc", "volume_electr_c_pc", "volume_manufact_c_pc", "doctors_per10",
#   "living_space", "n_companies", "pop_work", "log_population", "log_wage",
#   "workers", "t8006003", "pension_c", "schools_per_1000", "pupils_per_1000",
#   "t8006007", "log_per_capita_assets", "t8011011_0", "share_profitable_firms") все контроли

non_competitive_elections_old <- c("Республика Адыгея", "Республика Дагестан", 
                               "Республика Ингушетия", 
                               "Кабардино-Балкарская республика",
                               "Карачаево-Черкесская республика", 
                               "Республика Северная Осетия - Алания",
                               "Чеченская республика", 
                               "Астраханская область",
                               "Брянская область", "Республика Башкортостан", 
                               "Республика Калмыкия", "Кемеровская область",
                               "Чукотский автономный округ", 
                               "Республика Мордовия", "Саратовская область",
                               "Орловская область", "Тамбовская область",
                               "Республика Татарстан", "Тульская область",
                               "Республика Тыва", "Тюменская область",
                               "Республика Саха (Якутия)",
                               "Ямало-Ненецкий автономный округ")

non_competitive_elections <- c("Чеченская республика", "Республика Дагестан", 
                               "Республика Ингушетия", 
                               "Карачаево-Черкесская республика", "Республика Тыва", 
                               "Республика Мордовия", "Ямало-Ненецкий автономный округ", 
                               "Республика Татарстан", "Кабардино-Балкарская республика", 
                               "Кемеровская область", "Республика Башкортостан", "Тюменская область", 
                               "Чукотский автономный округ", "Тамбовская область", 
                               "Саратовская область", "Республика Калмыкия", 
                               "Республика Северная Осетия - Алания", 
                               "Республика Саха (Якутия)", 
                               "Астраханская область", "Тульская область")

big_cities$competitive <- 1 * !(big_cities$region %in% non_competitive_elections)

################################################################################


y_var <- "t8013002_1_c_pc"
cov_vars <- c("build_flat", "catering_c", "construction_c", "doctors_per10", 
              "living_space", "n_companies", "pop_work", "log_population", 
              "retail_c", "log_wage", "workers", "t8006003")

data <- big_cities %>%    
  filter(group != "unexpected") %>%  
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", "competitive", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(t8013002_1_c_pc ~ i(time_to_treat, treat, ref = -1) +          ## Key interaction: time × treatment status
                   build_flat + catering_c + construction_c + doctors_per10 + 
                   living_space + n_companies + pop_work + log_population + 
                   retail_c + log_wage + workers + t8006003 |                   ## Other controls
                   settlement + year,                                           ## FEs
                 cluster = ~region,                                             ## Clustered SEs
                 data = data)

mod_twfe_total = feols(t8013002_1_c_pc ~ treatment +       
                   build_flat + catering_c + construction_c + doctors_per10 + 
                   living_space + n_companies + pop_work + log_population + 
                   retail_c + log_wage + workers + t8006003 |                   
                   settlement + year,                                           
                 cluster = ~region,                                             
                 data = data)
summary(mod_twfe_total)


# mod_twfe_total = feols(t8013002_1_c ~ i(treatment, competitive, 0) + treatment + 
#                          build_flat + catering_c + construction_c + doctors_per10 + 
#                          living_space + n_companies + pop_work + log_population + 
#                          retail_c + log_wage + workers + t8006003 + 
#                          share_profitable_firms + log_per_capita_assets |                   
#                          settlement + year,                                           
#                        cluster = ~region,                                             
#                        data = data)
# summary(mod_twfe_total) # здесь внезапно 5% значимости тритмента

mod_twfe_total = feols(t8013002_1_c_pc ~ i(treatment, competitive, 0) + treatment + 
                         build_flat + catering_c + construction_c + doctors_per10 + 
                         living_space + n_companies + pop_work + log_population + 
                         retail_c + log_wage + workers + t8006003 |                   
                         settlement + year,                                           
                       cluster = ~region,                                             
                       data = data)
summary(mod_twfe_total) # а здесь нет значимости (с 1837 наблюдениями)


iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(t8013002_1_c_pc ~ sunab(first.treat, year, -1) + #+ i(treatment, competitive, 0) +                     ## key interaction: time × treatment status
                   build_flat + catering_c + construction_c + doctors_per10 + 
                   living_space + n_companies + pop_work + log_population + 
                   retail_c + log_wage + workers + t8006003 |                   ## Other controls
                   settlement + year,                                           ## FEs
                 cluster = ~region,                                             ## Clustered SEs
                 data = data)
summary(mod_sa, agg = "att")

iplot(list(mod_twfe, mod_sa), sep = 0.5, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2020)"), cex = 0.7)


################################################################################

# все расходы на душу населения с минимальным набором контролей
y_var <- "t8013002_1_c_pc"
cov_vars <- c("log_build_flat", "log_new_housing", "catering_c_pc", "construction_c_pc",
              "retail_c_pc", "volume_electr_c_pc", "volume_manufact_c_pc", "doctors_per10",
              "living_space", "n_companies", "pop_work", "log_population", "log_wage",
              "workers", "t8006003", "pension_c")

data <- big_cities %>% 
  filter(group != "unexpected") %>% 
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(t8013002_1_c_pc ~ i(time_to_treat, treat, ref = -1) + 
                   log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                   retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                   living_space + n_companies + pop_work + log_population + log_wage +
                   workers + t8006003 + pension_c | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

mod_twfe_total = feols(t8013002_1_c_pc ~ treatment +       
                         log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                         retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                         living_space + n_companies + pop_work + log_population + log_wage +
                         workers + t8006003 + pension_c |                   
                         settlement + year,                                           
                       cluster = ~region,                                             
                       data = data)
summary(mod_twfe_total)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(t8013002_1_c_pc ~ sunab(first.treat, year) + 
                 log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                 retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                 living_space + n_companies + pop_work + log_population + log_wage +
                 workers + t8006003 + pension_c | 
                 settlement + year,  
               cluster = ~region,  
               data = data)
summary(mod_sa, agg = "att")

iplot(list(mod_twfe, mod_sa), sep = 0.5, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2020)"), cex = 0.7)

################################################################################

# все расходы на душу населения с минимальным набором контролей | competitive
y_var <- "t8013002_1_c_pc" # 1%
cov_vars <- c("log_build_flat", "log_new_housing", "catering_c_pc", "construction_c_pc",
              "retail_c_pc", "volume_electr_c_pc", "volume_manufact_c_pc", "doctors_per10",
              "living_space", "n_companies", "pop_work", "log_population", "log_wage",
              "workers", "t8006003", "pension_c", "competitive")

data <- big_cities %>% 
  filter(group != "unexpected") %>% 
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(t8013002_1_c_pc ~ i(time_to_treat*(1-competitive), treat, ref = -1) + treatment + 
                   log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                   retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                   living_space + n_companies + pop_work + log_population + log_wage +
                   workers + t8006003 + pension_c | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

mod_twfe_total = feols(t8013002_1_c_pc ~ i(treatment, competitive, 1) + treatment +      
                         log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                         retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                         living_space + n_companies + pop_work + log_population + log_wage +
                         workers + t8006003 + pension_c |                   
                         settlement + year,                                           
                       cluster = ~region,                                             
                       data = data)
summary(mod_twfe_total)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(t8013002_1_c_pc ~ sunab(first.treat*(1-competitive), year) + treatment +
                 log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                 retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                 living_space + n_companies + pop_work + log_population + log_wage +
                 workers + t8006003 + pension_c | 
                 settlement + year[competitive],  
               cluster = ~region,  
               data = data)
summary(mod_sa, agg = "att")

iplot(list(mod_twfe, mod_sa), sep = 0.5, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2020)"), cex = 0.7)

################################################################################

# все расходы на душу населения с добавлением школ и дорог (по 200 пропусков) 
y_var <- "t8013002_1_c_pc"
cov_vars <- c("log_build_flat", "log_new_housing", "catering_c_pc", "construction_c_pc",
              "retail_c_pc", "volume_electr_c_pc", "volume_manufact_c_pc", "doctors_per10",
              "living_space", "n_companies", "pop_work", "log_population", "log_wage",
              "workers", "t8006003", "pension_c", "schools_per_1000", "pupils_per_1000",
              "t8006007")

data <- big_cities %>%    
  filter(group != "unexpected") %>%  
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(t8013002_1_c_pc ~ i(time_to_treat, treat, ref = -1) + 
                   log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                   retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                   living_space + n_companies + pop_work + log_population + log_wage +
                   workers + t8006003 + pension_c + schools_per_1000 + pupils_per_1000 +
                   t8006007 | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

mod_twfe_total = feols(t8013002_1_c_pc ~ treatment +       
                         log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                         retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                         living_space + n_companies + pop_work + log_population + log_wage +
                         workers + t8006003 + pension_c + schools_per_1000 + pupils_per_1000 +
                         t8006007 |                   
                         settlement + year,                                           
                       cluster = ~region,                                             
                       data = data)
summary(mod_twfe_total)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(t8013002_1_c_pc ~ sunab(first.treat, year) + 
                 log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                 retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                 living_space + n_companies + pop_work + log_population + log_wage +
                 workers + t8006003 + pension_c + schools_per_1000 + pupils_per_1000 +
                 t8006007 | 
                 settlement + year,  
               cluster = ~region,  
               data = data)
summary(mod_sa, agg = "att")

iplot(list(mod_twfe, mod_sa), sep = 0.5, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2020)"), cex = 0.7)


################################################################################

# все расходы на душу населения с добавлением школ и дорог (по 200 пропусков) | competitive
y_var <- "t8013002_1_c_pc" # 1%, лучшие претренды
cov_vars <- c("log_build_flat", "log_new_housing", "catering_c_pc", "construction_c_pc",
              "retail_c_pc", "volume_electr_c_pc", "volume_manufact_c_pc", "doctors_per10",
              "living_space", "n_companies", "pop_work", "log_population", "log_wage",
              "workers", "t8006003", "pension_c", "schools_per_1000", "pupils_per_1000",
              "t8006007", "competitive")

data <- big_cities %>% 
  filter(group != "unexpected") %>% 
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(t8013002_1_c_pc ~ i(time_to_treat*(1-competitive), treat, ref = -1) + treatment +
                   log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                   retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                   living_space + n_companies + pop_work + log_population + log_wage +
                   workers + t8006003 + pension_c + schools_per_1000 + pupils_per_1000 +
                   t8006007 | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

mod_twfe_total = feols(t8013002_1_c_pc ~ i(treatment, competitive, 1) + treatment +     
                         log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                         retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                         living_space + n_companies + pop_work + log_population + log_wage +
                         workers + t8006003 + pension_c + schools_per_1000 + pupils_per_1000 +
                         t8006007 |                   
                         settlement + year[competitive],                                           
                       cluster = ~region,                                             
                       data = data)
summary(mod_twfe_total)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')
# construction_c_pc ++ log_new_housing catering_c_pc  +
mod_sa = feols(t8013002_1_c_pc ~ sunab(first.treat*(1-competitive), year) + treatment +
                 log_build_flat + construction_c_pc + log_new_housing + catering_c_pc +
                 retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                 living_space + n_companies + pop_work + log_population + log_wage +
                 workers + t8006003 + pension_c + schools_per_1000 + pupils_per_1000 +
                 t8006007 | 
                 settlement + year + year[competitive*treat] + year[treat] + year[competitive],  
               cluster = ~region,  
               data = data)
summary(mod_sa, agg = "att")
wald(mod_sa, keep = "year::-[2]")

iplot(mod_sa, ci_level = 0.99, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (SA)')
iplot(list(mod_twfe, mod_sa), sep = 0.5, ref.line = -1, ci_level = 0.99,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2020)"), cex = 0.7)


################################################################################

# все расходы на душу населения с претрендами | competitive
y_var <- "t8013002_1_c_pc" # 1 %, идеальные претренды
cov_vars <- c("log_build_flat",
              "retail_c_pc", "volume_electr_c_pc", "volume_manufact_c_pc", "doctors_per10",
              "living_space", "n_companies", "pop_work", "log_population", "log_wage",
              "workers", "t8006003", "pension_c", "schools_per_1000", "pupils_per_1000",
              "t8006007")

data <- big_cities %>% 
  filter(group != "unexpected") %>% 
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", "competitive",  y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(t8013002_1_c_pc ~ i(time_to_treat*(1-competitive), treat, ref = -1) + treatment +
                   log_build_flat + 
                   retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                   living_space + n_companies + pop_work + log_population + log_wage +
                   workers + t8006003 + pension_c + schools_per_1000 + pupils_per_1000 +
                   t8006007 | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

mod_twfe_total = feols(t8013002_1_c_pc ~ i(treatment, competitive, 1) + treatment +     
                         log_build_flat + 
                         retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                         living_space + n_companies + pop_work + log_population + log_wage +
                         workers + t8006003 + pension_c + schools_per_1000 + pupils_per_1000 +
                         t8006007 |                   
                         settlement + year[competitive],                                           
                       cluster = ~region,                                             
                       data = data)
summary(mod_twfe_total)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(t8013002_1_c_pc ~ sunab(first.treat*(1-competitive), year) + treatment +
                 log_build_flat  + 
                 retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                 living_space + n_companies + pop_work + log_population + log_wage +
                 workers + t8006003 + pension_c + schools_per_1000 + pupils_per_1000 +
                 t8006007 | 
                 settlement + year + year[competitive],  
               cluster = ~region,  
               data = data)
summary(mod_sa, agg = "att")
wald(mod_sa, keep = "year::-[2]")

iplot(mod_sa, ci_level = 0.99, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (SA)')
iplot(list(mod_twfe, mod_sa), sep = 0.5, ref.line = -1, ci_level = 0.99,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2020)"), cex = 0.7)


################################################################################

# все расходы на душу населения с добавлением школ и дорог и нуждающихся | competitive
y_var <- "t8013002_1_c_pc" #
cov_vars <- c("log_build_flat", "log_new_housing", "catering_c_pc", "construction_c_pc",
              "retail_c_pc", "volume_electr_c_pc", "volume_manufact_c_pc", "doctors_per10",
              "living_space", "n_companies", "pop_work", "log_population", "log_wage",
              "workers", "t8006003", "pension_c", "schools_per_1000", "pupils_per_1000",
              "t8006007", "t8011011_0", "competitive")

data <- big_cities %>% 
  filter(group != "unexpected") %>% 
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(t8013002_1_c_pc ~ i(time_to_treat*(1-competitive), treat, ref = -1) + treatment +
                   log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                   retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                   living_space + n_companies + pop_work + log_population + log_wage +
                   workers + t8006003 + pension_c + schools_per_1000 + pupils_per_1000 +
                   t8006007 + t8011011_0 | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

mod_twfe_total = feols(t8013002_1_c_pc ~ i(treatment, competitive, 1) + treatment +     
                         log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                         retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                         living_space + n_companies + pop_work + log_population + log_wage +
                         workers + t8006003 + pension_c + schools_per_1000 + pupils_per_1000 +
                         t8006007 + t8011011_0 |                   
                         settlement + year[competitive],                                           
                       cluster = ~region,                                             
                       data = data)
summary(mod_twfe_total)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(t8013002_1_c_pc ~ sunab(first.treat*(1-competitive), year) + treatment +
                 log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                 retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                 living_space + n_companies + pop_work + log_population + log_wage +
                 workers + t8006003 + pension_c + schools_per_1000 + pupils_per_1000 +
                 t8006007 + t8011011_0 | 
                 settlement + year + year[competitive],  
               cluster = ~region,  
               data = data)
summary(mod_sa, agg = "att")
wald(mod_sa, keep = "year::-[2]")

iplot(list(mod_twfe, mod_sa), sep = 0.5, ref.line = -1, ci_level = 0.99,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2020)"), cex = 0.7)


################################################################################

# все расходы на душу населения все контроли
y_var <- "t8013002_1_c_pc"
cov_vars <- c("log_build_flat", "log_new_housing", "catering_c_pc", "construction_c_pc",
              "retail_c_pc", "volume_electr_c_pc", "volume_manufact_c_pc", "doctors_per10",
              "living_space", "n_companies", "pop_work", "log_population", "log_wage",
              "workers", "t8006003", "pension_c", "schools_per_1000", "pupils_per_1000",
              "t8006007", "log_per_capita_assets", "t8011011_0", "share_profitable_firms")

data <- big_cities %>%    
  filter(group != "unexpected") %>%  
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(t8013002_1_c_pc ~ i(time_to_treat, treat, ref = -1) + 
                   log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                   retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                   living_space + n_companies + pop_work + log_population + log_wage +
                   workers + t8006003 + pension_c + schools_per_1000 + pupils_per_1000 +
                   t8006007 + log_per_capita_assets + t8011011_0 + share_profitable_firms | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

mod_twfe_total = feols(t8013002_1_c_pc ~ treatment +       
                         log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                         retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                         living_space + n_companies + pop_work + log_population + log_wage +
                         workers + t8006003 + pension_c + schools_per_1000 + pupils_per_1000 +
                         t8006007 + log_per_capita_assets + t8011011_0 + share_profitable_firms |                   
                         settlement + year,                                           
                       cluster = ~region,                                             
                       data = data)
summary(mod_twfe_total)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(t8013002_1_c_pc ~ sunab(first.treat, year) + 
                 log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                 retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                 living_space + n_companies + pop_work + log_population + log_wage +
                 workers + t8006003 + pension_c + schools_per_1000 + pupils_per_1000 +
                 t8006007 + log_per_capita_assets + t8011011_0 + share_profitable_firms | 
                 settlement + year,  
               cluster = ~region,  
               data = data)
summary(mod_sa, agg = "att")
wald(mod_sa, keep = "year::-[2]")

iplot(list(mod_twfe, mod_sa), sep = 0.5, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2020)"), cex = 0.7)


################################################################################

# все расходы на душу населения все контроли | competitive
y_var <- "t8013002_1_c_pc"
cov_vars <- c("log_build_flat", "log_new_housing", "catering_c_pc", "construction_c_pc",
              "retail_c_pc", "volume_electr_c_pc", "volume_manufact_c_pc", "doctors_per10",
              "living_space", "n_companies", "pop_work", "log_population", "log_wage",
              "workers", "t8006003", "pension_c", "schools_per_1000", "pupils_per_1000",
              "t8006007", "log_per_capita_assets", "t8011011_0", "share_profitable_firms")

data <- big_cities %>%    
  filter(group != "unexpected") %>%  
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", "competitive", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(t8013002_1_c_pc ~ i(time_to_treat*(1-competitive), treat, ref = -1) + treatment + 
                   log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                   retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                   living_space + n_companies + pop_work + log_population + log_wage +
                   workers + t8006003 + pension_c + schools_per_1000 + pupils_per_1000 +
                   t8006007 + log_per_capita_assets + t8011011_0 + share_profitable_firms | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

mod_twfe_total = feols(t8013002_1_c_pc ~ i(treatment, competitive, 1) + treatment +      
                         log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                         retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                         living_space + n_companies + pop_work + log_population + log_wage +
                         workers + t8006003 + pension_c + schools_per_1000 + pupils_per_1000 +
                         t8006007 + log_per_capita_assets + t8011011_0 + share_profitable_firms |                   
                         settlement + year,                                           
                       cluster = ~region,                                             
                       data = data)
summary(mod_twfe_total)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(t8013002_1_c_pc ~ sunab(first.treat*(1-competitive), year) + treatment + 
                 log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                 retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                 living_space + n_companies + pop_work + log_population + log_wage +
                 workers + t8006003 + pension_c + schools_per_1000 + pupils_per_1000 +
                 t8006007 + log_per_capita_assets + t8011011_0 + share_profitable_firms | 
                 settlement + year[competitive] + year,  
               cluster = ~region,  
               data = data)
summary(mod_sa, agg = "att")
wald(mod_sa, keep = "year::-[2]")

iplot(list(mod_twfe, mod_sa), sep = 0.5, ref.line = -1, ci_level = 0.99,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2020)"), cex = 0.7)

iplot(mod_sa, ci_level = 0.99, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (SA)')
################################################################################

# все расходы на душу населения все контроли альтернативная версия (так хуже)
y_var <- "t8013002_1_c_pc"
cov_vars <- c("log_build_flat", "log_new_housing", "catering_c", "construction_c",
              "retail_c", "volume_electr_c", "volume_manufact_c", "doctors_per10",
              "living_space", "n_companies", "pop_work", "log_population", "log_wage",
              "workers", "t8006003", "pension_c", "schools_per_1000", "pupils_per_1000",
              "t8006007", "log_per_capita_assets", "t8011011_0", "share_profitable_firms")

data <- big_cities %>%    filter(group != "unexpected") %>%  
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(t8013002_1_c_pc ~ i(time_to_treat, treat, ref = -1) + 
                   log_build_flat + log_new_housing + catering_c + construction_c +
                   retail_c + volume_electr_c + volume_manufact_c + doctors_per10 +
                   living_space + n_companies + pop_work + log_population + log_wage +
                   workers + t8006003 + pension_c + schools_per_1000 + pupils_per_1000 +
                   t8006007 + log_per_capita_assets + t8011011_0 + share_profitable_firms | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

mod_twfe_total = feols(t8013002_1_c_pc ~ treatment +       
                         log_build_flat + log_new_housing + catering_c + construction_c +
                         retail_c + volume_electr_c + volume_manufact_c + doctors_per10 +
                         living_space + n_companies + pop_work + log_population + log_wage +
                         workers + t8006003 + pension_c + schools_per_1000 + pupils_per_1000 +
                         t8006007 + log_per_capita_assets + t8011011_0 + share_profitable_firms |                   
                         settlement + year,                                           
                       cluster = ~region,                                             
                       data = data)
summary(mod_twfe_total)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(t8013002_1_c_pc ~ sunab(first.treat, year) + 
                 log_build_flat + log_new_housing + catering_c + construction_c +
                 retail_c + volume_electr_c + volume_manufact_c + doctors_per10 +
                 living_space + n_companies + pop_work + log_population + log_wage +
                 workers + t8006003 + pension_c + schools_per_1000 + pupils_per_1000 +
                 t8006007 + log_per_capita_assets + t8011011_0 + share_profitable_firms | 
                 settlement + year,  
               cluster = ~region,  
               data = data)
summary(mod_sa, agg = "att")

iplot(list(mod_twfe, mod_sa), sep = 0.5, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2020)"), cex = 0.7)


################################################################################

# расходы на общегосударственные вопросы
y_var <- "t8013002_212_c"
cov_vars <- c("build_flat", "catering_c", "construction_c", "doctors_per10", 
              "living_space", "n_companies", "pop_work", "log_population", 
              "retail_c", "log_wage", "workers", "t8006003")

data <- big_cities %>%    
  filter(group != "unexpected") %>%  
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(t8013002_212_c ~ i(time_to_treat, treat, ref = -1) + 
                   build_flat + catering_c + construction_c + doctors_per10 + 
                   living_space + n_companies + pop_work + log_population + 
                   retail_c + log_wage + workers + t8006003 | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

mod_twfe_total = feols(t8013002_212_c ~ treatment +       
                         build_flat + catering_c + construction_c + doctors_per10 + 
                         living_space + n_companies + pop_work + log_population + 
                         retail_c + log_wage + workers + t8006003 |                   
                         settlement + year,                                           
                       cluster = ~region,                                             
                       data = data)
summary(mod_twfe_total)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(t8013002_212_c ~ sunab(first.treat, year) + 
                 build_flat + catering_c + construction_c + doctors_per10 + 
                 living_space + n_companies + pop_work + log_population + 
                 retail_c + log_wage + workers + t8006003 | 
                 settlement + year,  
               cluster = ~region,  
               data = data)
summary(mod_sa, agg = "att")

iplot(list(mod_twfe, mod_sa), sep = 0.5, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2020)"), cex = 0.7)

################################################################################

# расходы на общегосударственные вопросы на душу с минимальным набором контролей
y_var <- "t8013002_212_c_pc"
cov_vars <- c("log_build_flat", "log_new_housing", "catering_c_pc", "construction_c_pc",
              "retail_c_pc", "volume_electr_c_pc", "volume_manufact_c_pc", "doctors_per10",
              "living_space", "n_companies", "pop_work", "log_population", "log_wage",
              "workers", "t8006003", "pension_c")

data <- big_cities %>%    
  filter(group != "unexpected") %>%  
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(t8013002_212_c_pc ~ i(time_to_treat, treat, ref = -1) + 
                   log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                   retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                   living_space + n_companies + pop_work + log_population + log_wage +
                   workers + t8006003 + pension_c | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

mod_twfe_total = feols(t8013002_212_c_pc ~ treatment +       
                         log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                         retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                         living_space + n_companies + pop_work + log_population + log_wage +
                         workers + t8006003 + pension_c |                   
                         settlement + year,                                           
                       cluster = ~region,                                             
                       data = data)
summary(mod_twfe_total)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(t8013002_212_c_pc ~ sunab(first.treat, year) + 
                 log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                 retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                 living_space + n_companies + pop_work + log_population + log_wage +
                 workers + t8006003 + pension_c | 
                 settlement + year,  
               cluster = ~region,  
               data = data)
summary(mod_sa, agg = "att")

iplot(list(mod_twfe, mod_sa), sep = 0.5, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2020)"), cex = 0.7)


################################################################################

# расходы на общегосударственные вопросы на душу с минимальным набором контролей | competitive
y_var <- "t8013002_212_c_pc" # 1%
cov_vars <- c("log_build_flat", "log_new_housing", "catering_c_pc", "construction_c_pc",
              "retail_c_pc", "volume_electr_c_pc", "volume_manufact_c_pc", "doctors_per10",
              "living_space", "n_companies", "pop_work", "log_population", "log_wage",
              "workers", "t8006003", "pension_c", "competitive")

data <- big_cities %>%    
  filter(group != "unexpected") %>%  
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(t8013002_212_c_pc ~ i(time_to_treat*(1-competitive), treat, ref = -1) + treatment + 
                   log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                   retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                   living_space + n_companies + pop_work + log_population + log_wage +
                   workers + t8006003 + pension_c | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

mod_twfe_total = feols(t8013002_212_c_pc ~ i(treatment, competitive, 1) + treatment +      
                         log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                         retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                         living_space + n_companies + pop_work + log_population + log_wage +
                         workers + t8006003 + pension_c |                   
                         settlement + year,                                           
                       cluster = ~region,                                             
                       data = data)
summary(mod_twfe_total)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(t8013002_212_c_pc ~ sunab(first.treat*(1-competitive), year) + treatment +
                 log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                 retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                 living_space + n_companies + pop_work + log_population + log_wage +
                 workers + t8006003 + pension_c | 
                 settlement + year,  
               cluster = ~region,  
               data = data)
summary(mod_sa, agg = "att")

iplot(list(mod_twfe, mod_sa), sep = 0.5, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment', ci_level = 0.99)
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2020)"), cex = 0.7)

 
################################################################################

# расходы на правоохранителей
y_var <- "t8013002_220_c"
cov_vars <- c("build_flat", "catering_c", "construction_c", "doctors_per10", 
              "living_space", "n_companies", "pop_work", "log_population", 
              "retail_c", "log_wage", "workers", "t8006003",
              "share_profitable_firms", "log_per_capita_assets")

data <- big_cities %>%    filter(group != "unexpected") %>%  
  filter(competitive == 1) %>% 
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(t8013002_220_c ~ i(time_to_treat, treat, ref = -1) + 
                   build_flat + catering_c + construction_c + doctors_per10 + 
                   living_space + n_companies + pop_work + log_population + 
                   retail_c + log_wage + workers + t8006003 | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

mod_twfe_total = feols(t8013002_220_c ~ treatment +       
                         build_flat + catering_c + construction_c + doctors_per10 + 
                         living_space + n_companies + pop_work + log_population + 
                         retail_c + log_wage + workers + t8006003 |                   
                         settlement + year,                                           
                       cluster = ~region,                                             
                       data = data)
summary(mod_twfe_total)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(t8013002_220_c ~ sunab(first.treat, year) + 
                 log_population + log_wage + log_per_capita_assets +
                 share_profitable_firms | 
                 settlement + year, 
               cluster = ~region, 
               data = data)
summary(mod_sa, agg = "att")
iplot(mod_sa, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(t8013002_220_c ~ sunab(first.treat, year) + 
                 build_flat + catering_c + construction_c + doctors_per10 + 
                 living_space + n_companies + pop_work + log_population + 
                 retail_c + log_wage + workers + t8006003 | 
                 settlement + year,  
               cluster = ~region,  
               data = data)
summary(mod_sa, agg = "att")

iplot(list(mod_twfe, mod_sa), sep = 0.5, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2020)"), cex = 0.7)

################################################################################

# расходы на душу на правоохранителей на душу с минимальным набором контролей
y_var <- "t8013002_220_c_pc" # 10%
cov_vars <- c("log_build_flat", "log_new_housing", "catering_c_pc", "construction_c_pc",
              "retail_c_pc", "volume_electr_c_pc", "volume_manufact_c_pc", "doctors_per10",
              "living_space", "n_companies", "pop_work", "log_population", "log_wage",
              "workers", "t8006003", "pension_c")

data <- big_cities %>% 
  filter(group != "unexpected") %>% 
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(t8013002_220_c_pc ~ i(time_to_treat, treat, ref = -1) + 
                   log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                   retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                   living_space + n_companies + pop_work + log_population + log_wage +
                   workers + t8006003 + pension_c | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

mod_twfe_total = feols(t8013002_220_c_pc ~ treatment +       
                         log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                         retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                         living_space + n_companies + pop_work + log_population + log_wage +
                         workers + t8006003 + pension_c |                   
                         settlement + year,                                           
                       cluster = ~region,                                             
                       data = data)
summary(mod_twfe_total)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(t8013002_220_c_pc ~ sunab(first.treat, year) + 
                 log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                 retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                 living_space + n_companies + pop_work + log_population + log_wage +
                 workers + t8006003 + pension_c | 
                 settlement + year,  
               cluster = ~region,  
               data = data)
summary(mod_sa, agg = "att")

iplot(list(mod_twfe, mod_sa), sep = 0.5, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2020)"), cex = 0.7)


################################################################################

# расходы на душу на правоохранителей на душу с преступлениями
y_var <- "t8013002_220_c_pc" # 5% в обеих моделях
cov_vars <- c("log_build_flat", "log_new_housing", "catering_c_pc", "construction_c_pc",
              "retail_c_pc", "volume_electr_c_pc", "volume_manufact_c_pc", "doctors_per10",
              "living_space", "n_companies", "pop_work", "log_population", "log_wage",
              "workers", "t8006003", "pension_c", "crimes")

data <- big_cities %>% 
  filter(group != "unexpected") %>% 
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(t8013002_220_c_pc ~ i(time_to_treat, treat, ref = -1) + 
                   log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                   retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                   living_space + n_companies + pop_work + log_population + log_wage +
                   workers + t8006003 + pension_c + crimes | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

mod_twfe_total = feols(t8013002_220_c_pc ~ treatment +       
                         log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                         retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                         living_space + n_companies + pop_work + log_population + log_wage +
                         workers + t8006003 + pension_c + crimes |                   
                         settlement + year,                                           
                       cluster = ~region,                                             
                       data = data)
summary(mod_twfe_total)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(t8013002_220_c_pc ~ sunab(first.treat, year) + 
                 log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                 retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                 living_space + n_companies + pop_work + log_population + log_wage +
                 workers + t8006003 + pension_c + crimes | 
                 settlement + year,  
               cluster = ~region,  
               data = data)
summary(mod_sa, agg = "att")

iplot(list(mod_twfe, mod_sa), sep = 0.5, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2020)"), cex = 0.7)


################################################################################

# расходы на душу на правоохранителей на душу с преступлениями | competitve
y_var <- "t8013002_220_c_pc" # 1%
cov_vars <- c("log_build_flat", "log_new_housing", "catering_c_pc", "construction_c_pc",
              "retail_c_pc", "volume_electr_c_pc", "volume_manufact_c_pc", "doctors_per10",
              "living_space", "n_companies", "pop_work", "log_population", "log_wage",
              "workers", "t8006003", "pension_c", "crimes", "competitive")

data <- big_cities %>% 
  filter(group != "unexpected") %>% 
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(t8013002_220_c_pc ~ i(time_to_treat*(1-competitive), treat, ref = -1) + treatment + 
                   log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                   retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                   living_space + n_companies + pop_work + log_population + log_wage +
                   workers + t8006003 + pension_c + crimes | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

mod_twfe_total = feols(t8013002_220_c_pc ~ i(treatment, competitive, 1) + treatment + 
                         log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                         retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                         living_space + n_companies + pop_work + log_population + log_wage +
                         workers + t8006003 + pension_c + crimes |                   
                         settlement + year,                                           
                       cluster = ~region,                                             
                       data = data)
summary(mod_twfe_total)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(t8013002_220_c_pc ~ sunab(first.treat*(1-competitive), year) + treatment +
                 log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                 retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                 living_space + n_companies + pop_work + log_population + log_wage +
                 workers + t8006003 + pension_c + crimes | 
                 settlement + year,  
               cluster = ~region,  
               data = data)
summary(mod_sa, agg = "att")

iplot(list(mod_twfe, mod_sa), sep = 0.5, ref.line = -1, ci_level = 0.99,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2020)"), cex = 0.7)


################################################################################

# расходы на душу на правоохранителей с добавлением фирм и преступлений 
y_var <- "t8013002_220_c_pc" # 5%
cov_vars <- c("log_build_flat", "log_new_housing", "catering_c_pc", "construction_c_pc",
              "retail_c_pc", "volume_electr_c_pc", "volume_manufact_c_pc", "doctors_per10",
              "living_space", "n_companies", "pop_work", "log_population", "log_wage",
              "workers", "t8006003", "pension_c", "crimes", "share_profitable_firms")

data <- big_cities %>%    filter(group != "unexpected") %>%  
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(t8013002_220_c_pc ~ i(time_to_treat, treat, ref = -1) + 
                   log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                   retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                   living_space + n_companies + pop_work + log_population + log_wage +
                   workers + t8006003 + pension_c +
                   crimes + share_profitable_firms | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

mod_twfe_total = feols(t8013002_220_c_pc ~ treatment +       
                         log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                         retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                         living_space + n_companies + pop_work + log_population + log_wage +
                         workers + t8006003 + pension_c +
                         crimes + share_profitable_firms |                   
                         settlement + year,                                           
                       cluster = ~region,                                             
                       data = data)
summary(mod_twfe_total)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(t8013002_220_c_pc ~ sunab(first.treat, year) + 
                 log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                 retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                 living_space + n_companies + pop_work + log_population + log_wage +
                 workers + t8006003 + pension_c +
                 crimes + share_profitable_firms | 
                 settlement + year,  
               cluster = ~region,  
               data = data)
summary(mod_sa, agg = "att")

iplot(list(mod_twfe, mod_sa), sep = 0.5, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2020)"), cex = 0.7)


################################################################################

# расходы на душу на правоохранителей с добавлением фирм и преступлений | competitive
y_var <- "t8013002_220_c_pc" # 5%
cov_vars <- c("log_build_flat", "log_new_housing", "catering_c_pc", "construction_c_pc",
              "retail_c_pc", "volume_electr_c_pc", "volume_manufact_c_pc", "doctors_per10",
              "living_space", "n_companies", "pop_work", "log_population", "log_wage",
              "workers", "t8006003", "pension_c", "crimes", "share_profitable_firms", "competitive")

data <- big_cities %>% 
  filter(group != "unexpected") %>% 
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(t8013002_220_c_pc ~ i(time_to_treat*(1-competitive), treat, ref = -1) + treatment + 
                   log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                   retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                   living_space + n_companies + pop_work + log_population + log_wage +
                   workers + t8006003 + pension_c +
                   crimes + share_profitable_firms | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

mod_twfe_total = feols(t8013002_220_c_pc ~ i(treatment, competitive, 1) + treatment +     
                         log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                         retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                         living_space + n_companies + pop_work + log_population + log_wage +
                         workers + t8006003 + pension_c +
                         crimes + share_profitable_firms |                   
                         settlement + year,                                           
                       cluster = ~region,                                             
                       data = data)
summary(mod_twfe_total)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(t8013002_220_c_pc ~ sunab(first.treat*(1-competitive), year) + treatment +
                 log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                 retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                 living_space + n_companies + pop_work + log_population + log_wage +
                 workers + t8006003 + pension_c +
                 crimes + share_profitable_firms | 
                 settlement + year,  
               cluster = ~region,  
               data = data)
summary(mod_sa, agg = "att")

iplot(list(mod_twfe, mod_sa), sep = 0.5, ref.line = -1, ci_level = 0.99,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2020)"), cex = 0.7)


################################################################################

# расходы на душу на правоохранителей с добавлением фирм, дорог и преступлений 
y_var <- "t8013002_220_c_pc" # 5%
cov_vars <- c("log_build_flat", "log_new_housing", "catering_c_pc", "construction_c_pc",
              "retail_c_pc", "volume_electr_c_pc", "volume_manufact_c_pc", "doctors_per10",
              "living_space", "n_companies", "pop_work", "log_population", "log_wage",
              "workers", "t8006003", "pension_c", "t8006007", "crimes", "share_profitable_firms")

data <- big_cities %>%    
  filter(group != "unexpected") %>%  
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(t8013002_220_c_pc ~ i(time_to_treat, treat, ref = -1) + 
                   log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                   retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                   living_space + n_companies + pop_work + log_population + log_wage +
                   workers + t8006003 + pension_c +
                   t8006007 + crimes + share_profitable_firms | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

mod_twfe_total = feols(t8013002_220_c_pc ~ treatment +       
                         log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                         retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                         living_space + n_companies + pop_work + log_population + log_wage +
                         workers + t8006003 + pension_c +
                         t8006007 + crimes + share_profitable_firms |                   
                         settlement + year,                                           
                       cluster = ~region,                                             
                       data = data)
summary(mod_twfe_total)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(t8013002_220_c_pc ~ sunab(first.treat, year) + 
                 log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                 retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                 living_space + n_companies + pop_work + log_population + log_wage +
                 workers + t8006003 + pension_c +
                 t8006007 + crimes + share_profitable_firms | 
                 settlement + year,  
               cluster = ~region,  
               data = data)
summary(mod_sa, agg = "att")

iplot(list(mod_twfe, mod_sa), sep = 0.5, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2020)"), cex = 0.7)


################################################################################

# расходы на душу на правоохранителей с добавлением школ, фирм, дорог и преступлений 
y_var <- "t8013002_220_c_pc" # 5%
cov_vars <- c("log_build_flat", "log_new_housing", "catering_c_pc", "construction_c_pc",
              "retail_c_pc", "volume_electr_c_pc", "volume_manufact_c_pc", "doctors_per10",
              "living_space", "n_companies", "pop_work", "log_population", "log_wage",
              "workers", "t8006003", "pension_c", "schools_per_1000", "pupils_per_1000",
              "t8006007", "crimes", "share_profitable_firms")

data <- big_cities %>%    filter(group != "unexpected") %>%  
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(t8013002_220_c_pc ~ i(time_to_treat, treat, ref = -1) + 
                   log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                   retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                   living_space + n_companies + pop_work + log_population + log_wage +
                   workers + t8006003 + pension_c + schools_per_1000 + pupils_per_1000 +
                   t8006007 + crimes + share_profitable_firms | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

mod_twfe_total = feols(t8013002_220_c_pc ~ treatment +       
                         log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                         retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                         living_space + n_companies + pop_work + log_population + log_wage +
                         workers + t8006003 + pension_c + schools_per_1000 + pupils_per_1000 +
                         t8006007 + crimes + share_profitable_firms |                   
                         settlement + year,                                           
                       cluster = ~region,                                             
                       data = data)
summary(mod_twfe_total)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(t8013002_220_c_pc ~ sunab(first.treat, year) + 
                 log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                 retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                 living_space + n_companies + pop_work + log_population + log_wage +
                 workers + t8006003 + pension_c + schools_per_1000 + pupils_per_1000 +
                 t8006007 + crimes + share_profitable_firms | 
                 settlement + year,  
               cluster = ~region,  
               data = data)
summary(mod_sa, agg = "att")

iplot(list(mod_twfe, mod_sa), sep = 0.5, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2020)"), cex = 0.7)


################################################################################

# расходы на правоохранителей на зарегистрированное преступление с преступлениями
y_var <- "t8013002_220_c_per_crime" # 5 %
cov_vars <- c("log_build_flat", "log_new_housing", "catering_c_pc", "construction_c_pc",
              "retail_c_pc", "volume_electr_c_pc", "volume_manufact_c_pc", "doctors_per10",
              "living_space", "n_companies", "pop_work", "log_population", "log_wage",
              "workers", "t8006003", "pension_c", "crimes")

data <- big_cities %>%    
  filter(group != "unexpected") %>%  
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(t8013002_220_c_per_crime ~ i(time_to_treat, treat, ref = -1) + 
                   log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                   retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                   living_space + n_companies + pop_work + log_population + log_wage +
                   workers + t8006003 + pension_c + crimes | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

mod_twfe_total = feols(t8013002_220_c_per_crime ~ treatment +       
                         log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                         retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                         living_space + n_companies + pop_work + log_population + log_wage +
                         workers + t8006003 + pension_c + crimes |                   
                         settlement + year,                                           
                       cluster = ~region,                                             
                       data = data)
summary(mod_twfe_total)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(t8013002_220_c_per_crime ~ sunab(first.treat, year) + 
                 log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                 retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                 living_space + n_companies + pop_work + log_population + log_wage +
                 workers + t8006003 + pension_c + crimes | 
                 settlement + year,  
               cluster = ~region,  
               data = data)
summary(mod_sa, agg = "att")

iplot(list(mod_twfe, mod_sa), sep = 0.5, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2020)"), cex = 0.7)


################################################################################

# расходы на правоохранителей на зарегистрированное преступление с преступлениями | competitive
y_var <- "t8013002_220_c_per_crime" # 5 %
cov_vars <- c("log_build_flat", "log_new_housing", "catering_c_pc", "construction_c_pc",
              "retail_c_pc", "volume_electr_c_pc", "volume_manufact_c_pc", "doctors_per10",
              "living_space", "n_companies", "pop_work", "log_population", "log_wage",
              "workers", "t8006003", "pension_c", "crimes")

data <- big_cities %>%    
  filter(group != "unexpected") %>%  
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", "competitive", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(t8013002_220_c_per_crime ~ i(time_to_treat*(1-competitive), treat, ref = -1) + treatment +
                   log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                   retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                   living_space + n_companies + pop_work + log_population + log_wage +
                   workers + t8006003 + pension_c + crimes | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

mod_twfe_total = feols(t8013002_220_c_per_crime ~ i(treatment, competitive, 1) + treatment + 
                         log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                         retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                         living_space + n_companies + pop_work + log_population + log_wage +
                         workers + t8006003 + pension_c + crimes |                   
                         settlement + year,                                           
                       cluster = ~region,                                             
                       data = data)
summary(mod_twfe_total)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(t8013002_220_c_per_crime ~ sunab(first.treat*(1-competitive), year) + treatment + 
                 log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                 retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                 living_space + n_companies + pop_work + log_population + log_wage +
                 workers + t8006003 + pension_c + crimes | 
                 settlement + year,  
               cluster = ~region,  
               data = data)
summary(mod_sa, agg = "att")

iplot(list(mod_twfe, mod_sa), sep = 0.5, ref.line = -1, ci_level = 0.99,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2020)"), cex = 0.7)


################################################################################

# доля расходов на правоохранителей
y_var <- "t8013002_220_t8013002_1"
cov_vars <- c("build_flat", "catering_c", "construction_c", "doctors_per10", 
              "living_space", "n_companies", "pop_work", "log_population", 
              "retail_c", "log_wage", "workers", "t8006003",
              "share_profitable_firms", "log_per_capita_assets")

data <- big_cities %>%    
  filter(group != "unexpected") %>%  
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(t8013002_220_t8013002_1 ~ i(time_to_treat, treat, ref = -1) + 
                   build_flat + catering_c + construction_c + doctors_per10 + 
                   living_space + n_companies + pop_work + log_population + 
                   retail_c + log_wage + workers + t8006003 | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

mod_twfe_total = feols(t8013002_220_t8013002_1 ~ treatment +       
                         build_flat + catering_c + construction_c + doctors_per10 + 
                         living_space + n_companies + pop_work + log_population + 
                         retail_c + log_wage + workers + t8006003 |                   
                         settlement + year,                                           
                       cluster = ~region,                                             
                       data = data)
summary(mod_twfe_total)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(t8013002_220_t8013002_1 ~ sunab(first.treat, year, -1) + 
                 log_population + log_wage + log_per_capita_assets +
                 share_profitable_firms | 
                 settlement + year, 
               cluster = ~region, 
               data = data)
summary(mod_sa, agg = "att")
iplot(mod_sa, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(t8013002_220_t8013002_1 ~ sunab(first.treat, year) + 
                 build_flat + catering_c + construction_c + doctors_per10 + 
                 living_space + n_companies + pop_work + log_population + 
                 retail_c + log_wage + workers + t8006003 | 
                 settlement + year,  
               cluster = ~region,  
               data = data)
summary(mod_sa, agg = "att")

iplot(list(mod_twfe, mod_sa), sep = 0.5, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2020)"), cex = 0.7)


################################################################################

# доля расходов на правоохранителей с минимальным набором контролей
y_var <- "t8013002_220_t8013002_1"
cov_vars <- c("log_build_flat", "log_new_housing", "catering_c_pc", "construction_c_pc",
              "retail_c_pc", "volume_electr_c_pc", "volume_manufact_c_pc", "doctors_per10",
              "living_space", "n_companies", "pop_work", "log_population", "log_wage",
              "workers", "t8006003", "pension_c")

data <- big_cities %>%    filter(group != "unexpected") %>%  
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(t8013002_220_t8013002_1 ~ i(time_to_treat, treat, ref = -1) + 
                   log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                   retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                   living_space + n_companies + pop_work + log_population + log_wage +
                   workers + t8006003 + pension_c | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

mod_twfe_total = feols(t8013002_220_t8013002_1 ~ treatment +       
                         log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                         retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                         living_space + n_companies + pop_work + log_population + log_wage +
                         workers + t8006003 + pension_c |                   
                         settlement + year,                                           
                       cluster = ~region,                                             
                       data = data)
summary(mod_twfe_total)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(t8013002_220_t8013002_1 ~ sunab(first.treat, year) + 
                 log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                 retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                 living_space + n_companies + pop_work + log_population + log_wage +
                 workers + t8006003 + pension_c | 
                 settlement + year,  
               cluster = ~region,  
               data = data)
summary(mod_sa, agg = "att")

iplot(list(mod_twfe, mod_sa), sep = 0.5, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2020)"), cex = 0.7)


################################################################################

# доля расходов на правоохранителей с преступлениями
y_var <- "t8013002_220_t8013002_1"
cov_vars <- c("log_build_flat", "log_new_housing", "catering_c_pc", "construction_c_pc",
              "retail_c_pc", "volume_electr_c_pc", "volume_manufact_c_pc", "doctors_per10",
              "living_space", "n_companies", "pop_work", "log_population", "log_wage",
              "workers", "t8006003", "pension_c", "crimes")

data <- big_cities %>%    
  filter(group != "unexpected") %>%  
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(t8013002_220_t8013002_1 ~ i(time_to_treat, treat, ref = -1) + 
                   log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                   retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                   living_space + n_companies + pop_work + log_population + log_wage +
                   workers + t8006003 + pension_c + crimes | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

mod_twfe_total = feols(t8013002_220_t8013002_1 ~ treatment +       
                         log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                         retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                         living_space + n_companies + pop_work + log_population + log_wage +
                         workers + t8006003 + pension_c + crimes |                   
                         settlement + year,                                           
                       cluster = ~region,                                             
                       data = data)
summary(mod_twfe_total)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(t8013002_220_t8013002_1 ~ sunab(first.treat, year) + 
                 log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                 retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                 living_space + n_companies + pop_work + log_population + log_wage +
                 workers + t8006003 + pension_c + crimes | 
                 settlement + year,  
               cluster = ~region,  
               data = data)
summary(mod_sa, agg = "att")

iplot(list(mod_twfe, mod_sa), sep = 0.5, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2020)"), cex = 0.7)


################################################################################

# доля расходов на правоохранителей с преступлениями | competitive
y_var <- "t8013002_220_t8013002_1" # 5 %
cov_vars <- c("log_build_flat", "log_new_housing", "catering_c_pc", "construction_c_pc",
              "retail_c_pc", "volume_electr_c_pc", "volume_manufact_c_pc", "doctors_per10",
              "living_space", "n_companies", "pop_work", "log_population", "log_wage",
              "workers", "t8006003", "pension_c", "crimes", "competitive")

data <- big_cities %>%    
  filter(group != "unexpected") %>%  
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(t8013002_220_t8013002_1 ~ i(time_to_treat*(1-competitive), treat, ref = -1) + treatment + 
                   log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                   retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                   living_space + n_companies + pop_work + log_population + log_wage +
                   workers + t8006003 + pension_c + crimes | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

mod_twfe_total = feols(t8013002_220_t8013002_1 ~ i(treatment, competitive, 1) + treatment +      
                         log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                         retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                         living_space + n_companies + pop_work + log_population + log_wage +
                         workers + t8006003 + pension_c + crimes |                   
                         settlement + year,                                           
                       cluster = ~region,                                             
                       data = data)
summary(mod_twfe_total)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(t8013002_220_t8013002_1 ~ sunab(first.treat*(1-competitive), year) + treatment +
                 log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                 retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                 living_space + n_companies + pop_work + log_population + log_wage +
                 workers + t8006003 + pension_c + crimes | 
                 settlement + year,  
               cluster = ~region,  
               data = data)
summary(mod_sa, agg = "att")

iplot(list(mod_twfe, mod_sa), sep = 0.5, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2020)"), cex = 0.7)


################################################################################

# доля расходов на правоохранителей с преступлениями и фирмами
y_var <- "t8013002_220_t8013002_1"
cov_vars <- c("log_build_flat", "log_new_housing", "catering_c_pc", "construction_c_pc",
              "retail_c_pc", "volume_electr_c_pc", "volume_manufact_c_pc", "doctors_per10",
              "living_space", "n_companies", "pop_work", "log_population", "log_wage",
              "workers", "t8006003", "pension_c", "crimes", "share_profitable_firms")

data <- big_cities %>%    
  filter(group != "unexpected") %>%  
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(t8013002_220_t8013002_1 ~ i(time_to_treat, treat, ref = -1) + 
                   log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                   retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                   living_space + n_companies + pop_work + log_population + log_wage +
                   workers + t8006003 + pension_c + crimes + share_profitable_firms | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

mod_twfe_total = feols(t8013002_220_t8013002_1 ~ treatment +       
                         log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                         retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                         living_space + n_companies + pop_work + log_population + log_wage +
                         workers + t8006003 + pension_c + crimes + share_profitable_firms |                   
                         settlement + year,                                           
                       cluster = ~region,                                             
                       data = data)
summary(mod_twfe_total)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(t8013002_220_t8013002_1 ~ sunab(first.treat, year) + 
                 log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                 retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                 living_space + n_companies + pop_work + log_population + log_wage +
                 workers + t8006003 + pension_c + crimes + share_profitable_firms | 
                 settlement + year,  
               cluster = ~region,  
               data = data)
summary(mod_sa, agg = "att")

iplot(list(mod_twfe, mod_sa), sep = 0.5, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2020)"), cex = 0.7)


################################################################################

# доля расходов на правоохранителей с преступлениями и фирмами | competitive
y_var <- "t8013002_220_t8013002_1" # 1 %
cov_vars <- c("log_build_flat", "log_new_housing", "catering_c_pc", "construction_c_pc",
              "retail_c_pc", "volume_electr_c_pc", "volume_manufact_c_pc", "doctors_per10",
              "living_space", "n_companies", "pop_work", "log_population", "log_wage",
              "workers", "t8006003", "pension_c", "crimes", "share_profitable_firms")

data <- big_cities %>%    
  filter(group != "unexpected") %>%  
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", "competitive", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(t8013002_220_t8013002_1 ~ i(time_to_treat*(1-competitive), treat, ref = -1) + treatment +
                   log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                   retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                   living_space + n_companies + pop_work + log_population + log_wage +
                   workers + t8006003 + pension_c + crimes + share_profitable_firms | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

mod_twfe_total = feols(t8013002_220_t8013002_1 ~ i(treatment, competitive, 1) + treatment +        
                         log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                         retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                         living_space + n_companies + pop_work + log_population + log_wage +
                         workers + t8006003 + pension_c + crimes + share_profitable_firms |                   
                         settlement + year,                                           
                       cluster = ~region,                                             
                       data = data)
summary(mod_twfe_total)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(t8013002_220_t8013002_1 ~ sunab(first.treat*(1-competitive), year, ref.p=-1) + treatment +
                 log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                 retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                 living_space + n_companies + pop_work + log_population + log_wage +
                 workers + t8006003 + pension_c + crimes + share_profitable_firms | 
                 settlement + year,  
               cluster = ~region,  
               data = data)
summary(mod_sa, agg = "att")

iplot(list(mod_twfe, mod_sa), sep = 0.5, ref.line = -1, ci_level = 0.99,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2020)"), cex = 0.7)


################################################################################

# расходы на национальную экономику
y_var <- "t8013002_221_c"
cov_vars <- c("build_flat", "catering_c", "construction_c", "doctors_per10", 
              "living_space", "n_companies", "pop_work", "log_population", 
              "retail_c", "log_wage", "workers", "t8006003")

data <- big_cities %>%    filter(group != "unexpected") %>%  
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(t8013002_221_c ~ i(time_to_treat, treat, ref = -1) + 
                   build_flat + catering_c + construction_c + doctors_per10 + 
                   living_space + n_companies + pop_work + log_population + 
                   retail_c + log_wage + workers + t8006003 | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

mod_twfe_total = feols(t8013002_221_c ~ treatment +       
                         build_flat + catering_c + construction_c + doctors_per10 + 
                         living_space + n_companies + pop_work + log_population + 
                         retail_c + log_wage + workers + t8006003 |                   
                         settlement + year,                                           
                       cluster = ~region,                                             
                       data = data)
summary(mod_twfe_total)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(t8013002_221_c ~ sunab(first.treat, year) + 
                 build_flat + catering_c + construction_c + doctors_per10 + 
                 living_space + n_companies + pop_work + log_population + 
                 retail_c + log_wage + workers + t8006003 | 
                 settlement + year,  
               cluster = ~region,  
               data = data)
summary(mod_sa, agg = "att")

iplot(list(mod_twfe, mod_sa), sep = 0.5, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2020)"), cex = 0.7)


################################################################################

# расходы на национальную экономику на душу с минимальным набором контролей
y_var <- "t8013002_221_c_pc"
cov_vars <- c("log_build_flat", "log_new_housing", "catering_c_pc", "construction_c_pc",
              "retail_c_pc", "volume_electr_c_pc", "volume_manufact_c_pc", "doctors_per10",
              "living_space", "n_companies", "pop_work", "log_population", "log_wage",
              "workers", "t8006003", "pension_c")

data <- big_cities %>%    filter(group != "unexpected") %>%  
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(t8013002_221_c_pc ~ i(time_to_treat, treat, ref = -1) + 
                   log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                   retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                   living_space + n_companies + pop_work + log_population + log_wage +
                   workers + t8006003 + pension_c | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

mod_twfe_total = feols(t8013002_221_c_pc ~ treatment +       
                         log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                         retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                         living_space + n_companies + pop_work + log_population + log_wage +
                         workers + t8006003 + pension_c |                   
                         settlement + year,                                           
                       cluster = ~region,                                             
                       data = data)
summary(mod_twfe_total)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(t8013002_221_c_pc ~ sunab(first.treat, year) + 
                 log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                 retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                 living_space + n_companies + pop_work + log_population + log_wage +
                 workers + t8006003 + pension_c | 
                 settlement + year,  
               cluster = ~region,  
               data = data)
summary(mod_sa, agg = "att")

iplot(list(mod_twfe, mod_sa), sep = 0.5, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2020)"), cex = 0.7)


################################################################################

# расходы на национальную экономику на душу с минимальным набором контролей | competitive
y_var <- "t8013002_221_c_pc"
cov_vars <- c("log_build_flat", "log_new_housing", "catering_c_pc", "construction_c_pc",
              "retail_c_pc", "volume_electr_c_pc", "volume_manufact_c_pc", "doctors_per10",
              "living_space", "n_companies", "pop_work", "log_population", "log_wage",
              "workers", "t8006003", "pension_c")

data <- big_cities %>%    
  filter(group != "unexpected") %>%  
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", "competitive", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(t8013002_221_c_pc ~ i(time_to_treat*(1-competitive), treat, ref = -1) + treatment +
                   log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                   retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                   living_space + n_companies + pop_work + log_population + log_wage +
                   workers + t8006003 + pension_c | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

mod_twfe_total = feols(t8013002_221_c_pc ~ i(treatment, competitive, 1) + treatment + 
                         log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                         retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                         living_space + n_companies + pop_work + log_population + log_wage +
                         workers + t8006003 + pension_c |                   
                         settlement + year,                                           
                       cluster = ~region,                                             
                       data = data)
summary(mod_twfe_total)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(t8013002_221_c_pc ~ sunab(first.treat*(1-competitive), year) + treatment + 
                 log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                 retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                 living_space + n_companies + pop_work + log_population + log_wage +
                 workers + t8006003 + pension_c | 
                 settlement + year,  
               cluster = ~region,  
               data = data)
summary(mod_sa, agg = "att")

iplot(list(mod_twfe, mod_sa), sep = 0.5, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2020)"), cex = 0.7)


################################################################################

# расходы на ЖКХ
y_var <- "t8013002_229_c"
cov_vars <- c("build_flat", "catering_c", "construction_c", "doctors_per10", 
              "living_space", "n_companies", "pop_work", "log_population", 
              "retail_c", "log_wage", "workers", "t8006003")

data <- big_cities %>%    filter(group != "unexpected") %>%  
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(t8013002_229_c ~ i(time_to_treat, treat, ref = -1) + 
                   build_flat + catering_c + construction_c + doctors_per10 + 
                   living_space + n_companies + pop_work + log_population + 
                   retail_c + log_wage + workers + t8006003 | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

mod_twfe_total = feols(t8013002_229_c ~ treatment +       
                         build_flat + catering_c + construction_c + doctors_per10 + 
                         living_space + n_companies + pop_work + log_population + 
                         retail_c + log_wage + workers + t8006003 |                   
                         settlement + year,                                           
                       cluster = ~region,                                             
                       data = data)
summary(mod_twfe_total)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(t8013002_229_c ~ sunab(first.treat, year) + 
                 build_flat + catering_c + construction_c + doctors_per10 + 
                 living_space + n_companies + pop_work + log_population + 
                 retail_c + log_wage + workers + t8006003 | 
                 settlement + year,  
               cluster = ~region,  
               data = data)
summary(mod_sa, agg = "att")

iplot(list(mod_twfe, mod_sa), sep = 0.5, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2020)"), cex = 0.7)


################################################################################

# расходы на ЖКХ на душу с минимальным набором контролей
y_var <- "t8013002_229_c_pc" # 10%
cov_vars <- c("log_build_flat", "log_new_housing", "catering_c_pc", "construction_c_pc",
              "retail_c_pc", "volume_electr_c_pc", "volume_manufact_c_pc", "doctors_per10",
              "living_space", "n_companies", "pop_work", "log_population", "log_wage",
              "workers", "t8006003", "pension_c")

data <- big_cities %>%    filter(group != "unexpected") %>%  
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(t8013002_229_c_pc ~ i(time_to_treat, treat, ref = -1) + 
                   log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                   retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                   living_space + n_companies + pop_work + log_population + log_wage +
                   workers + t8006003 + pension_c | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

mod_twfe_total = feols(t8013002_229_c_pc ~ treatment +       
                         log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                         retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                         living_space + n_companies + pop_work + log_population + log_wage +
                         workers + t8006003 + pension_c |                   
                         settlement + year,                                           
                       cluster = ~region,                                             
                       data = data)
summary(mod_twfe_total)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(t8013002_229_c_pc ~ sunab(first.treat, year) + 
                 log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                 retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                 living_space + n_companies + pop_work + log_population + log_wage +
                 workers + t8006003 + pension_c | 
                 settlement + year,  
               cluster = ~region,  
               data = data)
summary(mod_sa, agg = "att")

iplot(list(mod_twfe, mod_sa), sep = 0.5, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2020)"), cex = 0.7)


################################################################################

# расходы на ЖКХ на душу с минимальным набором контролей | competitive
y_var <- "t8013002_229_c_pc" # 1 %
cov_vars <- c("log_build_flat", "log_new_housing", "catering_c_pc", "construction_c_pc",
              "retail_c_pc", "volume_electr_c_pc", "volume_manufact_c_pc", "doctors_per10",
              "living_space", "n_companies", "pop_work", "log_population", "log_wage",
              "workers", "t8006003", "pension_c")

data <- big_cities %>%    filter(group != "unexpected") %>%  
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", "competitive", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(t8013002_229_c_pc ~ i(time_to_treat*(1-competitive), treat, ref = -1) + treatment +
                   log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                   retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                   living_space + n_companies + pop_work + log_population + log_wage +
                   workers + t8006003 + pension_c | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

mod_twfe_total = feols(t8013002_229_c_pc ~ i(treatment, competitive, 1) + treatment + 
                         log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                         retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                         living_space + n_companies + pop_work + log_population + log_wage +
                         workers + t8006003 + pension_c |                   
                         settlement + year,                                           
                       cluster = ~region,                                             
                       data = data)
summary(mod_twfe_total)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(t8013002_229_c_pc ~ sunab(first.treat*(1-competitive), year, ref.p = -1) + treatment +
                 log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                 retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                 living_space + n_companies + pop_work + log_population + log_wage +
                 workers + t8006003 + pension_c | 
                 settlement + year,  
               cluster = ~region,  
               data = data)
summary(mod_sa, agg = "att")

iplot(list(mod_twfe, mod_sa), sep = 0.5, ref.line = -1, ci_level = 0.99,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2020)"), cex = 0.7)


################################################################################

# расходы на ЖКХ на душу с дорогами, школами, активами, нуждающимися и фирмами
y_var <- "t8013002_229_c_pc" # без фирм - как будто наоборот ближе к отрицательному, но все равно не значимо
cov_vars <- c("log_build_flat", "log_new_housing", "catering_c_pc", "construction_c_pc",
              "retail_c_pc", "volume_electr_c_pc", "volume_manufact_c_pc", "doctors_per10",
              "living_space", "n_companies", "pop_work", "log_population", "log_wage",
              "workers", "t8006003", "pension_c", "schools_per_1000", "pupils_per_1000",
              "t8006007", "log_per_capita_assets", "t8011011_0", "share_profitable_firms")

data <- big_cities %>%    filter(group != "unexpected") %>%  
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(t8013002_229_c_pc ~ i(time_to_treat, treat, ref = -1) + 
                   log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                   retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                   living_space + n_companies + pop_work + log_population + log_wage +
                   workers + t8006003 + pension_c + schools_per_1000 + pupils_per_1000 +
                   t8006007 + log_per_capita_assets + t8011011_0 + share_profitable_firms | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

mod_twfe_total = feols(t8013002_229_c_pc ~ treatment +       
                         log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                         retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                         living_space + n_companies + pop_work + log_population + log_wage +
                         workers + t8006003 + pension_c + schools_per_1000 + pupils_per_1000 +
                         t8006007 + log_per_capita_assets + t8011011_0 + share_profitable_firms |                   
                         settlement + year,                                           
                       cluster = ~region,                                             
                       data = data)
summary(mod_twfe_total)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(t8013002_229_c_pc ~ sunab(first.treat, year) + 
                 log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                 retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                 living_space + n_companies + pop_work + log_population + log_wage +
                 workers + t8006003 + pension_c + schools_per_1000 + pupils_per_1000 +
                 t8006007 + log_per_capita_assets + t8011011_0 + share_profitable_firms | 
                 settlement + year,  
               cluster = ~region,  
               data = data)
summary(mod_sa, agg = "att")

iplot(list(mod_twfe, mod_sa), sep = 0.5, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2020)"), cex = 0.7)


################################################################################

# расходы на ЖКХ на душу с дорогами, школами, активами, нуждающимися и фирмами | competitive
y_var <- "t8013002_229_c_pc" # 
cov_vars <- c("log_build_flat", "log_new_housing", "catering_c_pc", "construction_c_pc",
              "retail_c_pc", "volume_electr_c_pc", "volume_manufact_c_pc", "doctors_per10",
              "living_space", "n_companies", "pop_work", "log_population", "log_wage",
              "workers", "t8006003", "pension_c", "schools_per_1000", "pupils_per_1000",
              "t8006007", "log_per_capita_assets", "t8011011_0", "share_profitable_firms")

data <- big_cities %>%    
  filter(group != "unexpected") %>%  
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", "competitive", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(t8013002_229_c_pc ~ i(time_to_treat*(1-competitive), treat, ref = -1) + treatment +
                   log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                   retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                   living_space + n_companies + pop_work + log_population + log_wage +
                   workers + t8006003 + pension_c + schools_per_1000 + pupils_per_1000 +
                   t8006007 + log_per_capita_assets + t8011011_0 + share_profitable_firms | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

mod_twfe_total = feols(t8013002_229_c_pc ~ i(treatment, competitive, 1) + treatment + 
                         log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                         retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                         living_space + n_companies + pop_work + log_population + log_wage +
                         workers + t8006003 + pension_c + schools_per_1000 + pupils_per_1000 +
                         t8006007 + log_per_capita_assets + t8011011_0 + share_profitable_firms |                   
                         settlement + year,                                           
                       cluster = ~region,                                             
                       data = data)
summary(mod_twfe_total)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(t8013002_229_c_pc ~ sunab(first.treat*(1-competitive), year) + treatment +
                 log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                 retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                 living_space + n_companies + pop_work + log_population + log_wage +
                 workers + t8006003 + pension_c + schools_per_1000 + pupils_per_1000 +
                 t8006007 + log_per_capita_assets + t8011011_0 + share_profitable_firms | 
                 settlement + year,  
               cluster = ~region,  
               data = data)
summary(mod_sa, agg = "att")

iplot(list(mod_twfe, mod_sa), sep = 0.5, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2020)"), cex = 0.7)


################################################################################

# расходы на социальную политику
y_var <- "t8013002_234_c"
cov_vars <- c("build_flat", "catering_c", "construction_c", "doctors_per10", 
              "living_space", "n_companies", "pop_work", "log_population", 
              "retail_c", "log_wage", "workers", "t8006003")

data <- big_cities %>%    
  filter(group != "unexpected") %>%  
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(t8013002_234_c ~ i(time_to_treat, treat, ref = -1) + 
                   build_flat + catering_c + construction_c + doctors_per10 + 
                   living_space + n_companies + pop_work + log_population + 
                   retail_c + log_wage + workers + t8006003 | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

mod_twfe_total = feols(t8013002_234_c ~ treatment +       
                         build_flat + catering_c + construction_c + doctors_per10 + 
                         living_space + n_companies + pop_work + log_population + 
                         retail_c + log_wage + workers + t8006003 |                   
                         settlement + year,                                           
                       cluster = ~region,                                             
                       data = data)
summary(mod_twfe_total)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(t8013002_234_c ~ sunab(first.treat, year) + 
                 build_flat + catering_c + construction_c + doctors_per10 + 
                 living_space + n_companies + pop_work + log_population + 
                 retail_c + log_wage + workers + t8006003 | 
                 settlement + year,  
               cluster = ~region,  
               data = data)
summary(mod_sa, agg = "att")

iplot(list(mod_twfe, mod_sa), sep = 0.5, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2020)"), cex = 0.7)


################################################################################

# расходы на социальную политику на душу с минимальным набором контролей
y_var <- "t8013002_234_c_pc" # 10%
cov_vars <- c("log_build_flat", "log_new_housing", "catering_c_pc", "construction_c_pc",
              "retail_c_pc", "volume_electr_c_pc", "volume_manufact_c_pc", "doctors_per10",
              "living_space", "n_companies", "pop_work", "log_population", "log_wage",
              "workers", "t8006003", "pension_c")

data <- big_cities %>%    
  filter(group != "unexpected") %>%  
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(t8013002_234_c_pc ~ i(time_to_treat, treat, ref = -1) + 
                   log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                   retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                   living_space + n_companies + pop_work + log_population + log_wage +
                   workers + t8006003 + pension_c | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

mod_twfe_total = feols(t8013002_234_c_pc ~ treatment +       
                         log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                         retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                         living_space + n_companies + pop_work + log_population + log_wage +
                         workers + t8006003 + pension_c |                   
                         settlement + year,                                           
                       cluster = ~region,                                             
                       data = data)
summary(mod_twfe_total)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(t8013002_234_c_pc ~ sunab(first.treat, year) + 
                 log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                 retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                 living_space + n_companies + pop_work + log_population + log_wage +
                 workers + t8006003 + pension_c | 
                 settlement + year,  
               cluster = ~region,  
               data = data)
summary(mod_sa, agg = "att")

iplot(list(mod_twfe, mod_sa), sep = 0.5, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2020)"), cex = 0.7)


################################################################################

# расходы на социальную политику на душу с минимальным набором контролей | competitive
y_var <- "t8013002_234_c_pc"
cov_vars <- c("log_build_flat", "log_new_housing", "catering_c_pc", "construction_c_pc",
              "retail_c_pc", "volume_electr_c_pc", "volume_manufact_c_pc", "doctors_per10",
              "living_space", "n_companies", "pop_work", "log_population", "log_wage",
              "workers", "t8006003", "pension_c", "competitive")

data <- big_cities %>%    
  filter(group != "unexpected") %>%  
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(t8013002_234_c_pc ~ i(time_to_treat*(1-competitive), treat, ref = -1) + treatment + 
                   log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                   retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                   living_space + n_companies + pop_work + log_population + log_wage +
                   workers + t8006003 + pension_c | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

mod_twfe_total = feols(t8013002_234_c_pc ~ i(treatment, competitive, 1) + treatment +      
                         log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                         retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                         living_space + n_companies + pop_work + log_population + log_wage +
                         workers + t8006003 + pension_c |                   
                         settlement + year,                                           
                       cluster = ~region,                                             
                       data = data)
summary(mod_twfe_total)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(t8013002_234_c_pc ~ sunab(first.treat*(1-competitive), year) + treatment +
                 log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                 retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                 living_space + n_companies + pop_work + log_population + log_wage +
                 workers + t8006003 + pension_c | 
                 settlement + year,  
               cluster = ~region,  
               data = data)
summary(mod_sa, agg = "att")

iplot(list(mod_twfe, mod_sa), sep = 0.5, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2020)"), cex = 0.7)


################################################################################

# расходы на социальную политику на душу с нуждающимися
y_var <- "t8013002_234_c_pc" # при добавлении фирм тоже незначимо
cov_vars <- c("log_build_flat", "log_new_housing", "catering_c_pc", "construction_c_pc",
              "retail_c_pc", "volume_electr_c_pc", "volume_manufact_c_pc", "doctors_per10",
              "living_space", "n_companies", "pop_work", "log_population", "log_wage",
              "workers", "t8006003", "pension_c", "t8011011_0")

data <- big_cities %>%    filter(group != "unexpected") %>%  
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(t8013002_234_c_pc ~ i(time_to_treat, treat, ref = -1) + 
                   log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                   retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                   living_space + n_companies + pop_work + log_population + log_wage +
                   workers + t8006003 + pension_c + t8011011_0 | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

mod_twfe_total = feols(t8013002_234_c_pc ~ treatment +       
                         log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                         retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                         living_space + n_companies + pop_work + log_population + log_wage +
                         workers + t8006003 + pension_c + t8011011_0 |                   
                         settlement + year,                                           
                       cluster = ~region,                                             
                       data = data)
summary(mod_twfe_total)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(t8013002_234_c_pc ~ sunab(first.treat, year) + 
                 log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                 retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                 living_space + n_companies + pop_work + log_population + log_wage +
                 workers + t8006003 + pension_c + t8011011_0 | 
                 settlement + year,  
               cluster = ~region,  
               data = data)
summary(mod_sa, agg = "att")

iplot(list(mod_twfe, mod_sa), sep = 0.5, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2020)"), cex = 0.7)


################################################################################

# расходы на культуру на душу с минимальным набором контролей
y_var <- "t8013002_239_c_pc"
cov_vars <- c("log_build_flat", "log_new_housing", "catering_c_pc", "construction_c_pc",
              "retail_c_pc", "volume_electr_c_pc", "volume_manufact_c_pc", "doctors_per10",
              "living_space", "n_companies", "pop_work", "log_population", "log_wage",
              "workers", "t8006003", "pension_c")

data <- big_cities %>%    
  filter(group != "unexpected") %>%  
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(t8013002_239_c_pc ~ i(time_to_treat, treat, ref = -1) + 
                   log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                   retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                   living_space + n_companies + pop_work + log_population + log_wage +
                   workers + t8006003 + pension_c | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

mod_twfe_total = feols(t8013002_239_c_pc ~ treatment +       
                         log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                         retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                         living_space + n_companies + pop_work + log_population + log_wage +
                         workers + t8006003 + pension_c |                   
                         settlement + year,                                           
                       cluster = ~region,                                             
                       data = data)
summary(mod_twfe_total)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(t8013002_239_c_pc ~ sunab(first.treat, year) + 
                 log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                 retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                 living_space + n_companies + pop_work + log_population + log_wage +
                 workers + t8006003 + pension_c | 
                 settlement + year,  
               cluster = ~region,  
               data = data)
summary(mod_sa, agg = "att")

iplot(list(mod_twfe, mod_sa), sep = 0.5, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2020)"), cex = 0.7)


################################################################################

# расходы на культуру на душу с минимальным набором контролей | competitive
y_var <- "t8013002_239_c_pc"
cov_vars <- c("log_build_flat", "log_new_housing", "catering_c_pc", "construction_c_pc",
              "retail_c_pc", "volume_electr_c_pc", "volume_manufact_c_pc", "doctors_per10",
              "living_space", "n_companies", "pop_work", "log_population", "log_wage",
              "workers", "t8006003", "pension_c")

data <- big_cities %>%    
  filter(group != "unexpected") %>%  
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", "competitive", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(t8013002_239_c_pc ~ i(time_to_treat*(1-competitive), treat, ref = -1) + treatment +
                   log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                   retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                   living_space + n_companies + pop_work + log_population + log_wage +
                   workers + t8006003 + pension_c | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

mod_twfe_total = feols(t8013002_239_c_pc ~ i(treatment, competitive, 1) + treatment + 
                         log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                         retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                         living_space + n_companies + pop_work + log_population + log_wage +
                         workers + t8006003 + pension_c |                   
                         settlement + year,                                           
                       cluster = ~region,                                             
                       data = data)
summary(mod_twfe_total)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(t8013002_239_c_pc ~ sunab(first.treat*(1-competitive), year) + treatment +
                 log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                 retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                 living_space + n_companies + pop_work + log_population + log_wage +
                 workers + t8006003 + pension_c | 
                 settlement + year,  
               cluster = ~region,  
               data = data)
summary(mod_sa, agg = "att")

iplot(list(mod_twfe, mod_sa), sep = 0.5, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2020)"), cex = 0.7)


################################################################################

# расходы на дороги на душу с минимальным набором контролей
y_var <- "t8013002_285_c_pc"
cov_vars <- c("log_build_flat", "log_new_housing", "catering_c_pc", "construction_c_pc",
              "retail_c_pc", "volume_electr_c_pc", "volume_manufact_c_pc", "doctors_per10",
              "living_space", "n_companies", "pop_work", "log_population", "log_wage",
              "workers", "t8006003", "pension_c")

data <- big_cities %>%    
  filter(group != "unexpected") %>%  
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(t8013002_285_c_pc ~ i(time_to_treat, treat, ref = -1) + 
                   log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                   retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                   living_space + n_companies + pop_work + log_population + log_wage +
                   workers + t8006003 + pension_c | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

mod_twfe_total = feols(t8013002_285_c_pc ~ treatment +       
                         log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                         retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                         living_space + n_companies + pop_work + log_population + log_wage +
                         workers + t8006003 + pension_c |                   
                         settlement + year,                                           
                       cluster = ~region,                                             
                       data = data)
summary(mod_twfe_total)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(t8013002_285_c_pc ~ sunab(first.treat, year) + 
                 log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                 retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                 living_space + n_companies + pop_work + log_population + log_wage +
                 workers + t8006003 + pension_c | 
                 settlement + year,  
               cluster = ~region,  
               data = data)
summary(mod_sa, agg = "att")

iplot(list(mod_twfe, mod_sa), sep = 0.5, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2020)"), cex = 0.7)


################################################################################

# расходы на дороги на душу с дорогами
y_var <- "t8013002_285_c_pc"
cov_vars <- c("log_build_flat", "log_new_housing", "catering_c_pc", "construction_c_pc",
              "retail_c_pc", "volume_electr_c_pc", "volume_manufact_c_pc", "doctors_per10",
              "living_space", "n_companies", "pop_work", "log_population", "log_wage",
              "workers", "t8006003", "pension_c", "t8006007")

data <- big_cities %>%    
  filter(group != "unexpected") %>%  
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(t8013002_285_c_pc ~ i(time_to_treat, treat, ref = -1) + 
                   log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                   retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                   living_space + n_companies + pop_work + log_population + log_wage +
                   workers + t8006003 + pension_c + t8006007 | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

mod_twfe_total = feols(t8013002_285_c_pc ~ treatment +       
                         log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                         retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                         living_space + n_companies + pop_work + log_population + log_wage +
                         workers + t8006003 + pension_c + t8006007 |                   
                         settlement + year,                                           
                       cluster = ~region,                                             
                       data = data)
summary(mod_twfe_total)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(t8013002_285_c_pc ~ sunab(first.treat, year) + 
                 log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                 retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                 living_space + n_companies + pop_work + log_population + log_wage +
                 workers + t8006003 + pension_c + t8006007 | 
                 settlement + year,  
               cluster = ~region,  
               data = data)
summary(mod_sa, agg = "att")

iplot(list(mod_twfe, mod_sa), sep = 0.5, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2020)"), cex = 0.7)


################################################################################

# расходы на физкультуру и спорт на душу с минимальным набором контролей
y_var <- "t8013002_434_c_pc"
cov_vars <- c("log_build_flat", "log_new_housing", "catering_c_pc", "construction_c_pc",
              "retail_c_pc", "volume_electr_c_pc", "volume_manufact_c_pc", "doctors_per10",
              "living_space", "n_companies", "pop_work", "log_population", "log_wage",
              "workers", "t8006003", "pension_c")

data <- big_cities %>%    
  filter(group != "unexpected") %>%  
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(t8013002_434_c_pc ~ i(time_to_treat, treat, ref = -1) + 
                   log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                   retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                   living_space + n_companies + pop_work + log_population + log_wage +
                   workers + t8006003 + pension_c | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

mod_twfe_total = feols(t8013002_434_c_pc ~ treatment +       
                         log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                         retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                         living_space + n_companies + pop_work + log_population + log_wage +
                         workers + t8006003 + pension_c |                   
                         settlement + year,                                           
                       cluster = ~region,                                             
                       data = data)
summary(mod_twfe_total)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(t8013002_434_c_pc ~ sunab(first.treat, year) + 
                 log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                 retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                 living_space + n_companies + pop_work + log_population + log_wage +
                 workers + t8006003 + pension_c | 
                 settlement + year,  
               cluster = ~region,  
               data = data)
summary(mod_sa, agg = "att")

iplot(list(mod_twfe, mod_sa), sep = 0.5, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2020)"), cex = 0.7)


################################################################################

# доля инвестиций за счет бюджета
y_var <- "invest_budg"
cov_vars <- c("build_flat", "catering_c", "construction_c", "doctors_per10", 
              "living_space", "n_companies", "pop_work", "log_population", 
              "retail_c", "log_wage", "workers", "t8006003")

data <- big_cities %>%    
  filter(group != "unexpected") %>%  
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(invest_budg ~ i(time_to_treat, treat, ref = -1) + 
                   build_flat + catering_c + construction_c + doctors_per10 + 
                   living_space + n_companies + pop_work + log_population + 
                   retail_c + log_wage + workers + t8006003 | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

mod_twfe_total = feols(invest_budg ~ treatment +       
                         build_flat + catering_c + construction_c + doctors_per10 + 
                         living_space + n_companies + pop_work + log_population + 
                         retail_c + log_wage + workers + t8006003 |                   
                         settlement + year,                                           
                       cluster = ~region,                                             
                       data = data)
summary(mod_twfe_total)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(invest_budg ~ sunab(first.treat, year) + 
                 build_flat + catering_c + construction_c + doctors_per10 + 
                 living_space + n_companies + pop_work + log_population + 
                 retail_c + log_wage + workers + t8006003 | 
                 settlement + year,  
               cluster = ~region,  
               data = data)
summary(mod_sa, agg = "att")

iplot(list(mod_twfe, mod_sa), sep = 0.5, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2020)"), cex = 0.7)


################################################################################

# доля инвестиций за счет бюджета с минимальным набором контролей
y_var <- "invest_budg"
cov_vars <- c("log_build_flat", "log_new_housing", "catering_c_pc", "construction_c_pc",
              "retail_c_pc", "volume_electr_c_pc", "volume_manufact_c_pc", "doctors_per10",
              "living_space", "n_companies", "pop_work", "log_population", "log_wage",
              "workers", "t8006003", "pension_c")

data <- big_cities %>%    
  filter(group != "unexpected") %>%  
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(invest_budg ~ i(time_to_treat, treat, ref = -1) + 
                   log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                   retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                   living_space + n_companies + pop_work + log_population + log_wage +
                   workers + t8006003 + pension_c | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

mod_twfe_total = feols(invest_budg ~ treatment +       
                         log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                         retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                         living_space + n_companies + pop_work + log_population + log_wage +
                         workers + t8006003 + pension_c |                   
                         settlement + year,                                           
                       cluster = ~region,                                             
                       data = data)
summary(mod_twfe_total)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(invest_budg ~ sunab(first.treat, year) + 
                 log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                 retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                 living_space + n_companies + pop_work + log_population + log_wage +
                 workers + t8006003 + pension_c | 
                 settlement + year,  
               cluster = ~region,  
               data = data)
summary(mod_sa, agg = "att")

iplot(list(mod_twfe, mod_sa), sep = 0.5, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2020)"), cex = 0.7)


################################################################################

# доля инвестиций за счет бюджета с минимальным набором контролей | competitive
y_var <- "invest_budg" # 1 % и 5 % TODO: добавить контроли
cov_vars <- c("log_build_flat", "log_new_housing", "catering_c_pc", "construction_c_pc",
              "retail_c_pc", "volume_electr_c_pc", "volume_manufact_c_pc", "doctors_per10",
              "living_space", "n_companies", "pop_work", "log_population", "log_wage",
              "workers", "t8006003", "pension_c")

data <- big_cities %>%    
  filter(group != "unexpected") %>%  
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", "competitive", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(invest_budg ~ i(time_to_treat*(1-competitive), treat, ref = -1) + treatment +
                   log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                   retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                   living_space + n_companies + pop_work + log_population + log_wage +
                   workers + t8006003 + pension_c | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

mod_twfe_total = feols(invest_budg ~ i(treatment, competitive, 1) + treatment + 
                         log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                         retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                         living_space + n_companies + pop_work + log_population + log_wage +
                         workers + t8006003 + pension_c |                   
                         settlement + year,                                           
                       cluster = ~region,                                             
                       data = data)
summary(mod_twfe_total)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(invest_budg ~ sunab(first.treat*(1-competitive), year) + treatment +
                 log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                 retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                 living_space + n_companies + pop_work + log_population + log_wage +
                 workers + t8006003 + pension_c | 
                 settlement + year,  
               cluster = ~region,  
               data = data)
summary(mod_sa, agg = "att")

iplot(list(mod_twfe, mod_sa), sep = 0.5, ref.line = -1, ci_level = 0.99,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2020)"), cex = 0.7)


################################################################################

# доля инвестиций за счет федерального бюджета
y_var <- "invest_fed"
cov_vars <- c("build_flat", "catering_c", "construction_c", "doctors_per10", 
              "living_space", "n_companies", "pop_work", "log_population", 
              "retail_c", "log_wage", "workers", "t8006003")

data <- big_cities %>%    filter(group != "unexpected") %>%  
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(invest_fed ~ i(time_to_treat, treat, ref = -1) + 
                   build_flat + catering_c + construction_c + doctors_per10 + 
                   living_space + n_companies + pop_work + log_population + 
                   retail_c + log_wage + workers + t8006003 | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

mod_twfe_total = feols(invest_fed ~ treatment +       
                         build_flat + catering_c + construction_c + doctors_per10 + 
                         living_space + n_companies + pop_work + log_population + 
                         retail_c + log_wage + workers + t8006003 |                   
                         settlement + year,                                           
                       cluster = ~region,                                             
                       data = data)
summary(mod_twfe_total)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(invest_fed ~ sunab(first.treat, year) + 
                 build_flat + catering_c + construction_c + doctors_per10 + 
                 living_space + n_companies + pop_work + log_population + 
                 retail_c + log_wage + workers + t8006003 | 
                 settlement + year,  
               cluster = ~region,  
               data = data)
summary(mod_sa, agg = "att")

iplot(list(mod_twfe, mod_sa), sep = 0.5, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2020)"), cex = 0.7)


################################################################################

# доля инвестиций за счет федерального бюджета с минимальным набором контролей
y_var <- "invest_fed"
cov_vars <- c("log_build_flat", "log_new_housing", "catering_c_pc", "construction_c_pc",
              "retail_c_pc", "volume_electr_c_pc", "volume_manufact_c_pc", "doctors_per10",
              "living_space", "n_companies", "pop_work", "log_population", "log_wage",
              "workers", "t8006003", "pension_c")

data <- big_cities %>%    
  filter(group != "unexpected") %>%  
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(invest_fed ~ i(time_to_treat, treat, ref = -1) + 
                   log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                   retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                   living_space + n_companies + pop_work + log_population + log_wage +
                   workers + t8006003 + pension_c | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

mod_twfe_total = feols(invest_fed ~ treatment +       
                         log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                         retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                         living_space + n_companies + pop_work + log_population + log_wage +
                         workers + t8006003 + pension_c |                   
                         settlement + year,                                           
                       cluster = ~region,                                             
                       data = data)
summary(mod_twfe_total)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(invest_fed ~ sunab(first.treat, year) + 
                 log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                 retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                 living_space + n_companies + pop_work + log_population + log_wage +
                 workers + t8006003 + pension_c | 
                 settlement + year,  
               cluster = ~region,  
               data = data)
summary(mod_sa, agg = "att")

iplot(list(mod_twfe, mod_sa), sep = 0.5, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2020)"), cex = 0.7)


################################################################################

# доля инвестиций за счет федерального бюджета с минимальным набором контролей | competitive
y_var <- "invest_fed" # 1% и 5%
cov_vars <- c("log_build_flat", "log_new_housing", "catering_c_pc", "construction_c_pc",
              "retail_c_pc", "volume_electr_c_pc", "volume_manufact_c_pc", "doctors_per10",
              "living_space", "n_companies", "pop_work", "log_population", "log_wage",
              "workers", "t8006003", "pension_c")

data <- big_cities %>%    
  filter(group != "unexpected") %>%  
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", "competitive", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(invest_fed ~ i(time_to_treat*(1-competitive), treat, ref = -1) + treatment +
                   log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                   retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                   living_space + n_companies + pop_work + log_population + log_wage +
                   workers + t8006003 + pension_c | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

mod_twfe_total = feols(invest_fed ~ i(treatment, competitive, 1) + treatment + 
                         log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                         retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                         living_space + n_companies + pop_work + log_population + log_wage +
                         workers + t8006003 + pension_c |                   
                         settlement + year,                                           
                       cluster = ~region,                                             
                       data = data)
summary(mod_twfe_total)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(invest_fed ~ sunab(first.treat*(1-competitive), year) + treatment +
                 log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                 retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                 living_space + n_companies + pop_work + log_population + log_wage +
                 workers + t8006003 + pension_c | 
                 settlement + year,  
               cluster = ~region,  
               data = data)
summary(mod_sa, agg = "att")

iplot(list(mod_twfe, mod_sa), sep = 0.5, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2020)"), cex = 0.7)


################################################################################

# инвестиции в ОК
y_var <- "investment_c"
cov_vars <- c("build_flat", "catering_c", "construction_c", "doctors_per10", 
              "living_space", "n_companies", "pop_work", "log_population", 
              "retail_c", "log_wage", "workers", "t8006003")

data <- big_cities %>%    
  filter(group != "unexpected") %>%  
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(investment_c ~ i(time_to_treat, treat, ref = -1) + 
                   build_flat + catering_c + construction_c + doctors_per10 + 
                   living_space + n_companies + pop_work + log_population + 
                   retail_c + log_wage + workers + t8006003 | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

mod_twfe_total = feols(investment_c ~ treatment +       
                         build_flat + catering_c + construction_c + doctors_per10 + 
                         living_space + n_companies + pop_work + log_population + 
                         retail_c + log_wage + workers + t8006003 |                   
                         settlement + year,                                           
                       cluster = ~region,                                             
                       data = data)
summary(mod_twfe_total)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(investment_c ~ sunab(first.treat, year) + 
                 build_flat + catering_c + construction_c + doctors_per10 + 
                 living_space + n_companies + pop_work + log_population + 
                 retail_c + log_wage + workers + t8006003 | 
                 settlement + year,  
               cluster = ~region,  
               data = data)
summary(mod_sa, agg = "att")

iplot(list(mod_twfe, mod_sa), sep = 0.5, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2020)"), cex = 0.7)


################################################################################

# инвестиции в ОК на душу с минимальным набором контролей
y_var <- "investment_c_pc"
cov_vars <- c("log_build_flat", "log_new_housing", "catering_c_pc", "construction_c_pc",
              "retail_c_pc", "volume_electr_c_pc", "volume_manufact_c_pc", "doctors_per10",
              "living_space", "n_companies", "pop_work", "log_population", "log_wage",
              "workers", "t8006003", "pension_c")

data <- big_cities %>%    filter(group != "unexpected") %>%  
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(investment_c_pc ~ i(time_to_treat, treat, ref = -1) + 
                   log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                   retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                   living_space + n_companies + pop_work + log_population + log_wage +
                   workers + t8006003 + pension_c | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

mod_twfe_total = feols(investment_c_pc ~ treatment +       
                         log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                         retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                         living_space + n_companies + pop_work + log_population + log_wage +
                         workers + t8006003 + pension_c |                   
                         settlement + year,                                           
                       cluster = ~region,                                             
                       data = data)
summary(mod_twfe_total)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(investment_c_pc ~ sunab(first.treat, year) + 
                 log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                 retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                 living_space + n_companies + pop_work + log_population + log_wage +
                 workers + t8006003 + pension_c | 
                 settlement + year,  
               cluster = ~region,  
               data = data)
summary(mod_sa, agg = "att")

iplot(list(mod_twfe, mod_sa), sep = 0.5, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2020)"), cex = 0.7)


################################################################################

# инвестиции в ОК на душу с минимальным набором контролей | competitive
y_var <- "investment_c_pc"
cov_vars <- c("log_build_flat", "log_new_housing", "catering_c_pc", "construction_c_pc",
              "retail_c_pc", "volume_electr_c_pc", "volume_manufact_c_pc", "doctors_per10",
              "living_space", "n_companies", "pop_work", "log_population", "log_wage",
              "workers", "t8006003", "pension_c")

data <- big_cities %>%    
  filter(group != "unexpected") %>%  
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", "competitive", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(investment_c_pc ~ i(time_to_treat*(1-competitive), treat, ref = -1) + treatment +
                   log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                   retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                   living_space + n_companies + pop_work + log_population + log_wage +
                   workers + t8006003 + pension_c | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

mod_twfe_total = feols(investment_c_pc ~ i(treatment, competitive, 1) + treatment + 
                         log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                         retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                         living_space + n_companies + pop_work + log_population + log_wage +
                         workers + t8006003 + pension_c |                   
                         settlement + year,                                           
                       cluster = ~region,                                             
                       data = data)
summary(mod_twfe_total)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(investment_c_pc ~ sunab(first.treat*(1-competitive), year) + treatment +
                 log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                 retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                 living_space + n_companies + pop_work + log_population + log_wage +
                 workers + t8006003 + pension_c | 
                 settlement + year,  
               cluster = ~region,  
               data = data)
summary(mod_sa, agg = "att")

iplot(list(mod_twfe, mod_sa), sep = 0.5, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2020)"), cex = 0.7)


################################################################################

# инвестиции в ОК муниципальных фирм на душу с фирмами и активами
y_var <- "t8109002_c_pc"
cov_vars <- c("log_build_flat", "log_new_housing", "catering_c_pc", "construction_c_pc",
              "retail_c_pc", "volume_electr_c_pc", "volume_manufact_c_pc", "doctors_per10",
              "living_space", "n_companies", "pop_work", "log_population", "log_wage",
              "workers", "t8006003", "pension_c", "share_profitable_firms", "log_per_capita_assets")

data <- big_cities %>%    
  filter(group != "unexpected") %>%  
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(t8109002_c_pc ~ i(time_to_treat, treat, ref = -1) + 
                   log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                   retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                   living_space + n_companies + pop_work + log_population + log_wage +
                   workers + t8006003 + pension_c + share_profitable_firms + log_per_capita_assets | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

mod_twfe_total = feols(t8109002_c_pc ~ treatment +       
                         log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                         retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                         living_space + n_companies + pop_work + log_population + log_wage +
                         workers + t8006003 + pension_c + share_profitable_firms + log_per_capita_assets |                   
                         settlement + year,                                           
                       cluster = ~region,                                             
                       data = data)
summary(mod_twfe_total)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(t8109002_c_pc ~ sunab(first.treat, year) + 
                 log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                 retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                 living_space + n_companies + pop_work + log_population + log_wage +
                 workers + t8006003 + pension_c + share_profitable_firms + log_per_capita_assets | 
                 settlement + year,  
               cluster = ~region,  
               data = data)
summary(mod_sa, agg = "att")

iplot(list(mod_twfe, mod_sa), sep = 0.5, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2020)"), cex = 0.7)


################################################################################

# инвестиции в ОК за счет муниципалитета на душу с минимальным набором контролей
y_var <- "t8009001_c_pc" # интересная картинка
cov_vars <- c("log_build_flat", "log_new_housing", "catering_c_pc", "construction_c_pc",
              "retail_c_pc", "volume_electr_c_pc", "volume_manufact_c_pc", "doctors_per10",
              "living_space", "n_companies", "pop_work", "log_population", "log_wage",
              "workers", "t8006003", "pension_c")

data <- big_cities %>%    filter(group != "unexpected") %>%  
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(t8009001_c_pc ~ i(time_to_treat, treat, ref = -1) + 
                   log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                   retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                   living_space + n_companies + pop_work + log_population + log_wage +
                   workers + t8006003 + pension_c | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

mod_twfe_total = feols(t8009001_c_pc ~ treatment +       
                         log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                         retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                         living_space + n_companies + pop_work + log_population + log_wage +
                         workers + t8006003 + pension_c |                   
                         settlement + year,                                           
                       cluster = ~region,                                             
                       data = data)
summary(mod_twfe_total)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(t8009001_c_pc ~ sunab(first.treat, year) + 
                 log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                 retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                 living_space + n_companies + pop_work + log_population + log_wage +
                 workers + t8006003 + pension_c | 
                 settlement + year,  
               cluster = ~region,  
               data = data)
summary(mod_sa, agg = "att")

iplot(list(mod_twfe, mod_sa), sep = 0.5, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2020)"), cex = 0.7)


################################################################################

# инвестиции в ОК за счет муниципалитета на душу с минимальным набором контролей | competitive
y_var <- "t8009001_c_pc" # 5% + положительный эффект
cov_vars <- c("log_build_flat", "log_new_housing", "catering_c_pc", "construction_c_pc",
              "retail_c_pc", "volume_electr_c_pc", "volume_manufact_c_pc", "doctors_per10",
              "living_space", "n_companies", "pop_work", "log_population", "log_wage",
              "workers", "t8006003", "pension_c")

data <- big_cities %>%    
  filter(group != "unexpected") %>%  
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", "competitive", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(t8009001_c_pc ~ i(time_to_treat*(1-competitive), treat, ref = -1) + treatment +
                   log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                   retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                   living_space + n_companies + pop_work + log_population + log_wage +
                   workers + t8006003 + pension_c | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

mod_twfe_total = feols(t8009001_c_pc ~ i(treatment, competitive, 1) + treatment + 
                         log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                         retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                         living_space + n_companies + pop_work + log_population + log_wage +
                         workers + t8006003 + pension_c |                   
                         settlement + year,                                           
                       cluster = ~region,                                             
                       data = data)
summary(mod_twfe_total)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(t8009001_c_pc ~ sunab(first.treat*(1-competitive), year) + treatment +
                 log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                 retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                 living_space + n_companies + pop_work + log_population + log_wage +
                 workers + t8006003 + pension_c | 
                 settlement + year,  
               cluster = ~region,  
               data = data)
summary(mod_sa, agg = "att")

iplot(list(mod_twfe, mod_sa), sep = 0.5, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2020)"), cex = 0.7)


################################################################################

# инвестиции в ОК за счет муниципалитета на душу с фирмами и активами
y_var <- "t8009001_c_pc"
cov_vars <- c("log_build_flat", "log_new_housing", "catering_c_pc", "construction_c_pc",
              "retail_c_pc", "volume_electr_c_pc", "volume_manufact_c_pc", "doctors_per10",
              "living_space", "n_companies", "pop_work", "log_population", "log_wage",
              "workers", "t8006003", "pension_c", "share_profitable_firms", "log_per_capita_assets")

data <- big_cities %>%    
  filter(group != "unexpected") %>%  
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(t8009001_c_pc ~ i(time_to_treat, treat, ref = -1) + 
                   log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                   retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                   living_space + n_companies + pop_work + log_population + log_wage +
                   workers + t8006003 + pension_c + share_profitable_firms + log_per_capita_assets | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

mod_twfe_total = feols(t8009001_c_pc ~ treatment +       
                         log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                         retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                         living_space + n_companies + pop_work + log_population + log_wage +
                         workers + t8006003 + pension_c + share_profitable_firms + log_per_capita_assets |                   
                         settlement + year,                                           
                       cluster = ~region,                                             
                       data = data)
summary(mod_twfe_total)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(t8009001_c_pc ~ sunab(first.treat, year) + 
                 log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                 retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                 living_space + n_companies + pop_work + log_population + log_wage +
                 workers + t8006003 + pension_c + share_profitable_firms + log_per_capita_assets | 
                 settlement + year,  
               cluster = ~region,  
               data = data)
summary(mod_sa, agg = "att")

iplot(list(mod_twfe, mod_sa), sep = 0.5, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2020)"), cex = 0.7)


################################################################################

# инвестиции в ОК за счет муниципалитета на душу с фирмами
y_var <- "t8009001_c_pc"
cov_vars <- c("log_build_flat", "log_new_housing", "catering_c_pc", "construction_c_pc",
              "retail_c_pc", "volume_electr_c_pc", "volume_manufact_c_pc", "doctors_per10",
              "living_space", "n_companies", "pop_work", "log_population", "log_wage",
              "workers", "t8006003", "pension_c", "share_profitable_firms")

data <- big_cities %>%    filter(group != "unexpected") %>%  
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(t8009001_c_pc ~ i(time_to_treat, treat, ref = -1) + 
                   log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                   retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                   living_space + n_companies + pop_work + log_population + log_wage +
                   workers + t8006003 + pension_c + share_profitable_firms | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

mod_twfe_total = feols(t8009001_c_pc ~ treatment +       
                         log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                         retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                         living_space + n_companies + pop_work + log_population + log_wage +
                         workers + t8006003 + pension_c + share_profitable_firms |                   
                         settlement + year,                                           
                       cluster = ~region,                                             
                       data = data)
summary(mod_twfe_total)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(t8009001_c_pc ~ sunab(first.treat, year) + 
                 log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                 retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                 living_space + n_companies + pop_work + log_population + log_wage +
                 workers + t8006003 + pension_c + share_profitable_firms | 
                 settlement + year,  
               cluster = ~region,  
               data = data)
summary(mod_sa, agg = "att")

iplot(list(mod_twfe, mod_sa), sep = 0.5, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2020)"), cex = 0.7)


################################################################################

# работники культуры
y_var <- "t8016002" #5%
cov_vars <- c("build_flat", "catering_c", "construction_c", "doctors_per10", 
              "living_space", "n_companies", "pop_work", "log_population", 
              "retail_c", "log_wage", "workers", "t8006003")

data <- big_cities %>%    
  filter(group != "unexpected") %>%  
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(t8016002 ~ i(time_to_treat, treat, ref = -1) + 
                   build_flat + catering_c + construction_c + doctors_per10 + 
                   living_space + n_companies + pop_work + log_population + 
                   retail_c + log_wage + workers + t8006003 | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

mod_twfe_total = feols(t8016002 ~ treatment +       
                         build_flat + catering_c + construction_c + doctors_per10 + 
                         living_space + n_companies + pop_work + log_population + 
                         retail_c + log_wage + workers + t8006003 |                   
                         settlement + year,                                           
                       cluster = ~region,                                             
                       data = data)
summary(mod_twfe_total)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(t8016002 ~ sunab(first.treat, year) + 
                 build_flat + catering_c + construction_c + doctors_per10 + 
                 living_space + n_companies + pop_work + log_population + 
                 retail_c + log_wage + workers + t8006003 | 
                 settlement + year,  
               cluster = ~region,  
               data = data)
summary(mod_sa, agg = "att")

iplot(list(mod_twfe, mod_sa), sep = 0.5, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2021)"), cex = 0.7)


################################################################################

# работники культуры с минимальным набором контролей
y_var <- "t8016002" # 10 % 
cov_vars <- c("log_build_flat", "log_new_housing", "catering_c_pc", "construction_c_pc",
              "retail_c_pc", "volume_electr_c_pc", "volume_manufact_c_pc", "doctors_per10",
              "living_space", "n_companies", "pop_work", "log_population", "log_wage",
              "workers", "t8006003", "pension_c")

data <- big_cities %>%    
  filter(group != "unexpected") %>%  
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(t8016002 ~ i(time_to_treat, treat, ref = -1) + 
                   log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                   retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                   living_space + n_companies + pop_work + log_population + log_wage +
                   workers + t8006003 + pension_c | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

mod_twfe_total = feols(t8016002 ~ treatment +       
                         log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                         retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                         living_space + n_companies + pop_work + log_population + log_wage +
                         workers + t8006003 + pension_c |                   
                         settlement + year,                                           
                       cluster = ~region,                                             
                       data = data)
summary(mod_twfe_total)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(t8016002 ~ sunab(first.treat, year) + 
                 log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                 retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                 living_space + n_companies + pop_work + log_population + log_wage +
                 workers + t8006003 + pension_c | 
                 settlement + year,  
               cluster = ~region,  
               data = data)
summary(mod_sa, agg = "att")

iplot(list(mod_twfe, mod_sa), sep = 0.5, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2020)"), cex = 0.7)


################################################################################

# работники культуры с минимальным набором контролей | competitive
y_var <- "t8016002" #
cov_vars <- c("log_build_flat", "log_new_housing", "catering_c_pc", "construction_c_pc",
              "retail_c_pc", "volume_electr_c_pc", "volume_manufact_c_pc", "doctors_per10",
              "living_space", "n_companies", "pop_work", "log_population", "log_wage",
              "workers", "t8006003", "pension_c")

data <- big_cities %>%    
  filter(group != "unexpected") %>%  
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", "competitive", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(t8016002 ~ i(time_to_treat*(1-competitive), treat, ref = -1) + treatment +
                   log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                   retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                   living_space + n_companies + pop_work + log_population + log_wage +
                   workers + t8006003 + pension_c | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

mod_twfe_total = feols(t8016002 ~ i(treatment, competitive, 1) + treatment + 
                         log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                         retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                         living_space + n_companies + pop_work + log_population + log_wage +
                         workers + t8006003 + pension_c |                   
                         settlement + year,                                           
                       cluster = ~region,                                             
                       data = data)
summary(mod_twfe_total)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(t8016002 ~ sunab(first.treat*(1-competitive), year) + treatment +
                 log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                 retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                 living_space + n_companies + pop_work + log_population + log_wage +
                 workers + t8006003 + pension_c | 
                 settlement + year,  
               cluster = ~region,  
               data = data)
summary(mod_sa, agg = "att")

iplot(list(mod_twfe, mod_sa), sep = 0.5, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2020)"), cex = 0.7)


################################################################################

# доля работников культуры с минимальным набором контролей
y_var <- "share_culture_workers"
cov_vars <- c("log_build_flat", "log_new_housing", "catering_c_pc", "construction_c_pc",
              "retail_c_pc", "volume_electr_c_pc", "volume_manufact_c_pc", "doctors_per10",
              "living_space", "n_companies", "pop_work", "log_population", "log_wage",
              "workers", "t8006003", "pension_c")

data <- big_cities %>%    
  filter(group != "unexpected") %>%  
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(share_culture_workers ~ i(time_to_treat, treat, ref = -1) + 
                   log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                   retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                   living_space + n_companies + pop_work + log_population + log_wage +
                   workers + t8006003 + pension_c | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

mod_twfe_total = feols(share_culture_workers ~ treatment +       
                         log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                         retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                         living_space + n_companies + pop_work + log_population + log_wage +
                         workers + t8006003 + pension_c |                   
                         settlement + year,                                           
                       cluster = ~region,                                             
                       data = data)
summary(mod_twfe_total)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(share_culture_workers ~ sunab(first.treat, year) + 
                 log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                 retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                 living_space + n_companies + pop_work + log_population + log_wage +
                 workers + t8006003 + pension_c | 
                 settlement + year,  
               cluster = ~region,  
               data = data)
summary(mod_sa, agg = "att")

iplot(list(mod_twfe, mod_sa), sep = 0.5, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2020)"), cex = 0.7)


################################################################################

# доля работников культуры с минимальным набором контролей | competitive
y_var <- "share_culture_workers"
cov_vars <- c("log_build_flat", "log_new_housing", "catering_c_pc", "construction_c_pc",
              "retail_c_pc", "volume_electr_c_pc", "volume_manufact_c_pc", "doctors_per10",
              "living_space", "n_companies", "pop_work", "log_population", "log_wage",
              "workers", "t8006003", "pension_c")

data <- big_cities %>%    
  filter(group != "unexpected") %>%  
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", "competitive", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(share_culture_workers ~ i(time_to_treat*(1-competitive), treat, ref = -1) + treatment +
                   log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                   retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                   living_space + n_companies + pop_work + log_population + log_wage +
                   workers + t8006003 + pension_c | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

mod_twfe_total = feols(share_culture_workers ~ i(treatment, competitive, 1) + treatment + 
                         log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                         retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                         living_space + n_companies + pop_work + log_population + log_wage +
                         workers + t8006003 + pension_c |                   
                         settlement + year,                                           
                       cluster = ~region,                                             
                       data = data)
summary(mod_twfe_total)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(share_culture_workers ~ sunab(first.treat*(1-competitive), year) + treatment +
                 log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                 retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                 living_space + n_companies + pop_work + log_population + log_wage +
                 workers + t8006003 + pension_c | 
                 settlement + year,  
               cluster = ~region,  
               data = data)
summary(mod_sa, agg = "att")

iplot(list(mod_twfe, mod_sa), sep = 0.5, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2020)"), cex = 0.7)


################################################################################

# заменено водопроводной сети
y_var <- "t8008025"
cov_vars <- c("build_flat", "catering_c", "construction_c", "doctors_per10", 
              "living_space", "n_companies", "pop_work", "log_population", 
              "retail_c", "log_wage", "workers", "t8006003")

data <- big_cities %>%    
  filter(group != "unexpected") %>%  
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(t8008025 ~ i(time_to_treat, treat, ref = -1) + 
                   build_flat + catering_c + construction_c + doctors_per10 + 
                   living_space + n_companies + pop_work + log_population + 
                   retail_c + log_wage + workers + t8006003 | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

mod_twfe_total = feols(t8008025 ~ treatment +       
                         build_flat + catering_c + construction_c + doctors_per10 + 
                         living_space + n_companies + pop_work + log_population + 
                         retail_c + log_wage + workers + t8006003 |                   
                         settlement + year,                                           
                       cluster = ~region,                                             
                       data = data)
summary(mod_twfe_total)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(t8008025 ~ sunab(first.treat, year) + 
                 build_flat + catering_c + construction_c + doctors_per10 + 
                 living_space + n_companies + pop_work + log_population + 
                 retail_c + log_wage + workers + t8006003 | 
                 settlement + year,  
               cluster = ~region,  
               data = data)
summary(mod_sa, agg = "att")

iplot(list(mod_twfe, mod_sa), sep = 0.5, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2021)"), cex = 0.7)



################################################################################

# доля водопроводной сети нуждающейся в замене
y_var <- "t8008008_t8008007"
cov_vars <- c("build_flat", "catering_c", "construction_c", "doctors_per10", 
              "living_space", "n_companies", "pop_work", "log_population", 
              "retail_c", "log_wage", "workers", "t8006003")

data <- big_cities %>%    filter(group != "unexpected") %>%  
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(t8008008_t8008007 ~ i(time_to_treat, treat, ref = -1) + 
                   build_flat + catering_c + construction_c + doctors_per10 + 
                   living_space + n_companies + pop_work + log_population + 
                   retail_c + log_wage + workers + t8006003 | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

mod_twfe_total = feols(t8008008_t8008007 ~ treatment +       
                         build_flat + catering_c + construction_c + doctors_per10 + 
                         living_space + n_companies + pop_work + log_population + 
                         retail_c + log_wage + workers + t8006003 |                   
                         settlement + year,                                           
                       cluster = ~region,                                             
                       data = data)
summary(mod_twfe_total)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(t8008008_t8008007 ~ sunab(first.treat, year) + 
                 build_flat + catering_c + construction_c + doctors_per10 + 
                 living_space + n_companies + pop_work + log_population + 
                 retail_c + log_wage + workers + t8006003 | 
                 settlement + year,  
               cluster = ~region,  
               data = data)
summary(mod_sa, agg = "att")

iplot(list(mod_twfe, mod_sa), sep = 0.5, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2021)"), cex = 0.7)


################################################################################

# доля водопроводной сети нуждающейся в замене | competitive
y_var <- "t8008008_t8008007"
cov_vars <- c("build_flat", "catering_c", "construction_c", "doctors_per10", 
              "living_space", "n_companies", "pop_work", "log_population", 
              "retail_c", "log_wage", "workers", "t8006003")

data <- big_cities %>%    filter(group != "unexpected") %>%  
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", "competitive", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(t8008008_t8008007 ~ i(time_to_treat*(1-competitive), treat, ref = -1) + treatment +
                   build_flat + catering_c + construction_c + doctors_per10 + 
                   living_space + n_companies + pop_work + log_population + 
                   retail_c + log_wage + workers + t8006003 | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

mod_twfe_total = feols(t8008008_t8008007 ~ i(treatment, competitive, 1) + treatment + 
                         build_flat + catering_c + construction_c + doctors_per10 + 
                         living_space + n_companies + pop_work + log_population + 
                         retail_c + log_wage + workers + t8006003 |                   
                         settlement + year,                                           
                       cluster = ~region,                                             
                       data = data)
summary(mod_twfe_total)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(t8008008_t8008007 ~ sunab(first.treat*(1-competitive), year, ref.p = -1) + treatment +
                 build_flat + catering_c + construction_c + doctors_per10 + 
                 living_space + n_companies + pop_work + log_population + 
                 retail_c + log_wage + workers + t8006003 | 
                 settlement + year,  
               cluster = ~region,  
               data = data)
summary(mod_sa, agg = "att")

iplot(list(mod_twfe, mod_sa), sep = 0.5, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2021)"), cex = 0.7)


################################################################################

# доля отремонтированной водопроводной сети
y_var <- "t8008025_t8008008"
cov_vars <- c("build_flat", "catering_c", "construction_c", "doctors_per10", 
              "living_space", "n_companies", "pop_work", "log_population", 
              "retail_c", "log_wage", "workers", "t8006003")

data <- big_cities %>%    
  filter(group != "unexpected") %>%  
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(t8008025_t8008008 ~ i(time_to_treat, treat, ref = -1) + 
                   build_flat + catering_c + construction_c + doctors_per10 + 
                   living_space + n_companies + pop_work + log_population + 
                   retail_c + log_wage + workers + t8006003 | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

mod_twfe_total = feols(t8008025_t8008008 ~ treatment +       
                         build_flat + catering_c + construction_c + doctors_per10 + 
                         living_space + n_companies + pop_work + log_population + 
                         retail_c + log_wage + workers + t8006003 |                   
                         settlement + year,                                           
                       cluster = ~region,                                             
                       data = data)
summary(mod_twfe_total)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(t8008025_t8008008 ~ sunab(first.treat, year) + 
                 build_flat + catering_c + construction_c + doctors_per10 + 
                 living_space + n_companies + pop_work + log_population + 
                 retail_c + log_wage + workers + t8006003 | 
                 settlement + year,  
               cluster = ~region,  
               data = data)
summary(mod_sa, agg = "att")

iplot(list(mod_twfe, mod_sa), sep = 0.5, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2021)"), cex = 0.7)


################################################################################

# доля освещенных частей улиц
y_var <- "t8006003_t8006007"
cov_vars <- c("build_flat", "catering_c", "construction_c", "doctors_per10", 
              "living_space", "n_companies", "pop_work", "log_population", 
              "retail_c", "log_wage", "workers", "t8006003")

data <- big_cities %>%    filter(group != "unexpected") %>%  
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(t8006003_t8006007 ~ i(time_to_treat, treat, ref = -1) + 
                   build_flat + catering_c + construction_c + doctors_per10 + 
                   living_space + n_companies + pop_work + log_population + 
                   retail_c + log_wage + workers + t8006003 | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

mod_twfe_total = feols(t8006003_t8006007 ~ treatment +       
                         build_flat + catering_c + construction_c + doctors_per10 + 
                         living_space + n_companies + pop_work + log_population + 
                         retail_c + log_wage + workers + t8006003 |                   
                         settlement + year,                                           
                       cluster = ~region,                                             
                       data = data)
summary(mod_twfe_total)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(t8006003_t8006007 ~ sunab(first.treat, year) + 
                 build_flat + catering_c + construction_c + doctors_per10 + 
                 living_space + n_companies + pop_work + log_population + 
                 retail_c + log_wage + workers + t8006003 | 
                 settlement + year,  
               cluster = ~region,  
               data = data)
summary(mod_sa, agg = "att")

iplot(list(mod_twfe, mod_sa), sep = 0.5, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2021)"), cex = 0.7)


################################################################################

# все доходы
y_var <- "t8013001_1_c"
cov_vars <- c("build_flat", "catering_c", "construction_c", "doctors_per10", 
              "living_space", "n_companies", "pop_work", "log_population", 
              "retail_c", "log_wage", "workers", "t8006003", "share_profitable_firms")

data <- big_cities %>%    
  filter(group != "unexpected") %>%  
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(t8013001_1_c ~ i(time_to_treat, treat, ref = -1) + 
                   build_flat + catering_c + construction_c + doctors_per10 + 
                   living_space + n_companies + pop_work + log_population + 
                   retail_c + log_wage + workers + t8006003 | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

mod_twfe_total = feols(t8013001_1_c ~ treatment +       
                         build_flat + catering_c + construction_c + doctors_per10 + 
                         living_space + n_companies + pop_work + log_population + 
                         retail_c + log_wage + workers + t8006003 |                   
                         settlement + year,                                           
                       cluster = ~region,                                             
                       data = data)
summary(mod_twfe_total)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(t8013001_1_c ~ sunab(first.treat, year) + 
                 build_flat + catering_c + construction_c + doctors_per10 + 
                 living_space + n_companies + pop_work + log_population + 
                 retail_c + log_wage + workers + t8006003 | 
                 settlement + year,  
               cluster = ~region,  
               data = data)
summary(mod_sa, agg = "att")

iplot(list(mod_twfe, mod_sa), sep = 0.5, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2021)"), cex = 0.7)


################################################################################

# все доходы на душу с минимальным набором контролей и фирмами и активами
y_var <- "t8013001_1_c_pc"
cov_vars <- c("log_build_flat", "log_new_housing", "catering_c_pc", "construction_c_pc",
              "retail_c_pc", "volume_electr_c_pc", "volume_manufact_c_pc", "doctors_per10",
              "living_space", "n_companies", "pop_work", "log_population", "log_wage",
              "workers", "t8006003", "pension_c", "share_profitable_firms", "log_per_capita_assets")

data <- big_cities %>%    
  filter(group != "unexpected") %>%  
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(t8013001_1_c_pc ~ i(time_to_treat, treat, ref = -1) + 
                   log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                   retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                   living_space + n_companies + pop_work + log_population + log_wage +
                   workers + t8006003 + pension_c + share_profitable_firms + log_per_capita_assets | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

mod_twfe_total = feols(t8013001_1_c_pc ~ treatment +       
                         log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                         retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                         living_space + n_companies + pop_work + log_population + log_wage +
                         workers + t8006003 + pension_c + share_profitable_firms + log_per_capita_assets |                   
                         settlement + year,                                           
                       cluster = ~region,                                             
                       data = data)
summary(mod_twfe_total)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(t8013001_1_c_pc ~ sunab(first.treat, year) + 
                 log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                 retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                 living_space + n_companies + pop_work + log_population + log_wage +
                 workers + t8006003 + pension_c + share_profitable_firms + log_per_capita_assets | 
                 settlement + year,  
               cluster = ~region,  
               data = data)
summary(mod_sa, agg = "att")

iplot(list(mod_twfe, mod_sa), sep = 0.5, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2020)"), cex = 0.7)


################################################################################

# все доходы на душу с минимальным набором контролей
y_var <- "t8013001_1_c_pc"
cov_vars <- c("log_build_flat", "log_new_housing", "catering_c_pc", "construction_c_pc",
              "retail_c_pc", "volume_electr_c_pc", "volume_manufact_c_pc", "doctors_per10",
              "living_space", "n_companies", "pop_work", "log_population", "log_wage",
              "workers", "t8006003", "pension_c")

data <- big_cities %>%    
  filter(group != "unexpected") %>%  
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(t8013001_1_c_pc ~ i(time_to_treat, treat, ref = -1) + 
                   log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                   retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                   living_space + n_companies + pop_work + log_population + log_wage +
                   workers + t8006003 + pension_c | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

mod_twfe_total = feols(t8013001_1_c_pc ~ treatment +       
                         log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                         retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                         living_space + n_companies + pop_work + log_population + log_wage +
                         workers + t8006003 + pension_c |                   
                         settlement + year,                                           
                       cluster = ~region,                                             
                       data = data)
summary(mod_twfe_total)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(t8013001_1_c_pc ~ sunab(first.treat, year) + 
                 log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                 retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                 living_space + n_companies + pop_work + log_population + log_wage +
                 workers + t8006003 + pension_c | 
                 settlement + year,  
               cluster = ~region,  
               data = data)
summary(mod_sa, agg = "att")

iplot(list(mod_twfe, mod_sa), sep = 0.5, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2020)"), cex = 0.7)


################################################################################

# все доходы на душу с минимальным набором контролей | competitive
y_var <- "t8013001_1_c_pc" # 1 %
cov_vars <- c("log_build_flat", "log_new_housing", "catering_c_pc", "construction_c_pc",
              "retail_c_pc", "volume_electr_c_pc", "volume_manufact_c_pc", "doctors_per10",
              "living_space", "n_companies", "pop_work", "log_population", "log_wage",
              "workers", "t8006003", "pension_c")

data <- big_cities %>%    
  filter(group != "unexpected") %>%  
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", "competitive", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(t8013001_1_c_pc ~ i(time_to_treat*(1-competitive), treat, ref = -1) + treatment +
                   log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                   retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                   living_space + n_companies + pop_work + log_population + log_wage +
                   workers + t8006003 + pension_c | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

mod_twfe_total = feols(t8013001_1_c_pc ~ i(treatment, competitive, 1) + treatment + 
                         log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                         retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                         living_space + n_companies + pop_work + log_population + log_wage +
                         workers + t8006003 + pension_c |                   
                         settlement + year,                                           
                       cluster = ~region,                                             
                       data = data)
summary(mod_twfe_total)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(t8013001_1_c_pc ~ sunab(first.treat*(1-competitive), year) + treatment +
                 log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                 retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                 living_space + n_companies + pop_work + log_population + log_wage +
                 workers + t8006003 + pension_c | 
                 settlement + year,  
               cluster = ~region,  
               data = data)
summary(mod_sa, agg = "att")

iplot(list(mod_twfe, mod_sa), sep = 0.5, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2020)"), cex = 0.7)


################################################################################

# собственные доходы
y_var <- "t8013001_89_c"
cov_vars <- c("build_flat", "catering_c", "construction_c", "doctors_per10", 
              "living_space", "n_companies", "pop_work", "log_population", 
              "retail_c", "log_wage", "workers", "t8006003", "share_profitable_firms")

data <- big_cities %>%    
  filter(group != "unexpected") %>%  
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(t8013001_89_c ~ i(time_to_treat, treat, ref = -1) + 
                   build_flat + catering_c + construction_c + doctors_per10 + 
                   living_space + n_companies + pop_work + log_population + 
                   retail_c + log_wage + workers + t8006003 | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

mod_twfe_total = feols(t8013001_89_c ~ treatment +       
                         build_flat + catering_c + construction_c + doctors_per10 + 
                         living_space + n_companies + pop_work + log_population + 
                         retail_c + log_wage + workers + t8006003 |                   
                         settlement + year,                                           
                       cluster = ~region,                                             
                       data = data)
summary(mod_twfe_total)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(t8013001_89_c ~ sunab(first.treat, year) + 
                 build_flat + catering_c + construction_c + doctors_per10 + 
                 living_space + n_companies + pop_work + log_population + 
                 retail_c + log_wage + workers + t8006003 | 
                 settlement + year,  
               cluster = ~region,  
               data = data)
summary(mod_sa, agg = "att")

iplot(list(mod_twfe, mod_sa), sep = 0.5, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2021)"), cex = 0.7)

################################################################################

# собственные доходы на душу с минимальным набором контролей
y_var <- "t8013001_89_c_pc" # 10%
cov_vars <- c("log_build_flat", "log_new_housing", "catering_c_pc", "construction_c_pc",
              "retail_c_pc", "volume_electr_c_pc", "volume_manufact_c_pc", "doctors_per10",
              "living_space", "n_companies", "pop_work", "log_population", "log_wage",
              "workers", "t8006003", "pension_c")

data <- big_cities %>%    
  filter(group != "unexpected") %>%  
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(t8013001_89_c_pc ~ i(time_to_treat, treat, ref = -1) + 
                   log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                   retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                   living_space + n_companies + pop_work + log_population + log_wage +
                   workers + t8006003 + pension_c | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

mod_twfe_total = feols(t8013001_89_c_pc ~ treatment +       
                         log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                         retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                         living_space + n_companies + pop_work + log_population + log_wage +
                         workers + t8006003 + pension_c |                   
                         settlement + year,                                           
                       cluster = ~region,                                             
                       data = data)
summary(mod_twfe_total)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(t8013001_89_c_pc ~ sunab(first.treat, year) + 
                 log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                 retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                 living_space + n_companies + pop_work + log_population + log_wage +
                 workers + t8006003 + pension_c | 
                 settlement + year,  
               cluster = ~region,  
               data = data)
summary(mod_sa, agg = "att")

iplot(list(mod_twfe, mod_sa), sep = 0.5, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2020)"), cex = 0.7)


################################################################################

# собственные доходы на душу с минимальным набором контролей | competitive
y_var <- "t8013001_89_c_pc" # 1%
cov_vars <- c("log_build_flat", "log_new_housing", "catering_c_pc", "construction_c_pc",
              "retail_c_pc", "volume_electr_c_pc", "volume_manufact_c_pc", "doctors_per10",
              "living_space", "n_companies", "pop_work", "log_population", "log_wage",
              "workers", "t8006003", "pension_c")

data <- big_cities %>%    
  filter(group != "unexpected") %>%  
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", "competitive", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(t8013001_89_c_pc ~ i(time_to_treat*(1-competitive), treat, ref = -1) + treatment +
                   log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                   retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                   living_space + n_companies + pop_work + log_population + log_wage +
                   workers + t8006003 + pension_c | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

mod_twfe_total = feols(t8013001_89_c_pc ~ i(treatment, competitive, 1) + treatment + 
                         log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                         retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                         living_space + n_companies + pop_work + log_population + log_wage +
                         workers + t8006003 + pension_c |                   
                         settlement + year,                                           
                       cluster = ~region,                                             
                       data = data)
summary(mod_twfe_total)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(t8013001_89_c_pc ~ sunab(first.treat*(1-competitive), year) + treatment +
                 log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                 retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                 living_space + n_companies + pop_work + log_population + log_wage +
                 workers + t8006003 + pension_c | 
                 settlement + year,  
               cluster = ~region,  
               data = data)
summary(mod_sa, agg = "att")

iplot(list(mod_twfe, mod_sa), sep = 0.5, ref.line = -1, ci_level = 0.99,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2020)"), cex = 0.7)


################################################################################

# собственные доходы на душу с минимальным набором контролей и фирмами и активами
y_var <- "t8013001_89_c_pc"
cov_vars <- c("log_build_flat", "log_new_housing", "catering_c_pc", "construction_c_pc",
              "retail_c_pc", "volume_electr_c_pc", "volume_manufact_c_pc", "doctors_per10",
              "living_space", "n_companies", "pop_work", "log_population", "log_wage",
              "workers", "t8006003", "pension_c", "share_profitable_mun_firms", "log_per_capita_assets")

data <- big_cities %>%    
  filter(group != "unexpected") %>%  
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(t8013001_89_c_pc ~ i(time_to_treat, treat, ref = -1) + 
                   log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                   retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                   living_space + n_companies + pop_work + log_population + log_wage +
                   workers + t8006003 + pension_c + share_profitable_mun_firms + log_per_capita_assets | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

mod_twfe_total = feols(t8013001_89_c_pc ~ treatment +       
                         log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                         retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                         living_space + n_companies + pop_work + log_population + log_wage +
                         workers + t8006003 + pension_c + share_profitable_mun_firms + log_per_capita_assets |                   
                         settlement + year,                                           
                       cluster = ~region,                                             
                       data = data)
summary(mod_twfe_total)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(t8013001_89_c_pc ~ sunab(first.treat, year) + 
                 log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                 retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                 living_space + n_companies + pop_work + log_population + log_wage +
                 workers + t8006003 + pension_c + share_profitable_mun_firms + log_per_capita_assets | 
                 settlement + year,  
               cluster = ~region,  
               data = data)
summary(mod_sa, agg = "att")

iplot(list(mod_twfe, mod_sa), sep = 0.5, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2020)"), cex = 0.7)


################################################################################

# доля собственных доходов
y_var <- "t8013001_89_t8013001_1" # 5%
cov_vars <- c("build_flat", "catering_c", "construction_c", "doctors_per10", 
              "living_space", "n_companies", "pop_work", "log_population", 
              "retail_c", "log_wage", "workers", "t8006003", 
              "share_profitable_firms", "volume_electr_c", "volume_manufact_c")

data <- big_cities %>%    
  filter(group != "unexpected") %>%  
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(t8013001_89_t8013001_1 ~ i(time_to_treat, treat, ref = -1) + 
                   build_flat + catering_c + construction_c + doctors_per10 + 
                   living_space + n_companies + pop_work + log_population + 
                   retail_c + log_wage + workers + t8006003 + 
                   share_profitable_firms + volume_electr_c + volume_manufact_c| 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

mod_twfe_total = feols(t8013001_89_t8013001_1 ~ treatment +       
                         build_flat + catering_c + construction_c + doctors_per10 + 
                         living_space + n_companies + pop_work + log_population + 
                         retail_c + log_wage + workers + t8006003 +
                         share_profitable_firms + volume_electr_c + volume_manufact_c|                   
                         settlement + year,                                           
                       cluster = ~region,                                             
                       data = data)
summary(mod_twfe_total)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(t8013001_89_t8013001_1 ~ sunab(first.treat, year) + 
                 build_flat + catering_c + construction_c + doctors_per10 + 
                 living_space + n_companies + pop_work + log_population + 
                 retail_c + log_wage + workers + t8006003 +
                 share_profitable_firms + volume_electr_c + volume_manufact_c| 
                 settlement + year,  
               cluster = ~region,  
               data = data)
summary(mod_sa, agg = "att")

iplot(list(mod_twfe, mod_sa), sep = 0.5, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2021)"), cex = 0.7)

################################################################################

# доля собственных доходов с минимальным набором контролей
y_var <- "t8013001_89_t8013001_1"
cov_vars <- c("log_build_flat", "log_new_housing", "catering_c_pc", "construction_c_pc",
              "retail_c_pc", "volume_electr_c_pc", "volume_manufact_c_pc", "doctors_per10",
              "living_space", "n_companies", "pop_work", "log_population", "log_wage",
              "workers", "t8006003", "pension_c")

data <- big_cities %>%    
  filter(group != "unexpected") %>%  
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(t8013001_89_t8013001_1 ~ i(time_to_treat, treat, ref = -1) + 
                   log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                   retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                   living_space + n_companies + pop_work + log_population + log_wage +
                   workers + t8006003 + pension_c | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

mod_twfe_total = feols(t8013001_89_t8013001_1 ~ treatment +       
                         log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                         retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                         living_space + n_companies + pop_work + log_population + log_wage +
                         workers + t8006003 + pension_c |                   
                         settlement + year,                                           
                       cluster = ~region,                                             
                       data = data)
summary(mod_twfe_total)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(t8013001_89_t8013001_1 ~ sunab(first.treat, year) + 
                 log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                 retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                 living_space + n_companies + pop_work + log_population + log_wage +
                 workers + t8006003 + pension_c | 
                 settlement + year,  
               cluster = ~region,  
               data = data)
summary(mod_sa, agg = "att")

iplot(list(mod_twfe, mod_sa), sep = 0.5, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2020)"), cex = 0.7)


################################################################################

# доля собственных доходов с минимальным набором контролей | competitive
y_var <- "t8013001_89_t8013001_1" # 1%, отрицательный эффект
cov_vars <- c("log_build_flat", "log_new_housing", "catering_c_pc", "construction_c_pc",
              "retail_c_pc", "volume_electr_c_pc", "volume_manufact_c_pc", "doctors_per10",
              "living_space", "n_companies", "pop_work", "log_population", "log_wage",
              "workers", "t8006003", "pension_c")

data <- big_cities %>%    
  filter(group != "unexpected") %>%  
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", "competitive", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(t8013001_89_t8013001_1 ~ i(time_to_treat*(1-competitive), treat, ref = -1) + treatment +
                   log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                   retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                   living_space + n_companies + pop_work + log_population + log_wage +
                   workers + t8006003 + pension_c | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

mod_twfe_total = feols(t8013001_89_t8013001_1 ~ i(treatment, competitive, 1) + treatment + 
                         log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                         retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                         living_space + n_companies + pop_work + log_population + log_wage +
                         workers + t8006003 + pension_c |                   
                         settlement + year,                                           
                       cluster = ~region,                                             
                       data = data)
summary(mod_twfe_total)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(t8013001_89_t8013001_1 ~ sunab(first.treat*(1-competitive), year) + treatment +
                 log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                 retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                 living_space + n_companies + pop_work + log_population + log_wage +
                 workers + t8006003 + pension_c | 
                 settlement + year,  
               cluster = ~region,  
               data = data)
summary(mod_sa, agg = "att")

iplot(list(mod_twfe, mod_sa), sep = 0.5, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2020)"), cex = 0.7)


################################################################################

# доля собственных доходов с минимальным набором контролей | competitive (competitive)
y_var <- "t8013001_89_t8013001_1" 
cov_vars <- c("log_build_flat", "log_new_housing", "catering_c_pc", "construction_c_pc",
              "retail_c_pc", "volume_electr_c_pc", "volume_manufact_c_pc", "doctors_per10",
              "living_space", "n_companies", "pop_work", "log_population", "log_wage",
              "workers", "t8006003", "pension_c")

data <- big_cities %>%    
  filter(group != "unexpected") %>%  
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", "competitive", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(t8013001_89_t8013001_1 ~ i(time_to_treat*(competitive), treat, ref = -1) + treatment +
                   log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                   retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                   living_space + n_companies + pop_work + log_population + log_wage +
                   workers + t8006003 + pension_c | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

mod_twfe_total = feols(t8013001_89_t8013001_1 ~ i(treatment, competitive, 0) + treatment + 
                         log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                         retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                         living_space + n_companies + pop_work + log_population + log_wage +
                         workers + t8006003 + pension_c |                   
                         settlement + year,                                           
                       cluster = ~region,                                             
                       data = data)
summary(mod_twfe_total)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(t8013001_89_t8013001_1 ~ sunab(first.treat*(competitive), year) + treatment +
                 log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                 retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                 living_space + n_companies + pop_work + log_population + log_wage +
                 workers + t8006003 + pension_c | 
                 settlement + year,  
               cluster = ~region,  
               data = data)
summary(mod_sa, agg = "att")

iplot(list(mod_twfe, mod_sa), sep = 0.5, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2020)"), cex = 0.7)


################################################################################

# доля собственных доходов с минимальным набором контролей и фирмами и активами
y_var <- "t8013001_89_t8013001_1" # 5%, интересное направление эффекта + 2 метода сильно расходятся в event study
cov_vars <- c("log_build_flat", "log_new_housing", "catering_c_pc", "construction_c_pc",
              "retail_c_pc", "volume_electr_c_pc", "volume_manufact_c_pc", "doctors_per10",
              "living_space", "n_companies", "pop_work", "log_population", "log_wage",
              "workers", "t8006003", "pension_c", "share_profitable_mun_firms", 
              "log_per_capita_assets", "share_profitable_firms")

data <- big_cities %>%    
  filter(group != "unexpected") %>%  
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(t8013001_89_t8013001_1 ~ i(time_to_treat, treat, ref = -1) + 
                   log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                   retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                   living_space + n_companies + pop_work + log_population + log_wage +
                   workers + t8006003 + pension_c + share_profitable_mun_firms + 
                   log_per_capita_assets + share_profitable_firms | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

mod_twfe_total = feols(t8013001_89_t8013001_1 ~ treatment +       
                         log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                         retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                         living_space + n_companies + pop_work + log_population + log_wage +
                         workers + t8006003 + pension_c + share_profitable_mun_firms + 
                         log_per_capita_assets + share_profitable_firms |                   
                         settlement + year,                                           
                       cluster = ~region,                                             
                       data = data)
summary(mod_twfe_total)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(t8013001_89_t8013001_1 ~ sunab(first.treat, year) + 
                 log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                 retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                 living_space + n_companies + pop_work + log_population + log_wage +
                 workers + t8006003 + pension_c + share_profitable_mun_firms + 
                 log_per_capita_assets + share_profitable_firms | 
                 settlement + year,  
               cluster = ~region,  
               data = data)
summary(mod_sa, agg = "att")

iplot(list(mod_twfe, mod_sa), sep = 0.5, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2020)"), cex = 0.7)


################################################################################

# доля собственных доходов с минимальным набором контролей и фирмами и активами | competitive
y_var <- "t8013001_89_t8013001_1"
cov_vars <- c("log_build_flat", "log_new_housing", "catering_c_pc", "construction_c_pc",
              "retail_c_pc", "volume_electr_c_pc", "volume_manufact_c_pc", "doctors_per10",
              "living_space", "n_companies", "pop_work", "log_population", "log_wage",
              "workers", "t8006003", "pension_c", "share_profitable_mun_firms", 
              "log_per_capita_assets", "share_profitable_firms", "competitive")

data <- big_cities %>%    
  filter(group != "unexpected") %>%  
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(t8013001_89_t8013001_1 ~ i(time_to_treat*(1-competitive), treat, ref = -1) + treatment + 
                   log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                   retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                   living_space + n_companies + pop_work + log_population + log_wage +
                   workers + t8006003 + pension_c + share_profitable_mun_firms + 
                   log_per_capita_assets + share_profitable_firms | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

mod_twfe_total = feols(t8013001_89_t8013001_1 ~ i(treatment, competitive, 1) + treatment + 
                         log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                         retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                         living_space + n_companies + pop_work + log_population + log_wage +
                         workers + t8006003 + pension_c + share_profitable_mun_firms + 
                         log_per_capita_assets + share_profitable_firms |                   
                         settlement + year,                                           
                       cluster = ~region,                                             
                       data = data)
summary(mod_twfe_total)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(t8013001_89_t8013001_1 ~ sunab(first.treat*(1-competitive), year) + treatment +
                 log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                 retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                 living_space + n_companies + pop_work + log_population + log_wage +
                 workers + t8006003 + pension_c + share_profitable_mun_firms + 
                 log_per_capita_assets + share_profitable_firms | 
                 settlement + year,  
               cluster = ~region,  
               data = data)
summary(mod_sa, agg = "att")

iplot(list(mod_twfe, mod_sa), sep = 0.5, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2020)"), cex = 0.7)


################################################################################

# безвозмездные поступления
y_var <- "t8013001_34_c"
cov_vars <- c("build_flat", "catering_c", "construction_c", "doctors_per10", 
              "living_space", "n_companies", "pop_work", "log_population", 
              "retail_c", "log_wage", "workers", "t8006003", 
              "share_profitable_firms", "volume_electr_c", "volume_manufact_c")

data <- big_cities %>%    filter(group != "unexpected") %>%  
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(t8013001_34_c ~ i(time_to_treat, treat, ref = -1) + 
                   build_flat + catering_c + construction_c + doctors_per10 + 
                   living_space + n_companies + pop_work + log_population + 
                   retail_c + log_wage + workers + t8006003 +
                   share_profitable_firms + volume_electr_c + volume_manufact_c| 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

mod_twfe_total = feols(t8013001_34_c ~ treatment +       
                         build_flat + catering_c + construction_c + doctors_per10 + 
                         living_space + n_companies + pop_work + log_population + 
                         retail_c + log_wage + workers + t8006003 +
                         share_profitable_firms + volume_electr_c + volume_manufact_c|                   
                         settlement + year,                                           
                       cluster = ~region,                                             
                       data = data)
summary(mod_twfe_total)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(t8013001_34_c ~ sunab(first.treat, year) + 
                 build_flat + catering_c + construction_c + doctors_per10 + 
                 living_space + n_companies + pop_work + log_population + 
                 retail_c + log_wage + workers + t8006003 +
                 share_profitable_firms + volume_electr_c + volume_manufact_c | 
                 settlement + year,  
               cluster = ~region,  
               data = data)
summary(mod_sa, agg = "att")

iplot(list(mod_twfe, mod_sa), sep = 0.5, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2021)"), cex = 0.7)

################################################################################

# безвозмездные поступления на душу с минимальным набором контролей
y_var <- "t8013001_34_c_pc"
cov_vars <- c("log_build_flat", "log_new_housing", "catering_c_pc", "construction_c_pc",
              "retail_c_pc", "volume_electr_c_pc", "volume_manufact_c_pc", "doctors_per10",
              "living_space", "n_companies", "pop_work", "log_population", "log_wage",
              "workers", "t8006003", "pension_c")

data <- big_cities %>%    
  filter(group != "unexpected") %>%  
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(t8013001_34_c_pc ~ i(time_to_treat, treat, ref = -1) + 
                   log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                   retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                   living_space + n_companies + pop_work + log_population + log_wage +
                   workers + t8006003 + pension_c | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

mod_twfe_total = feols(t8013001_34_c_pc ~ treatment +       
                         log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                         retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                         living_space + n_companies + pop_work + log_population + log_wage +
                         workers + t8006003 + pension_c |                   
                         settlement + year,                                           
                       cluster = ~region,                                             
                       data = data)
summary(mod_twfe_total)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(t8013001_34_c_pc ~ sunab(first.treat, year) + 
                 log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                 retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                 living_space + n_companies + pop_work + log_population + log_wage +
                 workers + t8006003 + pension_c | 
                 settlement + year,  
               cluster = ~region,  
               data = data)
summary(mod_sa, agg = "att")

iplot(list(mod_twfe, mod_sa), sep = 0.5, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2020)"), cex = 0.7)


################################################################################

# безвозмездные поступления на душу с минимальным набором контролей | competitive
y_var <- "t8013001_34_c_pc"
cov_vars <- c("log_build_flat", "log_new_housing", "catering_c_pc", "construction_c_pc",
              "retail_c_pc", "volume_electr_c_pc", "volume_manufact_c_pc", "doctors_per10",
              "living_space", "n_companies", "pop_work", "log_population", "log_wage",
              "workers", "t8006003", "pension_c")

data <- big_cities %>%    
  filter(group != "unexpected") %>%  
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", "competitive", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(t8013001_34_c_pc ~ i(time_to_treat*(1-competitive), treat, ref = -1) + treatment +
                   log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                   retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                   living_space + n_companies + pop_work + log_population + log_wage +
                   workers + t8006003 + pension_c | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

mod_twfe_total = feols(t8013001_34_c_pc ~ i(treatment, competitive, 1) + treatment + 
                         log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                         retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                         living_space + n_companies + pop_work + log_population + log_wage +
                         workers + t8006003 + pension_c |                   
                         settlement + year,                                           
                       cluster = ~region,                                             
                       data = data)
summary(mod_twfe_total)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(t8013001_34_c_pc ~ sunab(first.treat*(1-competitive), year) + treatment +
                 log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                 retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                 living_space + n_companies + pop_work + log_population + log_wage +
                 workers + t8006003 + pension_c | 
                 settlement + year,  
               cluster = ~region,  
               data = data)
summary(mod_sa, agg = "att")

iplot(list(mod_twfe, mod_sa), sep = 0.5, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2020)"), cex = 0.7)


################################################################################

# безвозмездные поступления на душу с активами и фирмами
y_var <- "t8013001_34_c_pc"
cov_vars <- c("log_build_flat", "log_new_housing", "catering_c_pc", "construction_c_pc",
              "retail_c_pc", "volume_electr_c_pc", "volume_manufact_c_pc", "doctors_per10",
              "living_space", "n_companies", "pop_work", "log_population", "log_wage",
              "workers", "t8006003", "pension_c", "share_profitable_mun_firms", 
              "log_per_capita_assets", "share_profitable_firms")

data <- big_cities %>%    filter(group != "unexpected") %>%  
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(t8013001_34_c_pc ~ i(time_to_treat, treat, ref = -1) + 
                   log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                   retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                   living_space + n_companies + pop_work + log_population + log_wage +
                   workers + t8006003 + pension_c + share_profitable_mun_firms + 
                   log_per_capita_assets + share_profitable_firms | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

mod_twfe_total = feols(t8013001_34_c_pc ~ treatment +       
                         log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                         retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                         living_space + n_companies + pop_work + log_population + log_wage +
                         workers + t8006003 + pension_c + share_profitable_mun_firms + 
                         log_per_capita_assets + share_profitable_firms |                   
                         settlement + year,                                           
                       cluster = ~region,                                             
                       data = data)
summary(mod_twfe_total)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(t8013001_34_c_pc ~ sunab(first.treat, year) + 
                 log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                 retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                 living_space + n_companies + pop_work + log_population + log_wage +
                 workers + t8006003 + pension_c + share_profitable_mun_firms + 
                 log_per_capita_assets + share_profitable_firms | 
                 settlement + year,  
               cluster = ~region,  
               data = data)
summary(mod_sa, agg = "att")

iplot(list(mod_twfe, mod_sa), sep = 0.5, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2020)"), cex = 0.7)


################################################################################

# доля безвозмездных поступлений с минимальным набором контролей
y_var <- "t8013001_34_t8013001_1"
cov_vars <- c("log_build_flat", "log_new_housing", "catering_c_pc", "construction_c_pc",
              "retail_c_pc", "volume_electr_c_pc", "volume_manufact_c_pc", "doctors_per10",
              "living_space", "n_companies", "pop_work", "log_population", "log_wage",
              "workers", "t8006003", "pension_c")

data <- big_cities %>%    filter(group != "unexpected") %>%  
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(t8013001_34_t8013001_1 ~ i(time_to_treat, treat, ref = -1) + 
                   log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                   retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                   living_space + n_companies + pop_work + log_population + log_wage +
                   workers + t8006003 + pension_c | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

mod_twfe_total = feols(t8013001_34_t8013001_1 ~ treatment +       
                         log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                         retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                         living_space + n_companies + pop_work + log_population + log_wage +
                         workers + t8006003 + pension_c |                   
                         settlement + year,                                           
                       cluster = ~region,                                             
                       data = data)
summary(mod_twfe_total)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(t8013001_34_t8013001_1 ~ sunab(first.treat, year) + 
                 log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                 retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                 living_space + n_companies + pop_work + log_population + log_wage +
                 workers + t8006003 + pension_c | 
                 settlement + year,  
               cluster = ~region,  
               data = data)
summary(mod_sa, agg = "att")

iplot(list(mod_twfe, mod_sa), sep = 0.5, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2020)"), cex = 0.7)


################################################################################

# доля безвозмездных поступлений с активами и фирмами
y_var <- "t8013001_34_t8013001_1"
cov_vars <- c("log_build_flat", "log_new_housing", "catering_c_pc", "construction_c_pc",
              "retail_c_pc", "volume_electr_c_pc", "volume_manufact_c_pc", "doctors_per10",
              "living_space", "n_companies", "pop_work", "log_population", "log_wage",
              "workers", "t8006003", "pension_c", "share_profitable_mun_firms", 
              "log_per_capita_assets", "share_profitable_firms")

data <- big_cities %>%    filter(group != "unexpected") %>%  
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(t8013001_34_t8013001_1 ~ i(time_to_treat, treat, ref = -1) + 
                   log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                   retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                   living_space + n_companies + pop_work + log_population + log_wage +
                   workers + t8006003 + pension_c + share_profitable_mun_firms + 
                   log_per_capita_assets + share_profitable_firms | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

mod_twfe_total = feols(t8013001_34_t8013001_1 ~ treatment +       
                         log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                         retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                         living_space + n_companies + pop_work + log_population + log_wage +
                         workers + t8006003 + pension_c + share_profitable_mun_firms + 
                         log_per_capita_assets + share_profitable_firms |                   
                         settlement + year,                                           
                       cluster = ~region,                                             
                       data = data)
summary(mod_twfe_total)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(t8013001_34_t8013001_1 ~ sunab(first.treat, year) + 
                 log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                 retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                 living_space + n_companies + pop_work + log_population + log_wage +
                 workers + t8006003 + pension_c + share_profitable_mun_firms + 
                 log_per_capita_assets + share_profitable_firms | 
                 settlement + year,  
               cluster = ~region,  
               data = data)
summary(mod_sa, agg = "att")

iplot(list(mod_twfe, mod_sa), sep = 0.5, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2020)"), cex = 0.7)


################################################################################

# безвозмездные поступления от других бюджетов
y_var <- "t8013001_36_c"
cov_vars <- c("build_flat", "catering_c", "construction_c", "doctors_per10", 
              "living_space", "n_companies", "pop_work", "log_population", 
              "retail_c", "log_wage", "workers", "t8006003", 
              "share_profitable_firms", "volume_electr_c", "volume_manufact_c")

data <- big_cities %>%    filter(group != "unexpected") %>%  
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(t8013001_36_c ~ i(time_to_treat, treat, ref = -1) + 
                   build_flat + catering_c + construction_c + doctors_per10 + 
                   living_space + n_companies + pop_work + log_population + 
                   retail_c + log_wage + workers + t8006003 +
                   share_profitable_firms + volume_electr_c + volume_manufact_c| 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

mod_twfe_total = feols(t8013001_36_c ~ treatment +       
                         build_flat + catering_c + construction_c + doctors_per10 + 
                         living_space + n_companies + pop_work + log_population + 
                         retail_c + log_wage + workers + t8006003 +
                         share_profitable_firms + volume_electr_c + volume_manufact_c|                   
                         settlement + year,                                           
                       cluster = ~region,                                             
                       data = data)
summary(mod_twfe_total)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(t8013001_36_c ~ sunab(first.treat, year) + 
                 build_flat + catering_c + construction_c + doctors_per10 + 
                 living_space + n_companies + pop_work + log_population + 
                 retail_c + log_wage + workers + t8006003 +
                 share_profitable_firms + volume_electr_c + volume_manufact_c| 
                 settlement + year,  
               cluster = ~region,  
               data = data)
summary(mod_sa, agg = "att")

iplot(list(mod_twfe, mod_sa), sep = 0.5, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2021)"), cex = 0.7)

################################################################################

# безвозмездные поступления от других бюджетов на душу с минимальным набором контролей
y_var <- "t8013001_36_c_pc"
cov_vars <- c("log_build_flat", "log_new_housing", "catering_c_pc", "construction_c_pc",
              "retail_c_pc", "volume_electr_c_pc", "volume_manufact_c_pc", "doctors_per10",
              "living_space", "n_companies", "pop_work", "log_population", "log_wage",
              "workers", "t8006003", "pension_c")

data <- big_cities %>%    filter(group != "unexpected") %>%  
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(t8013001_36_c_pc ~ i(time_to_treat, treat, ref = -1) + 
                   log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                   retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                   living_space + n_companies + pop_work + log_population + log_wage +
                   workers + t8006003 + pension_c | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

mod_twfe_total = feols(t8013001_36_c_pc ~ treatment +       
                         log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                         retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                         living_space + n_companies + pop_work + log_population + log_wage +
                         workers + t8006003 + pension_c |                   
                         settlement + year,                                           
                       cluster = ~region,                                             
                       data = data)
summary(mod_twfe_total)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(t8013001_36_c_pc ~ sunab(first.treat, year) + 
                 log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                 retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                 living_space + n_companies + pop_work + log_population + log_wage +
                 workers + t8006003 + pension_c | 
                 settlement + year,  
               cluster = ~region,  
               data = data)
summary(mod_sa, agg = "att")

iplot(list(mod_twfe, mod_sa), sep = 0.5, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2020)"), cex = 0.7)


################################################################################

# безвозмездные поступления от других бюджетов на душу с активами и фирмами
y_var <- "t8013001_36_c_pc"
cov_vars <- c("log_build_flat", "log_new_housing", "catering_c_pc", "construction_c_pc",
              "retail_c_pc", "volume_electr_c_pc", "volume_manufact_c_pc", "doctors_per10",
              "living_space", "n_companies", "pop_work", "log_population", "log_wage",
              "workers", "t8006003", "pension_c", "share_profitable_mun_firms", 
              "log_per_capita_assets", "share_profitable_firms")

data <- big_cities %>%    filter(group != "unexpected") %>%  
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(t8013001_36_c_pc ~ i(time_to_treat, treat, ref = -1) + 
                   log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                   retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                   living_space + n_companies + pop_work + log_population + log_wage +
                   workers + t8006003 + pension_c + share_profitable_mun_firms + 
                   log_per_capita_assets + share_profitable_firms | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

mod_twfe_total = feols(t8013001_36_c_pc ~ treatment +       
                         log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                         retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                         living_space + n_companies + pop_work + log_population + log_wage +
                         workers + t8006003 + pension_c + share_profitable_mun_firms + 
                         log_per_capita_assets + share_profitable_firms |                   
                         settlement + year,                                           
                       cluster = ~region,                                             
                       data = data)
summary(mod_twfe_total)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(t8013001_36_c_pc ~ sunab(first.treat, year) + 
                 log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                 retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                 living_space + n_companies + pop_work + log_population + log_wage +
                 workers + t8006003 + pension_c + share_profitable_mun_firms + 
                 log_per_capita_assets + share_profitable_firms | 
                 settlement + year,  
               cluster = ~region,  
               data = data)
summary(mod_sa, agg = "att")

iplot(list(mod_twfe, mod_sa), sep = 0.5, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2020)"), cex = 0.7)


################################################################################

# доля безвозмездных поступлений от других бюджетов с минимальным набором контролей
y_var <- "t8013001_36_t8013001_1"
cov_vars <- c("log_build_flat", "log_new_housing", "catering_c_pc", "construction_c_pc",
              "retail_c_pc", "volume_electr_c_pc", "volume_manufact_c_pc", "doctors_per10",
              "living_space", "n_companies", "pop_work", "log_population", "log_wage",
              "workers", "t8006003", "pension_c")

data <- big_cities %>%    filter(group != "unexpected") %>%  
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(t8013001_36_t8013001_1 ~ i(time_to_treat, treat, ref = -1) + 
                   log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                   retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                   living_space + n_companies + pop_work + log_population + log_wage +
                   workers + t8006003 + pension_c | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

mod_twfe_total = feols(t8013001_36_t8013001_1 ~ treatment +       
                         log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                         retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                         living_space + n_companies + pop_work + log_population + log_wage +
                         workers + t8006003 + pension_c |                   
                         settlement + year,                                           
                       cluster = ~region,                                             
                       data = data)
summary(mod_twfe_total)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(t8013001_36_t8013001_1 ~ sunab(first.treat, year) + 
                 log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                 retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                 living_space + n_companies + pop_work + log_population + log_wage +
                 workers + t8006003 + pension_c | 
                 settlement + year,  
               cluster = ~region,  
               data = data)
summary(mod_sa, agg = "att")

iplot(list(mod_twfe, mod_sa), sep = 0.5, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2020)"), cex = 0.7)


################################################################################

# доля безвозмездных поступлений от других бюджетов с активами и фирмами
y_var <- "t8013001_36_t8013001_1"
cov_vars <- c("log_build_flat", "log_new_housing", "catering_c_pc", "construction_c_pc",
              "retail_c_pc", "volume_electr_c_pc", "volume_manufact_c_pc", "doctors_per10",
              "living_space", "n_companies", "pop_work", "log_population", "log_wage",
              "workers", "t8006003", "pension_c", "share_profitable_mun_firms", 
              "log_per_capita_assets", "share_profitable_firms")

data <- big_cities %>%    filter(group != "unexpected") %>%  
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(t8013001_36_t8013001_1 ~ i(time_to_treat, treat, ref = -1) + 
                   log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                   retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                   living_space + n_companies + pop_work + log_population + log_wage +
                   workers + t8006003 + pension_c + share_profitable_mun_firms + 
                   log_per_capita_assets + share_profitable_firms | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

mod_twfe_total = feols(t8013001_36_t8013001_1 ~ treatment +       
                         log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                         retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                         living_space + n_companies + pop_work + log_population + log_wage +
                         workers + t8006003 + pension_c + share_profitable_mun_firms + 
                         log_per_capita_assets + share_profitable_firms |                   
                         settlement + year,                                           
                       cluster = ~region,                                             
                       data = data)
summary(mod_twfe_total)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(t8013001_36_t8013001_1 ~ sunab(first.treat, year) + 
                 log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                 retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                 living_space + n_companies + pop_work + log_population + log_wage +
                 workers + t8006003 + pension_c + share_profitable_mun_firms + 
                 log_per_capita_assets + share_profitable_firms | 
                 settlement + year,  
               cluster = ~region,  
               data = data)
summary(mod_sa, agg = "att")

iplot(list(mod_twfe, mod_sa), sep = 0.5, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2020)"), cex = 0.7)


################################################################################

# доходы от имущества
y_var <- "t8013001_27_c"
cov_vars <- c("build_flat", "catering_c", "construction_c", "doctors_per10", 
              "living_space", "n_companies", "pop_work", "log_population", 
              "retail_c", "log_wage", "workers", "t8006003", 
              "share_profitable_firms", "volume_electr_c", "volume_manufact_c")

data <- big_cities %>%    filter(group != "unexpected") %>%  
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(t8013001_27_c ~ i(time_to_treat, treat, ref = -1) + 
                   build_flat + catering_c + construction_c + doctors_per10 + 
                   living_space + n_companies + pop_work + log_population + 
                   retail_c + log_wage + workers + t8006003 +
                   share_profitable_firms + volume_electr_c + volume_manufact_c| 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

mod_twfe_total = feols(t8013001_27_c ~ treatment +       
                         build_flat + catering_c + construction_c + doctors_per10 + 
                         living_space + n_companies + pop_work + log_population + 
                         retail_c + log_wage + workers + t8006003 +
                         share_profitable_firms + volume_electr_c + volume_manufact_c|                   
                         settlement + year,                                           
                       cluster = ~region,                                             
                       data = data)
summary(mod_twfe_total)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(t8013001_27_c ~ sunab(first.treat, year) + 
                 build_flat + catering_c + construction_c + doctors_per10 + 
                 living_space + n_companies + pop_work + log_population + 
                 retail_c + log_wage + workers + t8006003 +
                 share_profitable_firms + volume_electr_c + volume_manufact_c| 
                 settlement + year,  
               cluster = ~region,  
               data = data)
summary(mod_sa, agg = "att")

iplot(list(mod_twfe, mod_sa), sep = 0.5, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2021)"), cex = 0.7)

################################################################################

# доходы от имущества на душу с минимальным набором контролей
y_var <- "t8013001_27_c_pc"
cov_vars <- c("log_build_flat", "log_new_housing", "catering_c_pc", "construction_c_pc",
              "retail_c_pc", "volume_electr_c_pc", "volume_manufact_c_pc", "doctors_per10",
              "living_space", "n_companies", "pop_work", "log_population", "log_wage",
              "workers", "t8006003", "pension_c")

data <- big_cities %>%    filter(group != "unexpected") %>%  
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(t8013001_27_c_pc ~ i(time_to_treat, treat, ref = -1) + 
                   log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                   retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                   living_space + n_companies + pop_work + log_population + log_wage +
                   workers + t8006003 + pension_c | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

mod_twfe_total = feols(t8013001_27_c_pc ~ treatment +       
                         log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                         retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                         living_space + n_companies + pop_work + log_population + log_wage +
                         workers + t8006003 + pension_c |                   
                         settlement + year,                                           
                       cluster = ~region,                                             
                       data = data)
summary(mod_twfe_total)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(t8013001_27_c_pc ~ sunab(first.treat, year) + 
                 log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                 retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                 living_space + n_companies + pop_work + log_population + log_wage +
                 workers + t8006003 + pension_c | 
                 settlement + year,  
               cluster = ~region,  
               data = data)
summary(mod_sa, agg = "att")

iplot(list(mod_twfe, mod_sa), sep = 0.5, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2020)"), cex = 0.7)


################################################################################

# доходы от имущества на душу с активами и фирмами
y_var <- "t8013001_27_c_pc"
cov_vars <- c("log_build_flat", "log_new_housing", "catering_c_pc", "construction_c_pc",
              "retail_c_pc", "volume_electr_c_pc", "volume_manufact_c_pc", "doctors_per10",
              "living_space", "n_companies", "pop_work", "log_population", "log_wage",
              "workers", "t8006003", "pension_c", "share_profitable_mun_firms", 
              "log_per_capita_assets", "share_profitable_firms")

data <- big_cities %>%    filter(group != "unexpected") %>%  
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(t8013001_27_c_pc ~ i(time_to_treat, treat, ref = -1) + 
                   log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                   retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                   living_space + n_companies + pop_work + log_population + log_wage +
                   workers + t8006003 + pension_c + share_profitable_mun_firms + 
                   log_per_capita_assets + share_profitable_firms | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

mod_twfe_total = feols(t8013001_27_c_pc ~ treatment +       
                         log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                         retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                         living_space + n_companies + pop_work + log_population + log_wage +
                         workers + t8006003 + pension_c + share_profitable_mun_firms + 
                         log_per_capita_assets + share_profitable_firms |                   
                         settlement + year,                                           
                       cluster = ~region,                                             
                       data = data)
summary(mod_twfe_total)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(t8013001_27_c_pc ~ sunab(first.treat, year) + 
                 log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                 retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                 living_space + n_companies + pop_work + log_population + log_wage +
                 workers + t8006003 + pension_c + share_profitable_mun_firms + 
                 log_per_capita_assets + share_profitable_firms | 
                 settlement + year,  
               cluster = ~region,  
               data = data)
summary(mod_sa, agg = "att")

iplot(list(mod_twfe, mod_sa), sep = 0.5, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2020)"), cex = 0.7)


################################################################################

# профицит бюджета с минимальным набором контролей
y_var <- "budget_prof_c"
cov_vars <- c("log_build_flat", "log_new_housing", "catering_c_pc", "construction_c_pc",
              "retail_c_pc", "volume_electr_c_pc", "volume_manufact_c_pc", "doctors_per10",
              "living_space", "n_companies", "pop_work", "log_population", "log_wage",
              "workers", "t8006003", "pension_c")

data <- big_cities %>%    filter(group != "unexpected") %>%  
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(budget_prof_c ~ i(time_to_treat, treat, ref = -1) + 
                   log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                   retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                   living_space + n_companies + pop_work + log_population + log_wage +
                   workers + t8006003 + pension_c | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

mod_twfe_total = feols(budget_prof_c ~ treatment +       
                         log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                         retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                         living_space + n_companies + pop_work + log_population + log_wage +
                         workers + t8006003 + pension_c |                   
                         settlement + year,                                           
                       cluster = ~region,                                             
                       data = data)
summary(mod_twfe_total)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(budget_prof_c ~ sunab(first.treat, year) + 
                 log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                 retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                 living_space + n_companies + pop_work + log_population + log_wage +
                 workers + t8006003 + pension_c | 
                 settlement + year,  
               cluster = ~region,  
               data = data)
summary(mod_sa, agg = "att")

iplot(list(mod_twfe, mod_sa), sep = 0.5, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2020)"), cex = 0.7)


################################################################################

# профицит бюджета на душу с минимальным набором контролей
y_var <- "budget_prof_c_pc"
cov_vars <- c("log_build_flat", "log_new_housing", "catering_c_pc", "construction_c_pc",
              "retail_c_pc", "volume_electr_c_pc", "volume_manufact_c_pc", "doctors_per10",
              "living_space", "n_companies", "pop_work", "log_population", "log_wage",
              "workers", "t8006003", "pension_c")

data <- big_cities %>%    filter(group != "unexpected") %>%  
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(budget_prof_c_pc ~ i(time_to_treat, treat, ref = -1) + 
                   log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                   retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                   living_space + n_companies + pop_work + log_population + log_wage +
                   workers + t8006003 + pension_c | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

mod_twfe_total = feols(budget_prof_c_pc ~ treatment +       
                         log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                         retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                         living_space + n_companies + pop_work + log_population + log_wage +
                         workers + t8006003 + pension_c |                   
                         settlement + year,                                           
                       cluster = ~region,                                             
                       data = data)
summary(mod_twfe_total)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(budget_prof_c_pc ~ sunab(first.treat, year) + 
                 log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                 retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                 living_space + n_companies + pop_work + log_population + log_wage +
                 workers + t8006003 + pension_c | 
                 settlement + year,  
               cluster = ~region,  
               data = data)
summary(mod_sa, agg = "att")

iplot(list(mod_twfe, mod_sa), sep = 0.5, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2020)"), cex = 0.7)


################################################################################

# профицит бюджета на душу с активами и фирмами
y_var <- "budget_prof_c_pc"
cov_vars <- c("log_build_flat", "log_new_housing", "catering_c_pc", "construction_c_pc",
              "retail_c_pc", "volume_electr_c_pc", "volume_manufact_c_pc", "doctors_per10",
              "living_space", "n_companies", "pop_work", "log_population", "log_wage",
              "workers", "t8006003", "pension_c", "share_profitable_mun_firms", 
              "log_per_capita_assets", "share_profitable_firms")

data <- big_cities %>%    filter(group != "unexpected") %>%  
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(budget_prof_c_pc ~ i(time_to_treat, treat, ref = -1) + 
                   log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                   retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                   living_space + n_companies + pop_work + log_population + log_wage +
                   workers + t8006003 + pension_c + share_profitable_mun_firms + 
                   log_per_capita_assets + share_profitable_firms | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

mod_twfe_total = feols(budget_prof_c_pc ~ treatment +       
                         log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                         retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                         living_space + n_companies + pop_work + log_population + log_wage +
                         workers + t8006003 + pension_c + share_profitable_mun_firms + 
                         log_per_capita_assets + share_profitable_firms |                   
                         settlement + year,                                           
                       cluster = ~region,                                             
                       data = data)
summary(mod_twfe_total)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(budget_prof_c_pc ~ sunab(first.treat, year) + 
                 log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                 retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                 living_space + n_companies + pop_work + log_population + log_wage +
                 workers + t8006003 + pension_c + share_profitable_mun_firms + 
                 log_per_capita_assets + share_profitable_firms | 
                 settlement + year,  
               cluster = ~region,  
               data = data)
summary(mod_sa, agg = "att")

iplot(list(mod_twfe, mod_sa), sep = 0.5, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2020)"), cex = 0.7)


################################################################################

# численность работников госуправления с минимальным набором контролей
y_var <- "t8123015_12"
cov_vars <- c("log_build_flat", "log_new_housing", "catering_c_pc", "construction_c_pc",
              "retail_c_pc", "volume_electr_c_pc", "volume_manufact_c_pc", "doctors_per10",
              "living_space", "n_companies", "pop_work", "log_population", "log_wage",
              "workers", "t8006003", "pension_c")

data <- big_cities %>%    filter(group != "unexpected") %>%  
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(t8123015_12 ~ i(time_to_treat, treat, ref = -1) + 
                   log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                   retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                   living_space + n_companies + pop_work + log_population + log_wage +
                   workers + t8006003 + pension_c | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

mod_twfe_total = feols(t8123015_12 ~ treatment +       
                         log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                         retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                         living_space + n_companies + pop_work + log_population + log_wage +
                         workers + t8006003 + pension_c |                   
                         settlement + year,                                           
                       cluster = ~region,                                             
                       data = data)
summary(mod_twfe_total)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(t8123015_12 ~ sunab(first.treat, year) + 
                 log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                 retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                 living_space + n_companies + pop_work + log_population + log_wage +
                 workers + t8006003 + pension_c | 
                 settlement + year,  
               cluster = ~region,  
               data = data)
summary(mod_sa, agg = "att")

iplot(list(mod_twfe, mod_sa), sep = 0.5, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2020)"), cex = 0.7)


################################################################################

# заработная плата работников госуправления с минимальным набором контролей
y_var <- "t8123017_12_c"
cov_vars <- c("log_build_flat", "log_new_housing", "catering_c_pc", "construction_c_pc",
              "retail_c_pc", "volume_electr_c_pc", "volume_manufact_c_pc", "doctors_per10",
              "living_space", "n_companies", "pop_work", "log_population", "log_wage",
              "workers", "t8006003", "pension_c")

data <- big_cities %>%    filter(group != "unexpected") %>%  
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(t8123017_12_c ~ i(time_to_treat, treat, ref = -1) + 
                   log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                   retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                   living_space + n_companies + pop_work + log_population + log_wage +
                   workers + t8006003 + pension_c | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

mod_twfe_total = feols(t8123017_12_c ~ treatment +       
                         log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                         retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                         living_space + n_companies + pop_work + log_population + log_wage +
                         workers + t8006003 + pension_c |                   
                         settlement + year,                                           
                       cluster = ~region,                                             
                       data = data)
summary(mod_twfe_total)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(t8123017_12_c ~ sunab(first.treat, year) + 
                 log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                 retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                 living_space + n_companies + pop_work + log_population + log_wage +
                 workers + t8006003 + pension_c | 
                 settlement + year,  
               cluster = ~region,  
               data = data)
summary(mod_sa, agg = "att")

iplot(list(mod_twfe, mod_sa), sep = 0.5, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2020)"), cex = 0.7)


################################################################################

# расходы на содержание работников местного СУ на душу с минимальным набором контролей
y_var <- "t8013004_c"
cov_vars <- c("log_build_flat", "log_new_housing", "catering_c_pc", "construction_c_pc",
              "retail_c_pc", "volume_electr_c_pc", "volume_manufact_c_pc", "doctors_per10",
              "living_space", "n_companies", "pop_work", "log_population", "log_wage",
              "workers", "t8006003", "pension_c")

data <- big_cities %>%    filter(group != "unexpected") %>%  
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(t8013004_c ~ i(time_to_treat, treat, ref = -1) + 
                   log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                   retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                   living_space + n_companies + pop_work + log_population + log_wage +
                   workers + t8006003 + pension_c | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

mod_twfe_total = feols(t8013004_c ~ treatment +       
                         log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                         retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                         living_space + n_companies + pop_work + log_population + log_wage +
                         workers + t8006003 + pension_c |                   
                         settlement + year,                                           
                       cluster = ~region,                                             
                       data = data)
summary(mod_twfe_total)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(t8013004_c ~ sunab(first.treat, year) + 
                 log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                 retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                 living_space + n_companies + pop_work + log_population + log_wage +
                 workers + t8006003 + pension_c | 
                 settlement + year,  
               cluster = ~region,  
               data = data)
summary(mod_sa, agg = "att")

iplot(list(mod_twfe, mod_sa), sep = 0.5, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2020)"), cex = 0.7)


################################################################################

# доля прибыльных муниципальных фирм с минимальным набором контролей + доля прибыльных фирм
y_var <- "share_profitable_mun_firms"
cov_vars <- c("log_build_flat", "log_new_housing", "catering_c_pc", "construction_c_pc",
              "retail_c_pc", "volume_electr_c_pc", "volume_manufact_c_pc", "doctors_per10",
              "living_space", "n_companies", "pop_work", "log_population", "log_wage",
              "workers", "t8006003", "pension_c", "share_profitable_firms")

data <- big_cities %>%    filter(group != "unexpected") %>%  
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(share_profitable_mun_firms ~ i(time_to_treat, treat, ref = -1) + 
                   log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                   retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                   living_space + n_companies + pop_work + log_population + log_wage +
                   workers + t8006003 + pension_c + share_profitable_firms | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

mod_twfe_total = feols(share_profitable_mun_firms ~ treatment +       
                         log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                         retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                         living_space + n_companies + pop_work + log_population + log_wage +
                         workers + t8006003 + pension_c + share_profitable_firms |                   
                         settlement + year,                                           
                       cluster = ~region,                                             
                       data = data)
summary(mod_twfe_total)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(share_profitable_mun_firms ~ sunab(first.treat, year) + 
                 log_build_flat + log_new_housing + catering_c_pc + construction_c_pc +
                 retail_c_pc + volume_electr_c_pc + volume_manufact_c_pc + doctors_per10 +
                 living_space + n_companies + pop_work + log_population + log_wage +
                 workers + t8006003 + pension_c + share_profitable_firms | 
                 settlement + year,  
               cluster = ~region,  
               data = data)
summary(mod_sa, agg = "att")

iplot(list(mod_twfe, mod_sa), sep = 0.5, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2020)"), cex = 0.7)







##########                  зависимые переменные                    ############
# invest_budg Удельный вес инвестиций в основной капитал, финансируемых за счет бюджетных средств, в общем объеме инвестиций, процентов
# invest_fed Удельный вес инвестиций в основной капитал, финансируемых за счет бюджетных средств, в общем объеме инвестиций, процентов в том числе за счет федерального бюджета
# investment Инвестиции в основной капитал (в фактически действовавших ценах), млн. руб.
# volume_electr Объем отгруженных товаров собственного производства, выполненных работ и услуг собственными силами по видам деятельности, млн. руб.: производство и распределение электроэнергии, газа и воды
# volume_manufact Объем отгруженных товаров собственного производства, выполненных работ и услуг собственными силами по видам деятельности, млн. руб.: обрабатывающие производства
# t8013002_1 Расходы местного бюджета, фактически исполненные|||t8013002|||Всего
# t8013002_212 Расходы местного бюджета, фактически исполненные|||t8013002|||Общегосударственные вопросы
# t8013002_220 Расходы местного бюджета, фактически исполненные|||t8013002|||Национальная безопасность и правоохранительная деятельность
# t8013002_221 Расходы местного бюджета, фактически исполненные|||t8013002|||Национальная экономика
# t8013002_229 Расходы местного бюджета, фактически исполненные|||t8013002|||Жилищно-коммунальное хозяйство
# t8013002_234 Расходы местного бюджета, фактически исполненные|||t8013002|||Социальная политика
# t8016002 Численность работников организаций культурно-досугового типа с учетом обособленных подразделений, всего
# t8008025 Одиночное протяжение уличной водопроводной сети, которая заменена и отремонтирована за отчетный год
# t8006007 Общая протяженность улиц, проездов, набережных на конец года
# t8013001_1 Доходы местного бюджета, фактически исполненные|||t8013001|||Всего
# t8013001_89 Доходы местного бюджета, фактически исполненные|||t8013001|||Из общей величины доходов - собственные доходы
# t8013001_34 Доходы местного бюджета, фактически исполненные|||t8013001|||Безвозмездные поступления
# t8013001_36 Доходы местного бюджета, фактически исполненные|||t8013001|||Безвозмездные поступления от других бюджетов бюджетной системы Российской Федерации
# t8013001_27 Доходы местного бюджета, фактически исполненные|||t8013001|||Доходы от использования имущества, находящегося в государственной и муниципальной собственности
# t8011011_0 Число семей, состоящих на учете в качестве нуждающихся в жилых помещениях на конец года|||t8011011|||Всего
# t8011010_0 Число семей, получивших жилые помещения и улучшивших жилищные условия в отчетном году.|||t8011010|||Всего
# t8045002_21 Наличие основных фондов на конец года по полной учетной стоимости по некоммерческим организациям муниципальной формы собственности|||t8045002|||Здания
# t8045002_27 Наличие основных фондов на конец года по полной учетной стоимости по некоммерческим организациям муниципальной формы собственности|||t8045002|||Транспортные средства
# 





