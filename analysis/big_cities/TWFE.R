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


# "c" stands for inflation- and regional prices-corrected
big_cities$catering_c <- big_cities$catering * big_cities$index
big_cities$construction_c <- big_cities$construction * big_cities$index
big_cities$pension_c <- big_cities$pension * big_cities$index
big_cities$retail_c <- big_cities$retail * big_cities$index
big_cities$wage_c <- big_cities$wage * big_cities$index
big_cities$investment_c <- big_cities$investment * big_cities$index
big_cities$volume_electr_c <- big_cities$volume_electr * big_cities$index
big_cities$volume_manufact_c <- big_cities$volume_manufact * big_cities$index
big_cities$t8013002_1_c <- big_cities$t8013002_1 * big_cities$index
big_cities$t8013002_212_c <- big_cities$t8013002_212 * big_cities$index
big_cities$t8013002_220_c <- big_cities$t8013002_220 * big_cities$index
big_cities$t8013002_221_c <- big_cities$t8013002_221 * big_cities$index
big_cities$t8013002_229_c <- big_cities$t8013002_229 * big_cities$index
big_cities$t8013002_234_c <- big_cities$t8013002_234 * big_cities$index
big_cities["t8013002_220_t8013002_1"] <- big_cities$t8013002_220 / big_cities$t8013002_1 # доля расходов на правоохранителей
big_cities$log_population <- log(big_cities$population)
big_cities$log_wage <- log(big_cities$wage)
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
big_cities$log_per_capita_assets <- log(big_cities$assets / big_cities$population)


non_competitive_elections <- c("Республика Адыгея", "Республика Дагестан", 
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

big_cities$competitive <- 1 * !(big_cities$region %in% non_competitive_elections)


################################################################################


y_var <- "t8013002_1_c"
cov_vars <- c("build_flat", "catering_c", "construction_c", "doctors_per10", 
              "living_space", "n_companies", "pop_work", "log_population", 
              "retail_c", "log_wage", "workers", "t8006003",
              "share_profitable_firms", "log_per_capita_assets")

data <- big_cities %>% 
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", "competitive", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(t8013002_1_c ~ i(time_to_treat, treat, ref = -1) +             ## Key interaction: time × treatment status
                   build_flat + catering_c + construction_c + doctors_per10 + 
                   living_space + n_companies + pop_work + log_population + 
                   retail_c + log_wage + workers + t8006003 |                   ## Other controls
                   settlement + year,                                           ## FEs
                 cluster = ~region,                                             ## Clustered SEs
                 data = data)

mod_twfe_total = feols(t8013002_1_c ~ treatment +       
                   build_flat + catering_c + construction_c + doctors_per10 + 
                   living_space + n_companies + pop_work + log_population + 
                   retail_c + log_wage + workers + t8006003 |                   
                   settlement + year,                                           
                 cluster = ~region,                                             
                 data = data)
summary(mod_twfe_total)

mod_twfe_total = feols(t8013002_1_c ~ treatment +  
                         log_population + log_wage + log_per_capita_assets +
                         share_profitable_firms |                   
                         settlement + year,                                           
                       cluster = ~region,                                             
                       data = data)
summary(mod_twfe_total)

mod_twfe_total = feols(t8013002_1_c ~ i(treatment, competitive, 0) + treatment +     
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

mod_sa = feols(t8013002_1_c ~ sunab(first.treat, year, -1) + 
                 log_population + log_wage + log_per_capita_assets +
                 share_profitable_firms | 
                 settlement + year, 
               cluster = ~region, 
               data = data)
summary(mod_sa, agg = "att")
iplot(mod_sa, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(t8013002_1_c ~ sunab(first.treat, year, -1) +                        ## key interaction: time × treatment status
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


y_var <- "t8013002_212_c"
cov_vars <- c("build_flat", "catering_c", "construction_c", "doctors_per10", 
              "living_space", "n_companies", "pop_work", "log_population", 
              "retail_c", "log_wage", "workers", "t8006003")

data <- big_cities %>% 
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


y_var <- "t8013002_220_c" # значимо, пример как TWFE не ловит эффект
cov_vars <- c("build_flat", "catering_c", "construction_c", "doctors_per10", 
              "living_space", "n_companies", "pop_work", "log_population", 
              "retail_c", "log_wage", "workers", "t8006003",
              "share_profitable_firms", "log_per_capita_assets")

data <- big_cities %>% 
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


y_var <- "t8013002_220_t8013002_1"
cov_vars <- c("build_flat", "catering_c", "construction_c", "doctors_per10", 
              "living_space", "n_companies", "pop_work", "log_population", 
              "retail_c", "log_wage", "workers", "t8006003",
              "share_profitable_firms", "log_per_capita_assets")

data <- big_cities %>% 
  filter(competitive == 1) %>% 
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

y_var <- "t8013002_221_c"
cov_vars <- c("build_flat", "catering_c", "construction_c", "doctors_per10", 
              "living_space", "n_companies", "pop_work", "log_population", 
              "retail_c", "log_wage", "workers", "t8006003")

data <- big_cities %>% 
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


y_var <- "t8013002_229_c"
cov_vars <- c("build_flat", "catering_c", "construction_c", "doctors_per10", 
              "living_space", "n_companies", "pop_work", "log_population", 
              "retail_c", "log_wage", "workers", "t8006003")

data <- big_cities %>% 
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


y_var <- "t8013002_234_c"
cov_vars <- c("build_flat", "catering_c", "construction_c", "doctors_per10", 
              "living_space", "n_companies", "pop_work", "log_population", 
              "retail_c", "log_wage", "workers", "t8006003")

data <- big_cities %>% 
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


y_var <- "invest_budg"
cov_vars <- c("build_flat", "catering_c", "construction_c", "doctors_per10", 
              "living_space", "n_companies", "pop_work", "log_population", 
              "retail_c", "log_wage", "workers", "t8006003")

data <- big_cities %>% 
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


y_var <- "invest_fed"
cov_vars <- c("build_flat", "catering_c", "construction_c", "doctors_per10", 
              "living_space", "n_companies", "pop_work", "log_population", 
              "retail_c", "log_wage", "workers", "t8006003")

data <- big_cities %>% 
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


y_var <- "investment_c"
cov_vars <- c("build_flat", "catering_c", "construction_c", "doctors_per10", 
              "living_space", "n_companies", "pop_work", "log_population", 
              "retail_c", "log_wage", "workers", "t8006003")

data <- big_cities %>% 
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


y_var <- "volume_electr_c"
cov_vars <- c("build_flat", "catering_c", "construction_c", "doctors_per10", 
              "living_space", "n_companies", "pop_work", "log_population", 
              "retail_c", "log_wage", "workers", "t8006003")

data <- big_cities %>% 
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(volume_electr_c ~ i(time_to_treat, treat, ref = -1) + 
                   build_flat + catering_c + construction_c + doctors_per10 + 
                   living_space + n_companies + pop_work + log_population + 
                   retail_c + log_wage + workers + t8006003 | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

mod_twfe_total = feols(volume_electr_c ~ treatment +       
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

mod_sa = feols(volume_electr_c ~ sunab(first.treat, year) + 
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


y_var <- "volume_manufact_c"
cov_vars <- c("build_flat", "catering_c", "construction_c", "doctors_per10", 
              "living_space", "n_companies", "pop_work", "log_population", 
              "retail_c", "log_wage", "workers", "t8006003")

data <- big_cities %>% 
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(volume_manufact_c ~ i(time_to_treat, treat, ref = -1) + 
                   build_flat + catering_c + construction_c + doctors_per10 + 
                   living_space + n_companies + pop_work + log_population + 
                   retail_c + log_wage + workers + t8006003 | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

mod_twfe_total = feols(volume_manufact_c ~ treatment +       
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

mod_sa = feols(volume_manufact_c ~ sunab(first.treat, year) + 
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


y_var <- "t8016002" #10%
cov_vars <- c("build_flat", "catering_c", "construction_c", "doctors_per10", 
              "living_space", "n_companies", "pop_work", "log_population", 
              "retail_c", "log_wage", "workers", "t8006003")

data <- big_cities %>% 
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


y_var <- "t8008025"
cov_vars <- c("build_flat", "catering_c", "construction_c", "doctors_per10", 
              "living_space", "n_companies", "pop_work", "log_population", 
              "retail_c", "log_wage", "workers", "t8006003")

data <- big_cities %>% 
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


y_var <- "t8008008_t8008007"
cov_vars <- c("build_flat", "catering_c", "construction_c", "doctors_per10", 
              "living_space", "n_companies", "pop_work", "log_population", 
              "retail_c", "log_wage", "workers", "t8006003")

data <- big_cities %>% 
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


y_var <- "t8008025_t8008008"
cov_vars <- c("build_flat", "catering_c", "construction_c", "doctors_per10", 
              "living_space", "n_companies", "pop_work", "log_population", 
              "retail_c", "log_wage", "workers", "t8006003")

data <- big_cities %>% 
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


y_var <- "t8006003_t8006007"
cov_vars <- c("build_flat", "catering_c", "construction_c", "doctors_per10", 
              "living_space", "n_companies", "pop_work", "log_population", 
              "retail_c", "log_wage", "workers", "t8006003")

data <- big_cities %>% 
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


y_var <- "t8013001_1_c"
cov_vars <- c("build_flat", "catering_c", "construction_c", "doctors_per10", 
              "living_space", "n_companies", "pop_work", "log_population", 
              "retail_c", "log_wage", "workers", "t8006003", "share_profitable_firms")

data <- big_cities %>% 
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


y_var <- "t8013001_89_c"
cov_vars <- c("build_flat", "catering_c", "construction_c", "doctors_per10", 
              "living_space", "n_companies", "pop_work", "log_population", 
              "retail_c", "log_wage", "workers", "t8006003", "share_profitable_firms")

data <- big_cities %>% 
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


y_var <- "t8013001_89_t8013001_1"
cov_vars <- c("build_flat", "catering_c", "construction_c", "doctors_per10", 
              "living_space", "n_companies", "pop_work", "log_population", 
              "retail_c", "log_wage", "workers", "t8006003", 
              "share_profitable_firms", "volume_electr_c", "volume_manufact_c")

data <- big_cities %>% 
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


y_var <- "t8013001_34_c"
cov_vars <- c("build_flat", "catering_c", "construction_c", "doctors_per10", 
              "living_space", "n_companies", "pop_work", "log_population", 
              "retail_c", "log_wage", "workers", "t8006003", 
              "share_profitable_firms", "volume_electr_c", "volume_manufact_c")

data <- big_cities %>% 
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


y_var <- "t8013001_36_c"
cov_vars <- c("build_flat", "catering_c", "construction_c", "doctors_per10", 
              "living_space", "n_companies", "pop_work", "log_population", 
              "retail_c", "log_wage", "workers", "t8006003", 
              "share_profitable_firms", "volume_electr_c", "volume_manufact_c")

data <- big_cities %>% 
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


y_var <- "t8013001_27_c"
cov_vars <- c("build_flat", "catering_c", "construction_c", "doctors_per10", 
              "living_space", "n_companies", "pop_work", "log_population", 
              "retail_c", "log_wage", "workers", "t8006003", 
              "share_profitable_firms", "volume_electr_c", "volume_manufact_c")

data <- big_cities %>% 
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


y_var <- "t8013001_34_t8013001_1"
cov_vars <- c("build_flat", "catering_c", "construction_c", "doctors_per10", 
              "living_space", "n_companies", "pop_work", "log_population", 
              "retail_c", "log_wage", "workers", "t8006003", 
              "share_profitable_firms", "volume_electr_c", "volume_manufact_c")

data <- big_cities %>% 
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(t8013001_34_t8013001_1 ~ i(time_to_treat, treat, ref = -1) + 
                   build_flat + catering_c + construction_c + doctors_per10 + 
                   living_space + n_companies + pop_work + log_population + 
                   retail_c + log_wage + workers + t8006003 +
                   share_profitable_firms + volume_electr_c + volume_manufact_c| 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

mod_twfe_total = feols(t8013001_34_t8013001_1 ~ treatment +       
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

mod_sa = feols(t8013001_34_t8013001_1 ~ sunab(first.treat, year) + 
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





