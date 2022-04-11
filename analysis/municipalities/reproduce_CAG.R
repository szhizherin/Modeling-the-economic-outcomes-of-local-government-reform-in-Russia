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
municipalities$avg_wage_c <- municipalities$avg_wage * municipalities$index
municipalities$log_wage <- log1p(municipalities$avg_wage_c)

municipalities$t8013002_1_c <- municipalities$t8013002_1 * municipalities$index
municipalities$t8013002_212_c <- municipalities$t8013002_212 * municipalities$index
municipalities$t8013002_220_c <- municipalities$t8013002_220 * municipalities$index
municipalities$t8013002_221_c <- municipalities$t8013002_221 * municipalities$index
municipalities$t8013002_229_c <- municipalities$t8013002_229 * municipalities$index
municipalities$t8013002_234_c <- municipalities$t8013002_234 * municipalities$index

# per capita
municipalities$t8013002_1_c_pc <- municipalities$t8013002_1_c / municipalities$t8112013
municipalities$t8013002_212_c_pc <- municipalities$t8013002_212_c / municipalities$t8112013
municipalities$t8013002_220_c_pc <- municipalities$t8013002_220_c / municipalities$t8112013
municipalities$t8013002_221_c_pc <- municipalities$t8013002_221_c / municipalities$t8112013
municipalities$t8013002_229_c_pc <- municipalities$t8013002_229_c / municipalities$t8112013
municipalities$t8013002_234_c_pc <- municipalities$t8013002_234_c / municipalities$t8112013

municipalities["t8013002_220_t8013002_1"] <- municipalities$t8013002_220 / municipalities$t8013002_1 # доля расходов на правоохранителей
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

# per capita
municipalities$t8013001_1_c_pc <- municipalities$t8013001_1_c / municipalities$t8112013
municipalities$t8013001_89_c_pc <- municipalities$t8013001_89_c / municipalities$t8112013
municipalities$t8013001_34_c_pc <- municipalities$t8013001_34_c / municipalities$t8112013
municipalities$t8013001_36_c_pc <- municipalities$t8013001_36_c / municipalities$t8112013
municipalities$t8013001_27_c_pc <- municipalities$t8013001_27_c / municipalities$t8112013

municipalities$log_population <- log1p(municipalities$t8112013)
municipalities$log_per_capita_assets <- log1p(municipalities$t8045002_0 / municipalities$t8112013)
municipalities$log_per_capita_assets1 <- log1p(municipalities$t8045002_21 / municipalities$t8112013)
municipalities$log_per_capita_assets2 <- log1p(municipalities$t8045002_26 / municipalities$t8112013)
municipalities$log_per_capita_assets3 <- log1p(municipalities$t8045002_27 / municipalities$t8112013)
municipalities$log_new_housing <- log1p(municipalities$t8010001)


municipalities$log_per_capita_assets %>% is.na() %>% sum()

# "log_population", <- t8112013 или t8112027_11
# "log_per_capita_assets", <- t8045002_0 или t8045002_21 t8045002_26 t8045002_27
# "log_wage", <- avg_wage
# "share_profitable_firms", 
# "school", <- t8015001
# "school_child", <- t8015002
# "log_new_housing", <- t8010001
# "t8006007", 
# "t8011011_0"

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

municipalities$competitive <- 1 * !(municipalities$region %in% non_competitive_elections)


################################################################################

# все расходы на душу населения, все контроли + активы как 3 переменных
y_var <- "t8013002_1_c_pc"
cov_vars <- c("log_population", "log_per_capita_assets1", "log_per_capita_assets2",
              "log_per_capita_assets3", "log_wage", "share_profitable_firms",
              "t8015001", "t8015002", "log_new_housing", "t8006007", "t8011011_0")

data <- municipalities %>% 
  select(c("municipality", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", all_of(y_var), all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(t8013002_1_c_pc ~ i(time_to_treat, treat, ref = -1) + 
                   log_population + log_per_capita_assets1 + log_per_capita_assets2 +
                   log_per_capita_assets3 + log_wage + share_profitable_firms +
                   t8015001 + t8015002 + log_new_housing + t8006007 + t8011011_0 | 
                   municipality + year, 
                 cluster = ~region, 
                 data = data)

mod_twfe_total = feols(t8013002_1_c_pc ~ treatment +       
                         log_population + log_per_capita_assets1 + log_per_capita_assets2 +
                         log_per_capita_assets3 + log_wage + share_profitable_firms +
                         t8015001 + t8015002 + log_new_housing + t8006007 + t8011011_0 | 
                         municipality + year,                                           
                       cluster = ~region,                                             
                       data = data)
summary(mod_twfe_total)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(t8013002_1_c_pc ~ sunab(first.treat, year) + 
                 log_population + log_per_capita_assets1 + log_per_capita_assets2 +
                 log_per_capita_assets3 + log_wage + share_profitable_firms +
                 t8015001 + t8015002 + log_new_housing + t8006007 + t8011011_0 | 
                 municipality + year,  
               cluster = ~region,  
               data = data)
summary(mod_sa, agg = "att")

iplot(list(mod_twfe, mod_sa), sep = 0.5, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2020)"), cex = 0.7)


################################################################################

# все расходы на душу как ЦПУР
y_var <- "t8013002_1_c"
cov_vars <- c("log_population", "log_per_capita_assets", "log_wage", "share_profitable_firms",
              "t8015001", "t8015002", "log_new_housing", "t8006007", "t8011011_0")

municipalities$t8011011_0 %>% is.na() %>% sum()

data <- municipalities %>% 
  select(c("municipality", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", all_of(y_var), all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(t8013002_1_c ~ i(time_to_treat, treat, ref = -1) + 
                   log_population + log_per_capita_assets1 + log_per_capita_assets2 +
                   log_per_capita_assets3 + share_profitable_firms +
                   t8015001 + t8015002 + log_new_housing + t8006007 + t8011011_0 | 
                   municipality + year, 
                 cluster = ~region, 
                 data = data)

mod_twfe_total = feols(t8013002_1_c ~ treatment +       
                         log_population + log_per_capita_assets1 + log_per_capita_assets2 +
                         log_per_capita_assets3 + share_profitable_firms +
                         t8015001 + t8015002 + log_new_housing + t8006007 + t8011011_0 | 
                         municipality + year,                                           
                       cluster = ~region,                                             
                       data = data)
summary(mod_twfe_total)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(t8013002_1_c ~ sunab(first.treat, year) + 
                 log_population + log_per_capita_assets1 + log_per_capita_assets2 +
                 log_per_capita_assets3 + log_wage + share_profitable_firms +
                 t8015001 + t8015002 + log_new_housing + t8006007 + t8011011_0 | 
                 municipality + year,  
               cluster = ~region,  
               data = data)
summary(mod_sa, agg = "att")

iplot(list(mod_twfe, mod_sa), sep = 0.5, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2020)"), cex = 0.7)












