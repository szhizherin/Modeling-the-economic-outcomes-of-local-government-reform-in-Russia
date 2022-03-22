# Purpose: estimate panel matching models on big_cities data
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
big_cities$log_population <- log(big_cities$population)
big_cities$log_wage <- log(big_cities$wage)
big_cities["t8008008/t8008007"] <- big_cities$t8008008 / big_cities$t8008007


################################################################################


y_var <- "t8013002_1_c"
cov_vars <- c("build_flat", "catering_c", "construction_c", "doctors_per10", 
              "living_space", "n_companies", "pop_work", "log_population", 
              "retail_c", "log_wage", "workers", "t8006003")

data <- big_cities %>% 
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(t8013002_1_c ~ i(time_to_treat, treat, ref = -1) +             ## Key interaction: time × treatment status
                   build_flat + catering_c + construction_c + doctors_per10 + 
                   living_space + n_companies + pop_work + log_population + 
                   retail_c + log_wage + workers + t8006003 |                   ## Other controls
                   settlement + year,                                           ## FEs
                 cluster = ~region,                                             ## Clustered SEs
                 data = data)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(t8013002_1_c ~ sunab(first.treat, year) +                        ## key interaction: time × treatment status
                   build_flat + catering_c + construction_c + doctors_per10 + 
                   living_space + n_companies + pop_work + log_population + 
                   retail_c + log_wage + workers + t8006003 |                   ## Other controls
                   settlement + year,                                           ## FEs
                 cluster = ~region,                                             ## Clustered SEs
                 data = data)

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
           "time_to_treat", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(t8013002_212_c ~ i(time_to_treat, treat, ref = -1) + 
                   build_flat + catering_c + construction_c + doctors_per10 + 
                   living_space + n_companies + pop_work + log_population + 
                   retail_c + log_wage + workers + t8006003 | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

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

iplot(list(mod_twfe, mod_sa), sep = 0.5, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2020)"), cex = 0.7)


################################################################################


y_var <- "t8013002_220_c" # значимо, пример как TWFE не ловит эффект
cov_vars <- c("build_flat", "catering_c", "construction_c", "doctors_per10", 
              "living_space", "n_companies", "pop_work", "log_population", 
              "retail_c", "log_wage", "workers", "t8006003")

data <- big_cities %>% 
  filter(group %in% c("0", "0 -> 1", "1", "1 -> 2", "2", "0 -> 1 -> 2")) %>% 
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(t8013002_220_c ~ i(time_to_treat, treat, ref = -1) + 
                   build_flat + catering_c + construction_c + doctors_per10 + 
                   living_space + n_companies + pop_work + log_population + 
                   retail_c + log_wage + workers + t8006003 | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(t8013002_220_c ~ sunab(first.treat, year) + 
                 build_flat + catering_c + construction_c + doctors_per10 + 
                 living_space + n_companies + pop_work + log_population + 
                 retail_c + log_wage + workers + t8006003 | 
                 settlement + year,  
               cluster = ~region,  
               data = data)

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
           "time_to_treat", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(t8013002_221_c ~ i(time_to_treat, treat, ref = -1) + 
                   build_flat + catering_c + construction_c + doctors_per10 + 
                   living_space + n_companies + pop_work + log_population + 
                   retail_c + log_wage + workers + t8006003 | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

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
           "time_to_treat", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(t8013002_229_c ~ i(time_to_treat, treat, ref = -1) + 
                   build_flat + catering_c + construction_c + doctors_per10 + 
                   living_space + n_companies + pop_work + log_population + 
                   retail_c + log_wage + workers + t8006003 | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

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
           "time_to_treat", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(t8013002_234_c ~ i(time_to_treat, treat, ref = -1) + 
                   build_flat + catering_c + construction_c + doctors_per10 + 
                   living_space + n_companies + pop_work + log_population + 
                   retail_c + log_wage + workers + t8006003 | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

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
           "time_to_treat", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(invest_budg ~ i(time_to_treat, treat, ref = -1) + 
                   build_flat + catering_c + construction_c + doctors_per10 + 
                   living_space + n_companies + pop_work + log_population + 
                   retail_c + log_wage + workers + t8006003 | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

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
           "time_to_treat", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(invest_fed ~ i(time_to_treat, treat, ref = -1) + 
                   build_flat + catering_c + construction_c + doctors_per10 + 
                   living_space + n_companies + pop_work + log_population + 
                   retail_c + log_wage + workers + t8006003 | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

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
           "time_to_treat", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(investment_c ~ i(time_to_treat, treat, ref = -1) + 
                   build_flat + catering_c + construction_c + doctors_per10 + 
                   living_space + n_companies + pop_work + log_population + 
                   retail_c + log_wage + workers + t8006003 | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

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
           "time_to_treat", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(volume_electr_c ~ i(time_to_treat, treat, ref = -1) + 
                   build_flat + catering_c + construction_c + doctors_per10 + 
                   living_space + n_companies + pop_work + log_population + 
                   retail_c + log_wage + workers + t8006003 | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

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
           "time_to_treat", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(volume_manufact_c ~ i(time_to_treat, treat, ref = -1) + 
                   build_flat + catering_c + construction_c + doctors_per10 + 
                   living_space + n_companies + pop_work + log_population + 
                   retail_c + log_wage + workers + t8006003 | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

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

iplot(list(mod_twfe, mod_sa), sep = 0.5, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2020)"), cex = 0.7)


########## переменные, которые точно нужно брать как зависимые т.к. мало пропусков ############
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
# 
