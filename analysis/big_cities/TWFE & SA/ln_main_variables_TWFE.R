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
big_cities$t8013001_5_c <- big_cities$t8013001_5 * big_cities$index # доходы от НДФЛ
big_cities$t8013001_296_c <- big_cities$t8013001_296 * big_cities$index # субсидии
big_cities$t8013001_294_c <- big_cities$t8013001_294 * big_cities$index # субвенции
big_cities$t8013001_293_c <- big_cities$t8013001_293 * big_cities$index # дотации
big_cities$t8013001_15_c <- big_cities$t8013001_15 * big_cities$index
big_cities$t8013001_14_c <- big_cities$t8013001_14 * big_cities$index


# per capita
big_cities$t8013001_1_c_pc <- big_cities$t8013001_1_c / big_cities$population
big_cities$t8013001_89_c_pc <- big_cities$t8013001_89_c / big_cities$population
big_cities$t8013001_34_c_pc <- big_cities$t8013001_34_c / big_cities$population
big_cities$t8013001_36_c_pc <- big_cities$t8013001_36_c / big_cities$population
big_cities$t8013001_27_c_pc <- big_cities$t8013001_27_c / big_cities$population
big_cities$t8013001_5_c_pc <- big_cities$t8013001_5_c / big_cities$population # НДФЛ
big_cities$t8013001_296_c_pc <- big_cities$t8013001_296_c / big_cities$population # субсидии
big_cities$t8013001_294_c_pc <- big_cities$t8013001_294_c / big_cities$population # субвенции
big_cities$t8013001_293_c_pc <- big_cities$t8013001_293_c / big_cities$population # дотации
big_cities$t8013001_15_c_pc <- big_cities$t8013001_15_c / big_cities$population
big_cities$t8013001_14_c_pc <- big_cities$t8013001_14_c / big_cities$population


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
big_cities$mun_debt_c <- big_cities$mun_debt * big_cities$index
big_cities$mun_debt_c_pc <- big_cities$mun_debt_c / big_cities$population
big_cities <- big_cities %>% 
  group_by(settlement) %>% 
  mutate(share_got_flat = t8011010_0 / dplyr::lag(t8011011_0)) %>% 
  ungroup()

big_cities$share_culture_workers <- big_cities$t8016002 / big_cities$workers # доля работников культуры

big_cities$mun_debt_supl_c <- big_cities$mun_debt_supl * big_cities$index
big_cities$mun_debt_supl_c_pc <- big_cities$mun_debt_supl_c / big_cities$population

big_cities$mun_debt_gov_c <- big_cities$mun_debt_gov * big_cities$index
big_cities$mun_debt_gov_c_pc <- big_cities$mun_debt_gov_c / big_cities$population

big_cities$mun_debt_budg_c <- big_cities$mun_debt_budg * big_cities$index
big_cities$mun_debt_budg_c_pc <- big_cities$mun_debt_budg_c / big_cities$population

big_cities$mun_debt_bank_c <- big_cities$mun_debt_bank * big_cities$index
big_cities$mun_debt_bank_c_pc <- big_cities$mun_debt_bank_c / big_cities$population

big_cities$mun_firms_profit_c <- big_cities$mun_firms_profit * big_cities$index
big_cities$mun_firms_profit_c_pc <- big_cities$mun_firms_profit_c / big_cities$population

big_cities$debt_to_mun_firms_c <- big_cities$debt_to_mun_firms * big_cities$index
big_cities$debt_to_mun_firms_c_pc <- big_cities$debt_to_mun_firms_c / big_cities$population

# показатели муниципальных предприятия в расчете на предприятие, предоставившее отчетность
big_cities$mun_debt_c_pf <- big_cities$mun_debt_c / big_cities$n_mun_firms_reported
big_cities$mun_debt_supl_c_pf <- big_cities$mun_debt_supl_c / big_cities$n_mun_firms_reported
big_cities$mun_debt_gov_c_pf <- big_cities$mun_debt_gov_c / big_cities$n_mun_firms_reported
big_cities$mun_debt_budg_c_pf <- big_cities$mun_debt_budg_c / big_cities$n_mun_firms_reported
big_cities$mun_debt_bank_c_pf <- big_cities$mun_debt_bank_c / big_cities$n_mun_firms_reported
big_cities$mun_firms_profit_c_pf <- big_cities$mun_firms_profit_c / big_cities$n_mun_firms_reported
big_cities$debt_to_mun_firms_c_pf <- big_cities$debt_to_mun_firms_c / big_cities$n_mun_firms_reported

# ln
big_cities$ln_t8013002_1_c_pc <- log(big_cities$t8013002_1_c_pc)
big_cities$ln_t8013002_212_c_pc <- log(big_cities$t8013002_212_c_pc)
big_cities$ln_t8013002_221_c_pc <- log(big_cities$t8013002_221_c_pc)
big_cities$ln_t8013002_229_c_pc <- log(big_cities$t8013002_229_c_pc)
big_cities$ln_t8013002_234_c_pc <- log(big_cities$t8013002_234_c_pc)
big_cities$ln_t8013002_239_c_pc <- log(big_cities$t8013002_239_c_pc)
big_cities$ln_investment_c_pc <- log(big_cities$investment_c_pc)
big_cities$ln_t8009001_c_pc <- log(big_cities$t8009001_c_pc + 1e-5)
big_cities$ln_t8013001_1_c_pc <- log(big_cities$t8013001_1_c_pc)
big_cities$ln_t8013001_5_c_pc <- log(big_cities$t8013001_5_c_pc)
big_cities$ln_t8013001_15_c_pc <- log(big_cities$t8013001_15_c_pc)
big_cities$ln_t8013001_14_c_pc <- log(big_cities$t8013001_14_c_pc)
big_cities$ln_t8013001_296_c_pc <- log(big_cities$t8013001_296_c_pc)
big_cities$ln_t8013001_294_c_pc <- log(big_cities$t8013001_294_c_pc)
big_cities$ln_t8013001_293_c_pc <- log(big_cities$t8013001_293_c_pc + 1e-5)
big_cities$ln_t8013001_89_c_pc <- log(big_cities$t8013001_89_c_pc)
big_cities$ln_t8013001_34_c_pc <- log(big_cities$t8013001_34_c_pc)

big_cities$ln_catering_c_pc <- log(big_cities$catering_c_pc)
big_cities$ln_construction_c_pc <- log(big_cities$construction_c_pc)
big_cities$ln_retail_c_pc <- log(big_cities$retail_c_pc)
big_cities$ln_volume_electr_c_pc <- log(big_cities$volume_electr_c_pc)
big_cities$ln_volume_manufact_c_pc <- log(big_cities$volume_manufact_c_pc)
big_cities$ln_doctors_per10 <- log(big_cities$doctors_per10)
big_cities$ln_living_space <- log(big_cities$living_space)
big_cities$ln_n_companies <- log(big_cities$n_companies)
big_cities$ln_workers <- log(big_cities$workers)
big_cities$ln_t8006003 <- log(big_cities$t8006003)
big_cities$ln_pension_c <- log(big_cities$pension_c)
big_cities$ln_mun_debt_c_pc <- log(big_cities$mun_debt_c_pc)
big_cities$ln_mun_debt_c_pf <- log(big_cities$mun_debt_c_pf)
big_cities$ln_mun_firms_profit_c_pf <- log(big_cities$mun_firms_profit_c_pf)
big_cities$ln_n_mun_firms_reported <- log(big_cities$n_mun_firms_reported)


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

# все расходы на душу населения с минимальным набором контролей
y_var <- "ln_t8013002_1_c_pc"
cov_vars <- c("log_build_flat", "log_new_housing", "ln_catering_c_pc", "ln_construction_c_pc",
              "ln_retail_c_pc", "ln_volume_electr_c_pc", "ln_volume_manufact_c_pc", "ln_doctors_per10",
              "ln_living_space", "ln_n_companies", "pop_work", "log_population", "log_wage",
              "ln_workers", "ln_t8006003", "ln_pension_c")

data <- big_cities %>% 
  filter(group != "unexpected") %>% 
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(ln_t8013002_1_c_pc ~ i(time_to_treat, treat, ref = -1) + 
                   log_build_flat + log_new_housing + ln_catering_c_pc + ln_construction_c_pc +
                   ln_retail_c_pc + ln_volume_electr_c_pc + ln_volume_manufact_c_pc + ln_doctors_per10 +
                   ln_living_space + ln_n_companies + pop_work + log_population + log_wage +
                   ln_workers + ln_t8006003 + ln_pension_c | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

mod_twfe_total = feols(ln_t8013002_1_c_pc ~ treatment +       
                         log_build_flat + log_new_housing + ln_catering_c_pc + ln_construction_c_pc +
                         ln_retail_c_pc + ln_volume_electr_c_pc + ln_volume_manufact_c_pc + ln_doctors_per10 +
                         ln_living_space + ln_n_companies + pop_work + log_population + log_wage +
                         ln_workers + ln_t8006003 + ln_pension_c |                   
                         settlement + year,                                           
                       cluster = ~region,                                             
                       data = data)
summary(mod_twfe_total)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(ln_t8013002_1_c_pc ~ sunab(first.treat, year) + 
                 log_build_flat + log_new_housing + ln_catering_c_pc + ln_construction_c_pc +
                 ln_retail_c_pc + ln_volume_electr_c_pc + ln_volume_manufact_c_pc + ln_doctors_per10 +
                 ln_living_space + ln_n_companies + pop_work + log_population + log_wage +
                 ln_workers + ln_t8006003 + ln_pension_c | 
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
y_var <- "ln_t8013002_212_c_pc"
cov_vars <- c("log_build_flat", "log_new_housing", "ln_catering_c_pc", "ln_construction_c_pc",
              "ln_retail_c_pc", "ln_volume_electr_c_pc", "ln_volume_manufact_c_pc", "ln_doctors_per10",
              "ln_living_space", "ln_n_companies", "pop_work", "log_population", "log_wage",
              "ln_workers", "ln_t8006003", "ln_pension_c")

data <- big_cities %>%    
  filter(group != "unexpected") %>%  
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(ln_t8013002_212_c_pc ~ i(time_to_treat, treat, ref = -1) + 
                   log_build_flat + log_new_housing + ln_catering_c_pc + ln_construction_c_pc +
                   ln_retail_c_pc + ln_volume_electr_c_pc + ln_volume_manufact_c_pc + ln_doctors_per10 +
                   ln_living_space + ln_n_companies + pop_work + log_population + log_wage +
                   ln_workers + ln_t8006003 + ln_pension_c | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

mod_twfe_total = feols(ln_t8013002_212_c_pc ~ treatment +       
                         log_build_flat + log_new_housing + ln_catering_c_pc + ln_construction_c_pc +
                         ln_retail_c_pc + ln_volume_electr_c_pc + ln_volume_manufact_c_pc + doctors_per10 +
                         ln_living_space + ln_n_companies + pop_work + log_population + log_wage +
                         ln_workers + ln_t8006003 + ln_pension_c |                   
                         settlement + year,                                           
                       cluster = ~region,                                             
                       data = data)
summary(mod_twfe_total)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(ln_t8013002_212_c_pc ~ sunab(first.treat, year) + 
                 log_build_flat + log_new_housing + ln_catering_c_pc + ln_construction_c_pc +
                 ln_retail_c_pc + ln_volume_electr_c_pc + ln_volume_manufact_c_pc + doctors_per10 +
                 ln_living_space + ln_n_companies + pop_work + log_population + log_wage +
                 ln_workers + ln_t8006003 + ln_pension_c | 
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
y_var <- "ln_t8013002_229_c_pc" # 10%
cov_vars <- c("log_build_flat", "log_new_housing", "ln_catering_c_pc", "ln_construction_c_pc",
              "ln_retail_c_pc", "ln_volume_electr_c_pc", "ln_volume_manufact_c_pc", "ln_doctors_per10",
              "ln_living_space", "ln_n_companies", "pop_work", "log_population", "log_wage",
              "ln_workers", "ln_t8006003", "ln_pension_c")

data <- big_cities %>%    
  filter(group != "unexpected") %>%  
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(ln_t8013002_229_c_pc ~ i(time_to_treat, treat, ref = -1) + 
                   log_build_flat + log_new_housing + ln_catering_c_pc + ln_construction_c_pc +
                   ln_retail_c_pc + ln_volume_electr_c_pc + ln_volume_manufact_c_pc + ln_doctors_per10 +
                   ln_living_space + ln_n_companies + pop_work + log_population + log_wage +
                   ln_workers + ln_t8006003 + ln_pension_c | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

mod_twfe_total = feols(ln_t8013002_229_c_pc ~ treatment +       
                         log_build_flat + log_new_housing + ln_catering_c_pc + ln_construction_c_pc +
                         ln_retail_c_pc + ln_volume_electr_c_pc + ln_volume_manufact_c_pc + ln_doctors_per10 +
                         ln_living_space + ln_n_companies + pop_work + log_population + log_wage +
                         ln_workers + ln_t8006003 + ln_pension_c |                   
                         settlement + year,                                           
                       cluster = ~region,                                             
                       data = data)
summary(mod_twfe_total)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(ln_t8013002_229_c_pc ~ sunab(first.treat, year) + 
                 log_build_flat + log_new_housing + ln_catering_c_pc + ln_construction_c_pc +
                 ln_retail_c_pc + ln_volume_electr_c_pc + ln_volume_manufact_c_pc + ln_doctors_per10 +
                 ln_living_space + ln_n_companies + pop_work + log_population + log_wage +
                 ln_workers + ln_t8006003 + ln_pension_c | 
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
y_var <- "ln_t8013002_234_c_pc" # 10%
cov_vars <- c("log_build_flat", "log_new_housing", "ln_catering_c_pc", "ln_construction_c_pc",
              "ln_retail_c_pc", "ln_volume_electr_c_pc", "ln_volume_manufact_c_pc", "ln_doctors_per10",
              "ln_living_space", "ln_n_companies", "pop_work", "log_population", "log_wage",
              "ln_workers", "ln_t8006003", "ln_pension_c")

data <- big_cities %>%    
  filter(group != "unexpected") %>%  
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(ln_t8013002_234_c_pc ~ i(time_to_treat, treat, ref = -1) + 
                   log_build_flat + log_new_housing + ln_catering_c_pc + ln_construction_c_pc +
                   ln_retail_c_pc + ln_volume_electr_c_pc + ln_volume_manufact_c_pc + ln_doctors_per10 +
                   ln_living_space + ln_n_companies + pop_work + log_population + log_wage +
                   ln_workers + ln_t8006003 + ln_pension_c | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

mod_twfe_total = feols(ln_t8013002_234_c_pc ~ treatment +       
                         log_build_flat + log_new_housing + ln_catering_c_pc + ln_construction_c_pc +
                         ln_retail_c_pc + ln_volume_electr_c_pc + ln_volume_manufact_c_pc + ln_doctors_per10 +
                         ln_living_space + ln_n_companies + pop_work + log_population + log_wage +
                         ln_workers + ln_t8006003 + ln_pension_c |                   
                         settlement + year,                                           
                       cluster = ~region,                                             
                       data = data)
summary(mod_twfe_total)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(ln_t8013002_234_c_pc ~ sunab(first.treat, year) + 
                 log_build_flat + log_new_housing + ln_catering_c_pc + ln_construction_c_pc +
                 ln_retail_c_pc + ln_volume_electr_c_pc + ln_volume_manufact_c_pc + ln_doctors_per10 +
                 ln_living_space + ln_n_companies + pop_work + log_population + log_wage +
                 ln_workers + ln_t8006003 + ln_pension_c | 
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
cov_vars <- c("log_build_flat", "log_new_housing", "ln_catering_c_pc", "ln_construction_c_pc",
              "ln_retail_c_pc", "ln_volume_electr_c_pc", "ln_volume_manufact_c_pc", "ln_doctors_per10",
              "ln_living_space", "ln_n_companies", "pop_work", "log_population", "log_wage",
              "ln_workers", "ln_t8006003", "ln_pension_c")

data <- big_cities %>%    
  filter(group != "unexpected") %>%  
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(invest_budg ~ i(time_to_treat, treat, ref = -1) + 
                   log_build_flat + log_new_housing + ln_catering_c_pc + ln_construction_c_pc +
                   ln_retail_c_pc + ln_volume_electr_c_pc + ln_volume_manufact_c_pc + ln_doctors_per10 +
                   ln_living_space + ln_n_companies + pop_work + log_population + log_wage +
                   ln_workers + ln_t8006003 + ln_pension_c | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

mod_twfe_total = feols(invest_budg ~ treatment +       
                         log_build_flat + log_new_housing + ln_catering_c_pc + ln_construction_c_pc +
                         ln_retail_c_pc + ln_volume_electr_c_pc + ln_volume_manufact_c_pc + ln_doctors_per10 +
                         ln_living_space + ln_n_companies + pop_work + log_population + log_wage +
                         ln_workers + ln_t8006003 + ln_pension_c |                   
                         settlement + year,                                           
                       cluster = ~region,                                             
                       data = data)
summary(mod_twfe_total)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(invest_budg ~ sunab(first.treat, year) + 
                 log_build_flat + log_new_housing + ln_catering_c_pc + ln_construction_c_pc +
                 ln_retail_c_pc + ln_volume_electr_c_pc + ln_volume_manufact_c_pc + ln_doctors_per10 +
                 ln_living_space + ln_n_companies + pop_work + log_population + log_wage +
                 ln_workers + ln_t8006003 + ln_pension_c | 
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
cov_vars <- c("log_build_flat", "log_new_housing", "ln_catering_c_pc", "ln_construction_c_pc",
              "ln_retail_c_pc", "ln_volume_electr_c_pc", "ln_volume_manufact_c_pc", "ln_doctors_per10",
              "ln_living_space", "ln_n_companies", "pop_work", "log_population", "log_wage",
              "ln_workers", "ln_t8006003", "ln_pension_c")

data <- big_cities %>%    
  filter(group != "unexpected") %>%  
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(invest_fed ~ i(time_to_treat, treat, ref = -1) + 
                   log_build_flat + log_new_housing + ln_catering_c_pc + ln_construction_c_pc +
                   ln_retail_c_pc + ln_volume_electr_c_pc + ln_volume_manufact_c_pc + ln_doctors_per10 +
                   ln_living_space + ln_n_companies + pop_work + log_population + log_wage +
                   ln_workers + ln_t8006003 + ln_pension_c | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

mod_twfe_total = feols(invest_fed ~ treatment +       
                         log_build_flat + log_new_housing + ln_catering_c_pc + ln_construction_c_pc +
                         ln_retail_c_pc + ln_volume_electr_c_pc + ln_volume_manufact_c_pc + ln_doctors_per10 +
                         ln_living_space + ln_n_companies + pop_work + log_population + log_wage +
                         ln_workers + ln_t8006003 + ln_pension_c |                   
                         settlement + year,                                           
                       cluster = ~region,                                             
                       data = data)
summary(mod_twfe_total)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(invest_fed ~ sunab(first.treat, year) + 
                 log_build_flat + log_new_housing + ln_catering_c_pc + ln_construction_c_pc +
                 ln_retail_c_pc + ln_volume_electr_c_pc + ln_volume_manufact_c_pc + ln_doctors_per10 +
                 ln_living_space + ln_n_companies + pop_work + log_population + log_wage +
                 ln_workers + ln_t8006003 + ln_pension_c | 
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
y_var <- "ln_investment_c_pc"
cov_vars <- c("log_build_flat", "log_new_housing", "ln_catering_c_pc", "ln_construction_c_pc",
              "ln_retail_c_pc", "ln_volume_electr_c_pc", "ln_volume_manufact_c_pc", "ln_doctors_per10",
              "ln_living_space", "ln_n_companies", "pop_work", "log_population", "log_wage",
              "ln_workers", "ln_t8006003", "ln_pension_c")

data <- big_cities %>%    
  filter(group != "unexpected") %>%  
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(ln_investment_c_pc ~ i(time_to_treat, treat, ref = -1) + 
                   log_build_flat + log_new_housing + ln_catering_c_pc + ln_construction_c_pc +
                   ln_retail_c_pc + ln_volume_electr_c_pc + ln_volume_manufact_c_pc + ln_doctors_per10 +
                   ln_living_space + ln_n_companies + pop_work + log_population + log_wage +
                   ln_workers + ln_t8006003 + ln_pension_c | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

mod_twfe_total = feols(ln_investment_c_pc ~ treatment +       
                         log_build_flat + log_new_housing + ln_catering_c_pc + ln_construction_c_pc +
                         ln_retail_c_pc + ln_volume_electr_c_pc + ln_volume_manufact_c_pc + ln_doctors_per10 +
                         ln_living_space + ln_n_companies + pop_work + log_population + log_wage +
                         ln_workers + ln_t8006003 + ln_pension_c |                   
                         settlement + year,                                           
                       cluster = ~region,                                             
                       data = data)
summary(mod_twfe_total)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(ln_investment_c_pc ~ sunab(first.treat, year) + 
                 log_build_flat + log_new_housing + ln_catering_c_pc + ln_construction_c_pc +
                 ln_retail_c_pc + ln_volume_electr_c_pc + ln_volume_manufact_c_pc + ln_doctors_per10 +
                 ln_living_space + ln_n_companies + pop_work + log_population + log_wage +
                 ln_workers + ln_t8006003 + ln_pension_c | 
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
y_var <- "ln_t8009001_c_pc"
cov_vars <- c("log_build_flat", "log_new_housing", "ln_catering_c_pc", "ln_construction_c_pc",
              "ln_retail_c_pc", "ln_volume_electr_c_pc", "ln_volume_manufact_c_pc", "ln_doctors_per10",
              "ln_living_space", "ln_n_companies", "pop_work", "log_population", "log_wage",
              "ln_workers", "ln_t8006003", "ln_pension_c")

data <- big_cities %>%    
  filter(group != "unexpected") %>%  
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(ln_t8009001_c_pc ~ i(time_to_treat, treat, ref = -1) + 
                   log_build_flat + log_new_housing + ln_catering_c_pc + ln_construction_c_pc +
                   ln_retail_c_pc + ln_volume_electr_c_pc + ln_volume_manufact_c_pc + ln_doctors_per10 +
                   ln_living_space + ln_n_companies + pop_work + log_population + log_wage +
                   ln_workers + ln_t8006003 + ln_pension_c | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

mod_twfe_total = feols(ln_t8009001_c_pc ~ treatment +       
                         log_build_flat + log_new_housing + ln_catering_c_pc + ln_construction_c_pc +
                         ln_retail_c_pc + ln_volume_electr_c_pc + ln_volume_manufact_c_pc + ln_doctors_per10 +
                         ln_living_space + ln_n_companies + pop_work + log_population + log_wage +
                         ln_workers + ln_t8006003 + ln_pension_c |                   
                         settlement + year,                                           
                       cluster = ~region,                                             
                       data = data)
summary(mod_twfe_total)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(ln_t8009001_c_pc ~ sunab(first.treat, year) + 
                 log_build_flat + log_new_housing + ln_catering_c_pc + ln_construction_c_pc +
                 ln_retail_c_pc + ln_volume_electr_c_pc + ln_volume_manufact_c_pc + ln_doctors_per10 +
                 ln_living_space + ln_n_companies + pop_work + log_population + log_wage +
                 ln_workers + ln_t8006003 + ln_pension_c | 
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

# доля водопроводной сети нуждающейся в замене с минимальным набором контролей
y_var <- "t8008008_t8008007"
cov_vars <- c("log_build_flat", "log_new_housing", "ln_catering_c_pc", "ln_construction_c_pc",
              "ln_retail_c_pc", "ln_volume_electr_c_pc", "ln_volume_manufact_c_pc", "ln_doctors_per10",
              "ln_living_space", "ln_n_companies", "pop_work", "log_population", "log_wage",
              "ln_workers", "ln_t8006003", "ln_pension_c")

data <- big_cities %>%    
  filter(group != "unexpected") %>%  
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(t8008008_t8008007 ~ i(time_to_treat, treat, ref = -1) + 
                   log_build_flat + log_new_housing + ln_catering_c_pc + ln_construction_c_pc +
                   ln_retail_c_pc + ln_volume_electr_c_pc + ln_volume_manufact_c_pc + ln_doctors_per10 +
                   ln_living_space + ln_n_companies + pop_work + log_population + log_wage +
                   ln_workers + ln_t8006003 + ln_pension_c | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

mod_twfe_total = feols(t8008008_t8008007 ~ treatment +       
                         log_build_flat + log_new_housing + ln_catering_c_pc + ln_construction_c_pc +
                         ln_retail_c_pc + ln_volume_electr_c_pc + ln_volume_manufact_c_pc + ln_doctors_per10 +
                         ln_living_space + ln_n_companies + pop_work + log_population + log_wage +
                         ln_workers + ln_t8006003 + ln_pension_c |                   
                         settlement + year,                                           
                       cluster = ~region,                                             
                       data = data)
summary(mod_twfe_total)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(t8008008_t8008007 ~ sunab(first.treat, year) + 
                 log_build_flat + log_new_housing + ln_catering_c_pc + ln_construction_c_pc +
                 ln_retail_c_pc + ln_volume_electr_c_pc + ln_volume_manufact_c_pc + ln_doctors_per10 +
                 ln_living_space + ln_n_companies + pop_work + log_population + log_wage +
                 ln_workers + ln_t8006003 + ln_pension_c | 
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

# все доходы на душу с минимальным набором контролей
y_var <- "ln_t8013001_1_c_pc"
cov_vars <- c("log_build_flat", "log_new_housing", "ln_catering_c_pc", "ln_construction_c_pc",
              "ln_retail_c_pc", "ln_volume_electr_c_pc", "ln_volume_manufact_c_pc", "ln_doctors_per10",
              "ln_living_space", "ln_n_companies", "pop_work", "log_population", "log_wage",
              "ln_workers", "ln_t8006003", "ln_pension_c")

data <- big_cities %>%    
  filter(group != "unexpected") %>%  
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(ln_t8013001_1_c_pc ~ i(time_to_treat, treat, ref = -1) + 
                   log_build_flat + log_new_housing + ln_catering_c_pc + ln_construction_c_pc +
                   ln_retail_c_pc + ln_volume_electr_c_pc + ln_volume_manufact_c_pc + ln_doctors_per10 +
                   ln_living_space + ln_n_companies + pop_work + log_population + log_wage +
                   ln_workers + ln_t8006003 + ln_pension_c | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

mod_twfe_total = feols(ln_t8013001_1_c_pc ~ treatment +       
                         log_build_flat + log_new_housing + ln_catering_c_pc + ln_construction_c_pc +
                         ln_retail_c_pc + ln_volume_electr_c_pc + ln_volume_manufact_c_pc + ln_doctors_per10 +
                         ln_living_space + ln_n_companies + pop_work + log_population + log_wage +
                         ln_workers + ln_t8006003 + ln_pension_c |                   
                         settlement + year,                                           
                       cluster = ~region,                                             
                       data = data)
summary(mod_twfe_total)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(ln_t8013001_1_c_pc ~ sunab(first.treat, year) + 
                 log_build_flat + log_new_housing + ln_catering_c_pc + ln_construction_c_pc +
                 ln_retail_c_pc + ln_volume_electr_c_pc + ln_volume_manufact_c_pc + ln_doctors_per10 +
                 ln_living_space + ln_n_companies + pop_work + log_population + log_wage +
                 ln_workers + ln_t8006003 + ln_pension_c | 
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

# доходы от НДФЛ на душу с минимальным набором контролей
y_var <- "ln_t8013001_5_c_pc" # 10 %
cov_vars <- c("log_build_flat", 
              "ln_retail_c_pc", "ln_volume_electr_c_pc", "ln_volume_manufact_c_pc", "ln_doctors_per10",
              "ln_living_space", "ln_n_companies", "pop_work", "log_population", "log_wage",
              "ln_workers", "ln_t8006003", "ln_pension_c")

data <- big_cities %>%    
  filter(group != "unexpected") %>%  
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(ln_t8013001_5_c_pc ~ i(time_to_treat, treat, ref = -1) + 
                   log_build_flat + 
                   ln_retail_c_pc + ln_volume_electr_c_pc + ln_volume_manufact_c_pc + ln_doctors_per10 +
                   ln_living_space + ln_n_companies + pop_work + log_population + log_wage +
                   ln_workers + ln_t8006003 + ln_pension_c | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

mod_twfe_total = feols(ln_t8013001_5_c_pc ~ treatment +       
                         log_build_flat + 
                         ln_retail_c_pc + ln_volume_electr_c_pc + ln_volume_manufact_c_pc + ln_doctors_per10 +
                         ln_living_space + ln_n_companies + pop_work + log_population + log_wage +
                         ln_workers + ln_t8006003 + ln_pension_c |                   
                         settlement + year,                                           
                       cluster = ~region,                                             
                       data = data)
summary(mod_twfe_total)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(ln_t8013001_5_c_pc ~ sunab(first.treat, year) + 
                 log_build_flat +
                 ln_retail_c_pc + ln_volume_electr_c_pc + ln_volume_manufact_c_pc + ln_doctors_per10 +
                 ln_living_space + ln_n_companies + pop_work + log_population + log_wage +
                 ln_workers + ln_t8006003 + ln_pension_c | 
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

# доходы от налогов на имущество на душу с минимальным набором контролей
y_var <- "ln_t8013001_15_c_pc"
cov_vars <- c("log_build_flat", "log_new_housing", "ln_catering_c_pc", "ln_construction_c_pc",
              "ln_retail_c_pc", "ln_volume_electr_c_pc", "ln_volume_manufact_c_pc", "ln_doctors_per10",
              "ln_living_space", "ln_n_companies", "pop_work", "log_population", "log_wage",
              "ln_workers", "ln_t8006003", "ln_pension_c")

data <- big_cities %>%    
  filter(group != "unexpected") %>%  
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(ln_t8013001_15_c_pc ~ i(time_to_treat, treat, ref = -1) + 
                   log_build_flat + log_new_housing + ln_catering_c_pc + ln_construction_c_pc +
                   ln_retail_c_pc + ln_volume_electr_c_pc + ln_volume_manufact_c_pc + ln_doctors_per10 +
                   ln_living_space + ln_n_companies + pop_work + log_population + log_wage +
                   ln_workers + ln_t8006003 + ln_pension_c | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

mod_twfe_total = feols(ln_t8013001_15_c_pc ~ treatment +       
                         log_build_flat + log_new_housing + ln_catering_c_pc + ln_construction_c_pc +
                         ln_retail_c_pc + ln_volume_electr_c_pc + ln_volume_manufact_c_pc + ln_doctors_per10 +
                         ln_living_space + ln_n_companies + pop_work + log_population + log_wage +
                         ln_workers + ln_t8006003 + ln_pension_c |                   
                         settlement + year,                                           
                       cluster = ~region,                                             
                       data = data)
summary(mod_twfe_total)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(ln_t8013001_15_c_pc ~ sunab(first.treat, year) + 
                 log_build_flat + log_new_housing + ln_catering_c_pc + ln_construction_c_pc +
                 ln_retail_c_pc + ln_volume_electr_c_pc + ln_volume_manufact_c_pc + ln_doctors_per10 +
                 ln_living_space + ln_n_companies + pop_work + log_population + log_wage +
                 ln_workers + ln_t8006003 + ln_pension_c | 
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

# доходы от налогов на совокупный доход на душу с минимальным набором контролей
y_var <- "ln_t8013001_14_c_pc"
cov_vars <- c("log_build_flat", "log_new_housing", "ln_catering_c_pc", "ln_construction_c_pc",
              "ln_retail_c_pc", "ln_volume_electr_c_pc", "ln_volume_manufact_c_pc", "ln_doctors_per10",
              "ln_living_space", "ln_n_companies", "pop_work", "log_population", "log_wage",
              "ln_workers", "ln_t8006003", "ln_pension_c")

data <- big_cities %>%    
  filter(group != "unexpected") %>%  
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(ln_t8013001_14_c_pc ~ i(time_to_treat, treat, ref = -1) + 
                   log_build_flat + log_new_housing + ln_catering_c_pc + ln_construction_c_pc +
                   ln_retail_c_pc + ln_volume_electr_c_pc + ln_volume_manufact_c_pc + ln_doctors_per10 +
                   ln_living_space + ln_n_companies + pop_work + log_population + log_wage +
                   ln_workers + ln_t8006003 + ln_pension_c | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

mod_twfe_total = feols(ln_t8013001_14_c_pc ~ treatment +       
                         log_build_flat + log_new_housing + ln_catering_c_pc + ln_construction_c_pc +
                         ln_retail_c_pc + ln_volume_electr_c_pc + ln_volume_manufact_c_pc + ln_doctors_per10 +
                         ln_living_space + ln_n_companies + pop_work + log_population + log_wage +
                         ln_workers + ln_t8006003 + ln_pension_c |                   
                         settlement + year,                                           
                       cluster = ~region,                                             
                       data = data)
summary(mod_twfe_total)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(ln_t8013001_14_c_pc ~ sunab(first.treat, year) + 
                 log_build_flat + log_new_housing + ln_catering_c_pc + ln_construction_c_pc +
                 ln_retail_c_pc + ln_volume_electr_c_pc + ln_volume_manufact_c_pc + ln_doctors_per10 +
                 ln_living_space + ln_n_companies + pop_work + log_population + log_wage +
                 ln_workers + ln_t8006003 + ln_pension_c | 
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

# субсидии на душу с минимальным набором контролей
y_var <- "ln_t8013001_296_c_pc" # 5 %
cov_vars <- c("log_build_flat", "log_new_housing", "ln_catering_c_pc", "ln_construction_c_pc",
              "ln_retail_c_pc", "ln_volume_electr_c_pc", "ln_volume_manufact_c_pc", "ln_doctors_per10",
              "ln_living_space", "ln_n_companies", "pop_work", "log_population", "log_wage",
              "ln_workers", "ln_t8006003", "ln_pension_c")

data <- big_cities %>%    
  filter(group != "unexpected") %>%  
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(ln_t8013001_296_c_pc ~ i(time_to_treat, treat, ref = -1) + 
                   log_build_flat + log_new_housing + ln_catering_c_pc + ln_construction_c_pc +
                   ln_retail_c_pc + ln_volume_electr_c_pc + ln_volume_manufact_c_pc + ln_doctors_per10 +
                   ln_living_space + ln_n_companies + pop_work + log_population + log_wage +
                   ln_workers + ln_t8006003 + ln_pension_c | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

mod_twfe_total = feols(ln_t8013001_296_c_pc ~ treatment +       
                         log_build_flat + log_new_housing + ln_catering_c_pc + ln_construction_c_pc +
                         ln_retail_c_pc + ln_volume_electr_c_pc + ln_volume_manufact_c_pc + ln_doctors_per10 +
                         ln_living_space + ln_n_companies + pop_work + log_population + log_wage +
                         ln_workers + ln_t8006003 + ln_pension_c |                   
                         settlement + year,                                           
                       cluster = ~region,                                             
                       data = data)
summary(mod_twfe_total)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(ln_t8013001_296_c_pc ~ sunab(first.treat, year) + 
                 log_build_flat + log_new_housing + ln_catering_c_pc + ln_construction_c_pc +
                 ln_retail_c_pc + ln_volume_electr_c_pc + ln_volume_manufact_c_pc + ln_doctors_per10 +
                 ln_living_space + ln_n_companies + pop_work + log_population + log_wage +
                 ln_workers + ln_t8006003 + ln_pension_c | 
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

# субвенции на душу с минимальным набором контролей
y_var <- "ln_t8013001_294_c_pc"
cov_vars <- c("log_build_flat", "log_new_housing", "ln_catering_c_pc", "ln_construction_c_pc",
              "ln_retail_c_pc", "ln_volume_electr_c_pc", "ln_volume_manufact_c_pc", "ln_doctors_per10",
              "ln_living_space", "ln_n_companies", "pop_work", "log_population", "log_wage",
              "ln_workers", "ln_t8006003", "ln_pension_c")

data <- big_cities %>%    
  filter(group != "unexpected") %>%  
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(ln_t8013001_294_c_pc ~ i(time_to_treat, treat, ref = -1) + 
                   log_build_flat + log_new_housing + ln_catering_c_pc + ln_construction_c_pc +
                   ln_retail_c_pc + ln_volume_electr_c_pc + ln_volume_manufact_c_pc + ln_doctors_per10 +
                   ln_living_space + ln_n_companies + pop_work + log_population + log_wage +
                   ln_workers + ln_t8006003 + ln_pension_c | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

mod_twfe_total = feols(ln_t8013001_294_c_pc ~ treatment +       
                         log_build_flat + log_new_housing + ln_catering_c_pc + ln_construction_c_pc +
                         ln_retail_c_pc + ln_volume_electr_c_pc + ln_volume_manufact_c_pc + ln_doctors_per10 +
                         ln_living_space + ln_n_companies + pop_work + log_population + log_wage +
                         ln_workers + ln_t8006003 + ln_pension_c |                   
                         settlement + year,                                           
                       cluster = ~region,                                             
                       data = data)
summary(mod_twfe_total)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(ln_t8013001_294_c_pc ~ sunab(first.treat, year) + 
                 log_build_flat + log_new_housing + ln_catering_c_pc + ln_construction_c_pc +
                 ln_retail_c_pc + ln_volume_electr_c_pc + ln_volume_manufact_c_pc + ln_doctors_per10 +
                 ln_living_space + ln_n_companies + pop_work + log_population + log_wage +
                 ln_workers + ln_t8006003 + ln_pension_c | 
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

# дотации на душу с минимальным набором контролей
y_var <- "ln_t8013001_293_c_pc"
cov_vars <- c("log_build_flat", "log_new_housing", "ln_catering_c_pc", "ln_construction_c_pc",
              "ln_retail_c_pc", "ln_volume_electr_c_pc", "ln_volume_manufact_c_pc", "ln_doctors_per10",
              "ln_living_space", "ln_n_companies", "pop_work", "log_population", "log_wage",
              "ln_workers", "ln_t8006003", "ln_pension_c")

data <- big_cities %>%    
  filter(group != "unexpected") %>%  
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(ln_t8013001_293_c_pc ~ i(time_to_treat, treat, ref = -1) + 
                   log_build_flat + log_new_housing + ln_catering_c_pc + ln_construction_c_pc +
                   ln_retail_c_pc + ln_volume_electr_c_pc + ln_volume_manufact_c_pc + ln_doctors_per10 +
                   ln_living_space + ln_n_companies + pop_work + log_population + log_wage +
                   ln_workers + ln_t8006003 + ln_pension_c | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

mod_twfe_total = feols(ln_t8013001_293_c_pc ~ treatment +       
                         log_build_flat + log_new_housing + ln_catering_c_pc + ln_construction_c_pc +
                         ln_retail_c_pc + ln_volume_electr_c_pc + ln_volume_manufact_c_pc + ln_doctors_per10 +
                         ln_living_space + ln_n_companies + pop_work + log_population + log_wage +
                         ln_workers + ln_t8006003 + ln_pension_c |                   
                         settlement + year,                                           
                       cluster = ~region,                                             
                       data = data)
summary(mod_twfe_total)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(ln_t8013001_293_c_pc ~ sunab(first.treat, year) + 
                 log_build_flat + log_new_housing + ln_catering_c_pc + ln_construction_c_pc +
                 ln_retail_c_pc + ln_volume_electr_c_pc + ln_volume_manufact_c_pc + ln_doctors_per10 +
                 ln_living_space + ln_n_companies + pop_work + log_population + log_wage +
                 ln_workers + ln_t8006003 + ln_pension_c | 
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

# собственные доходы на душу с минимальным набором контролей
y_var <- "ln_t8013001_89_c_pc" # 10%
cov_vars <- c("log_build_flat", "log_new_housing", "ln_catering_c_pc", "ln_construction_c_pc",
              "ln_retail_c_pc", "ln_volume_electr_c_pc", "ln_volume_manufact_c_pc", "ln_doctors_per10",
              "ln_living_space", "ln_n_companies", "pop_work", "log_population", "log_wage",
              "ln_workers", "ln_t8006003", "ln_pension_c")

data <- big_cities %>%    
  filter(group != "unexpected") %>%  
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(ln_t8013001_89_c_pc ~ i(time_to_treat, treat, ref = -1) + 
                   log_build_flat + log_new_housing + ln_catering_c_pc + ln_construction_c_pc +
                   ln_retail_c_pc + ln_volume_electr_c_pc + ln_volume_manufact_c_pc + ln_doctors_per10 +
                   ln_living_space + ln_n_companies + pop_work + log_population + log_wage +
                   ln_workers + ln_t8006003 + ln_pension_c | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

mod_twfe_total = feols(ln_t8013001_89_c_pc ~ treatment +       
                         log_build_flat + log_new_housing + ln_catering_c_pc + ln_construction_c_pc +
                         ln_retail_c_pc + ln_volume_electr_c_pc + ln_volume_manufact_c_pc + ln_doctors_per10 +
                         ln_living_space + ln_n_companies + pop_work + log_population + log_wage +
                         ln_workers + ln_t8006003 + ln_pension_c |                   
                         settlement + year,                                           
                       cluster = ~region,                                             
                       data = data)
summary(mod_twfe_total)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(ln_t8013001_89_c_pc ~ sunab(first.treat, year) + 
                 log_build_flat + log_new_housing + ln_catering_c_pc + ln_construction_c_pc +
                 ln_retail_c_pc + ln_volume_electr_c_pc + ln_volume_manufact_c_pc + ln_doctors_per10 +
                 ln_living_space + ln_n_companies + pop_work + log_population + log_wage +
                 ln_workers + ln_t8006003 + ln_pension_c | 
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

# безвозмездные поступления на душу с минимальным набором контролей
y_var <- "ln_t8013001_34_c_pc"
cov_vars <- c("log_build_flat", "log_new_housing", "ln_catering_c_pc", "ln_construction_c_pc",
              "ln_retail_c_pc", "ln_volume_electr_c_pc", "ln_volume_manufact_c_pc", "ln_doctors_per10",
              "ln_living_space", "ln_n_companies", "pop_work", "log_population", "log_wage",
              "ln_workers", "ln_t8006003", "ln_pension_c")

data <- big_cities %>%    
  filter(group != "unexpected") %>%  
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(ln_t8013001_34_c_pc ~ i(time_to_treat, treat, ref = -1) + 
                   log_build_flat + log_new_housing + ln_catering_c_pc + ln_construction_c_pc +
                   ln_retail_c_pc + ln_volume_electr_c_pc + ln_volume_manufact_c_pc + ln_doctors_per10 +
                   ln_living_space + ln_n_companies + pop_work + log_population + log_wage +
                   ln_workers + ln_t8006003 + ln_pension_c | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

mod_twfe_total = feols(ln_t8013001_34_c_pc ~ treatment +       
                         log_build_flat + log_new_housing + ln_catering_c_pc + ln_construction_c_pc +
                         ln_retail_c_pc + ln_volume_electr_c_pc + ln_volume_manufact_c_pc + ln_doctors_per10 +
                         ln_living_space + ln_n_companies + pop_work + log_population + log_wage +
                         ln_workers + ln_t8006003 + ln_pension_c |                   
                         settlement + year,                                           
                       cluster = ~region,                                             
                       data = data)
summary(mod_twfe_total)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(ln_t8013001_34_c_pc ~ sunab(first.treat, year) + 
                 log_build_flat + log_new_housing + ln_catering_c_pc + ln_construction_c_pc +
                 ln_retail_c_pc + ln_volume_electr_c_pc + ln_volume_manufact_c_pc + ln_doctors_per10 +
                 ln_living_space + ln_n_companies + pop_work + log_population + log_wage +
                 ln_workers + ln_t8006003 + ln_pension_c | 
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

# кредиторская задолженность на душу с минимальным набором контролей
y_var <- "ln_mun_debt_c_pf"
cov_vars <- c("log_build_flat", "log_new_housing", "ln_catering_c_pc", "ln_construction_c_pc",
              "ln_retail_c_pc", "ln_volume_electr_c_pc", "ln_volume_manufact_c_pc", "ln_doctors_per10",
              "ln_living_space", "ln_n_companies", "pop_work", "log_population", "log_wage",
              "ln_workers", "ln_t8006003", "ln_pension_c")

data <- big_cities %>%    
  filter(group != "unexpected") %>%  
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(ln_mun_debt_c_pf ~ i(time_to_treat, treat, ref = -1) + 
                   log_build_flat + log_new_housing + ln_catering_c_pc + ln_construction_c_pc +
                   ln_retail_c_pc + ln_volume_electr_c_pc + ln_volume_manufact_c_pc + ln_doctors_per10 +
                   ln_living_space + ln_n_companies + pop_work + log_population + log_wage +
                   ln_workers + ln_t8006003 + ln_pension_c | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

mod_twfe_total = feols(ln_mun_debt_c_pf ~ treatment +       
                         log_build_flat + log_new_housing + ln_catering_c_pc + ln_construction_c_pc +
                         ln_retail_c_pc + ln_volume_electr_c_pc + ln_volume_manufact_c_pc + ln_doctors_per10 +
                         ln_living_space + ln_n_companies + pop_work + log_population + log_wage +
                         ln_workers + ln_t8006003 + ln_pension_c |                   
                         settlement + year,                                           
                       cluster = ~region,                                             
                       data = data)
summary(mod_twfe_total)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(ln_mun_debt_c_pf ~ sunab(first.treat, year) + 
                 log_build_flat + log_new_housing + ln_catering_c_pc + ln_construction_c_pc +
                 ln_retail_c_pc + ln_volume_electr_c_pc + ln_volume_manufact_c_pc + ln_doctors_per10 +
                 ln_living_space + ln_n_companies + pop_work + log_population + log_wage +
                 ln_workers + ln_t8006003 + ln_pension_c | 
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

# прибыль муниципальных предприятий на предприятие с минимальным набором контролей
y_var <- "ln_mun_firms_profit_c_pf"
cov_vars <- c("log_build_flat", "log_new_housing", "ln_catering_c_pc", "ln_construction_c_pc",
              "ln_retail_c_pc", "ln_volume_electr_c_pc", "ln_volume_manufact_c_pc", "ln_doctors_per10",
              "ln_living_space", "ln_n_companies", "pop_work", "log_population", "log_wage",
              "ln_workers", "ln_t8006003", "ln_pension_c")

data <- big_cities %>%    
  filter(group != "unexpected") %>%  
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(ln_mun_firms_profit_c_pf ~ i(time_to_treat, treat, ref = -1) + 
                   log_build_flat + log_new_housing + ln_catering_c_pc + ln_construction_c_pc +
                   ln_retail_c_pc + ln_volume_electr_c_pc + ln_volume_manufact_c_pc + ln_doctors_per10 +
                   ln_living_space + ln_n_companies + pop_work + log_population + log_wage +
                   ln_workers + ln_t8006003 + ln_pension_c | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

mod_twfe_total = feols(ln_mun_firms_profit_c_pf ~ treatment +       
                         log_build_flat + log_new_housing + ln_catering_c_pc + ln_construction_c_pc +
                         ln_retail_c_pc + ln_volume_electr_c_pc + ln_volume_manufact_c_pc + ln_doctors_per10 +
                         ln_living_space + ln_n_companies + pop_work + log_population + log_wage +
                         ln_workers + ln_t8006003 + ln_pension_c |                   
                         settlement + year,                                           
                       cluster = ~region,                                             
                       data = data)
summary(mod_twfe_total)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(ln_mun_firms_profit_c_pf ~ sunab(first.treat, year) + 
                 log_build_flat + log_new_housing + ln_catering_c_pc + ln_construction_c_pc +
                 ln_retail_c_pc + ln_volume_electr_c_pc + ln_volume_manufact_c_pc + ln_doctors_per10 +
                 ln_living_space + ln_n_companies + pop_work + log_population + log_wage +
                 ln_workers + ln_t8006003 + ln_pension_c | 
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

# число отчитавшихся муниципальных предприятий с минимальным набором контролей
y_var <- "ln_n_mun_firms_reported"
cov_vars <- c("log_build_flat", "log_new_housing", "ln_catering_c_pc", "ln_construction_c_pc",
              "ln_retail_c_pc", "ln_volume_electr_c_pc", "ln_volume_manufact_c_pc", "ln_doctors_per10",
              "ln_living_space", "ln_n_companies", "pop_work", "log_population", "log_wage",
              "ln_workers", "ln_t8006003", "ln_pension_c")

data <- big_cities %>%    
  filter(group != "unexpected") %>%  
  select(c("settlement", "region", "year", "treat", "first.treat", 
           "time_to_treat", "treatment", y_var, all_of(cov_vars))) %>% 
  drop_na() %>% as.data.frame()

mod_twfe = feols(ln_n_mun_firms_reported ~ i(time_to_treat, treat, ref = -1) + 
                   log_build_flat + log_new_housing + ln_catering_c_pc + ln_construction_c_pc +
                   ln_retail_c_pc + ln_volume_electr_c_pc + ln_volume_manufact_c_pc + ln_doctors_per10 +
                   ln_living_space + ln_n_companies + pop_work + log_population + log_wage +
                   ln_workers + ln_t8006003 + ln_pension_c | 
                   settlement + year, 
                 cluster = ~region, 
                 data = data)

mod_twfe_total = feols(ln_n_mun_firms_reported ~ treatment +       
                         log_build_flat + log_new_housing + ln_catering_c_pc + ln_construction_c_pc +
                         ln_retail_c_pc + ln_volume_electr_c_pc + ln_volume_manufact_c_pc + ln_doctors_per10 +
                         ln_living_space + ln_n_companies + pop_work + log_population + log_wage +
                         ln_workers + ln_t8006003 + ln_pension_c |                   
                         settlement + year,                                           
                       cluster = ~region,                                             
                       data = data)
summary(mod_twfe_total)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa = feols(ln_n_mun_firms_reported ~ sunab(first.treat, year) + 
                 log_build_flat + log_new_housing + ln_catering_c_pc + ln_construction_c_pc +
                 ln_retail_c_pc + ln_volume_electr_c_pc + ln_volume_manufact_c_pc + ln_doctors_per10 +
                 ln_living_space + ln_n_companies + pop_work + log_population + log_wage +
                 ln_workers + ln_t8006003 + ln_pension_c | 
                 settlement + year,  
               cluster = ~region,  
               data = data)
summary(mod_sa, agg = "att")

iplot(list(mod_twfe, mod_sa), sep = 0.5, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2020)"), cex = 0.7)







