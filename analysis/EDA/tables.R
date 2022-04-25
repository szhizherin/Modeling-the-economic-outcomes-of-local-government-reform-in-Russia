# Purpose: estimate TWFE models on big_cities data
# Inputs:  intermediate_data/BDMO_id_name.csv
#          final_data/big_cities.csv
# Outputs: -




library(data.table)
library(dplyr)
library(fixest)
library(readr)
library(stargazer)
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

num_treated_and_never_treated <- function(data) {
  res <- data %>% 
    group_by(settlement) %>% 
    summarise(type = get_group(treatment)[1]) %>% 
    ungroup() %>% 
    count(type)
  return(res) 
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

big_cities$share_culture_workers <- big_cities$t8016002 / big_cities$workers # доля работников культуры
big_cities$wages_payed <- big_cities$wage_c * big_cities$workers

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


dep_vars <- c("t8013002_1_c_pc", "t8013002_212_c_pc", "t8013002_221_c_pc",
              "t8013002_229_c_pc", "t8013002_234_c_pc", "t8013002_239_c_pc",
              "invest_budg", "invest_fed", "investment_c_pc", "t8009001_c_pc",
              "t8008008_t8008007", "t8013001_1_c_pc", "t8013001_5_c_pc",
              "t8013001_15_c_pc", "t8013001_14_c_pc", "t8013001_296_c_pc",
              "t8013001_294_c_pc", "t8013001_293_c_pc", "t8013001_89_c_pc",
              "t8013001_34_c_pc", "t8123015_12")

dep_vars <- c("ln_t8013002_1_c_pc", "ln_t8013002_212_c_pc", "ln_t8013002_221_c_pc",
              "ln_t8013002_229_c_pc", "ln_t8013002_234_c_pc", "ln_t8013002_239_c_pc",
              "invest_budg", "invest_fed", "ln_investment_c_pc", "ln_t8009001_c_pc",
              "t8008008_t8008007", "ln_t8013001_1_c_pc", "ln_t8013001_5_c_pc",
              "ln_t8013001_15_c_pc", "ln_t8013001_14_c_pc", "ln_t8013001_296_c_pc",
              "ln_t8013001_294_c_pc", "ln_t8013001_293_c_pc", "ln_t8013001_89_c_pc",
              "ln_t8013001_34_c_pc", "t8123015_12")

data <- big_cities %>% select(all_of(dep_vars)) %>% as.data.frame()
data %>% stargazer(type = "text")













