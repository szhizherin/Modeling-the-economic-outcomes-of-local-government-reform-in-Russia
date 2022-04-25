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

big_cities$share_culture_workers <- big_cities$t8016002 / big_cities$workers # доля работников культуры

big_cities$invest_budg %>% is.na() %>% sum()
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





y_names <- c(# "Общий объем расходов бюджета муниципального образования|||t8313001|||Всего",
  #"Расходы бюджета муниципального образования на содержание работников органов местного самоуправления в расчете на одного жителя муниципального образования (2008г. - тысяч рублей)",
  #"Расходы бюджета муниципального образования на содержание работников органов местного самоуправления в расчете на одного жителя муниципального образования (2008г. - тыс. рублей)",
  #"Расходы местного бюджета, фактически исполненные|||t8013002|||Расходы на содержание работников органов местного самоуправления",
  "Расходы местного бюджета, фактически исполненные|||t8013002|||Всего",
  "Расходы местного бюджета, фактически исполненные|||t8013002|||Общегосударственные вопросы",
  "Расходы местного бюджета, фактически исполненные|||t8013002|||Национальная безопасность и правоохранительная деятельность",
  "Расходы местного бюджета, фактически исполненные|||t8013002|||Национальная экономика",
  "Расходы местного бюджета, фактически исполненные|||t8013002|||Жилищно-коммунальное хозяйство",
  "Расходы местного бюджета, фактически исполненные|||t8013002|||Социальная политика",
  #"Расходы местного бюджета, фактически исполненные|||t8013002|||Культура, кинематография",
  #"Расходы местного бюджета, фактически исполненные|||t8013002|||Дорожное хозяйство (дорожные фонды)",
  #"Расходы местного бюджета, фактически исполненные|||t8013002|||Физическая культура и спорт",
  #"Расходы местного бюджета, фактически исполненные|||t8013002|||Образование",
  #"Численность работников организаций культурно-досугового типа с учетом обособленных подразделений, всего",
  #"Среднесписочная численность работников организаций муниципальной формы собственности|||t8123015|||Раздел L Государственное управление и обеспечение военной безопасности; социальное страхование",
  #"Среднемесячная заработная плата работников организаций муниципальной формы собственности|||t8123017|||Раздел L Государственное управление и обеспечение военной безопасности; социальное страхование",
  #"Инвестиции в основной капитал за счет средств бюджета муниципального образования",
  "Инвестиции в основной капитал организаций муниципальной формы собственности",
  "Инвестиции в основной капитал, осуществляемые организациями, находящимися на территории муниципального образования (без субъектов малого предпринимательства)",
  #"Одиночное протяжение уличной водопроводной сети (lдо 2008г.- км)",
  #"Одиночное протяжение уличной водопроводной сети, нуждающейся в замене (lдо 2008г.- км)",
  #"Одиночное протяжение уличной водопроводной сети, которая заменена и отремонтирована за отчетный год",
  "Доходы местного бюджета, фактически исполненные|||t8013001|||Всего",
  "Доходы местного бюджета, фактически исполненные|||t8013001|||Из общей величины доходов - собственные доходы",
  #"Доходы местного бюджета, фактически исполненные|||t8013001|||Безвозмездные поступления",
  #"Доходы местного бюджета, фактически исполненные|||t8013001|||Безвозмездные поступления от других бюджетов бюджетной системы Российской Федерации",
  #"Доходы местного бюджета, фактически исполненные|||t8013001|||Доходы от использования имущества, находящегося в государственной и муниципальной собственности",
  "Доходы местного бюджета, фактически исполненные|||t8013001|||Налог на доходы физических лиц",
  "Доходы местного бюджета, фактически исполненные|||t8013001|||Субвенции бюджетам бюджетной системы Российской Федерации",
  "Доходы местного бюджета, фактически исполненные|||t8013001|||Дотации бюджетам бюджетной системы Российской Федерации",
  "Доходы местного бюджета, фактически исполненные|||t8013001|||Субсидии бюджетам бюджетной системы Российской Федерации (межбюджетные субсидии)"
  #"Число семей, состоящих на учете в качестве нуждающихся в жилых помещениях на конец года|||t8011011|||Всего",
  #"Число семей, получивших жилые помещения и улучшивших жилищные условия в отчетном году.|||t8011010|||Всего",
  #"Общая протяженность освещенных частей улиц, проездов, набережных на конец года",
  #"Удельный вес прибыльных организаций в общем числе организаций (по 2016 год)|||t8042018|||Муниципальная собственность_Всего",
  #"Удельный вес прибыльных организаций в общем числе организаций (по okved2)|||t8942018|||Муниципальная собственность_Всего по обследуемым видам экономической деятельности",
  #"Число работников муниципальных органов охраны общественного порядка",
  #"Среднесписочная численность работников организаций муниципальной формы собственности|||t8123015|||Всего",
  #"Среднемесячная заработная плата работников организаций муниципальной формы собственности|||t8123017|||Всего",
  #"Доходы местного бюджета, фактически исполненные|||t8013001|||Налоги на имущество",
  #"Доходы местного бюджета, фактически исполненные|||t8013001|||Налоги на совокупный доход"
  )






