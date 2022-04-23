# Purpose: estimate staggered adoption models on big_cities data
# Inputs:  intermediate_data/BDMO_id_name.csv
#          final_data/big_cities.csv
# Outputs: -




library(did)
library(dplyr)
library(plm)
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

# все расходы
y_var <- "t8013002_1_c_pc" 
cov_vars <- c()

data <- big_cities %>% 
  filter(group != "unexpected") %>% 
  select(c("settlement", "region", "year", "treat", "first.treat", "oktmo",
           "time_to_treat", "treatment", "competitive", y_var, all_of(cov_vars))) %>% 
  mutate(first.treat.nc = first.treat*(1-competitive)) %>% 
  drop_na() %>% as.data.frame()

out <- att_gt(yname = y_var,
              gname = "first.treat",
              idname = "oktmo",
              tname = "year",
              xformla = ~1,
              data = data,
              est_method = "dr",
              clustervars = "region",
              #bstrap = F,
              #cband = F
)
es <- aggte(out, type = "dynamic", na.rm = T)
ggdid(es)
group_effects <- aggte(out, type = "group", na.rm = T)

out %>% summary()
ggdid(out)

es %>% summary()
ggdid(es)

group_effects %>% summary()
ggdid(group_effects)


################################################################################

y_id <- "t8013002_1_c"


x_fm <- ~ 1 + t8042018_kfs199_okved0 + t8045002_21_c + t8045002_26_c + t8045002_27_c +
  t8006005_9 + mun_type
x_fm <- ~ 1 + t8042018_kfs199_okved0 + t8045002_21_c + t8045002_26_c + t8045002_27_c
x_fm <- ~ 1 + t8042018_kfs199_okved0 + t8112013 + t8045002_21_c + mun_type
x_fm <- ~ 1 + t8042018_kfs199_okved0 + t8112013 + t8045002_21_c #
x_fm <- ~ 1 + t8042018_kfs199_okved0 + t8045002_21_c
x_fm <- ~ 1 + t8042018_kfs199_okved0 + t8112013
x_fm <- ~ 1 + t8042018_kfs199_okved0 + mun_type
x_fm <- ~ 1 + t8042018_kfs199_okved0
x_fm <- ~ 1

x_fm <- ~ 1 + wage + workers

out <- att_gt(yname = y_id,
              gname = "first.treat",
              idname = "oktmo",
              tname = "year",
              xformla = x_fm,
              data = big_cities,
              est_method = "dr",
              clustervars = "region_oktmo",
              #bstrap = F,
              #cband = F
)
es <- aggte(out, type = "dynamic", na.rm = T)
ggdid(es)
group_effects <- aggte(out, type = "group", na.rm = T)

out %>% summary()
ggdid(out)

es %>% summary()
ggdid(es)

group_effects %>% summary()
ggdid(group_effects)


################################################################################
#  Общая протяженность освещенных частей улиц, проездов, набережных на конец   #
#  года / Общая протяженность улиц, проездов, набережных на конец года         #
#                                                                              #
################################################################################


"Общая протяженность освещенных частей улиц, проездов, набережных на конец года" %>% get_code()
"Общая протяженность улиц, проездов, набережных на конец года" %>% get_code()

big_cities["t8006003/t8006007"] <- big_cities$t8006003 / big_cities$t8006007

y_id <- "t8006003"
x_fm <- ~ 1 + t8042018_kfs199_okved0


out <- att_gt(yname = y_id,
              gname = "first.treat",
              idname = "oktmo",
              tname = "year",
              xformla = x_fm,
              data = big_cities,
              est_method = "dr",
              clustervars = "region_oktmo"
)

es <- aggte(out, type = "dynamic")
group_effects <- aggte(out, type = "group")

out %>% summary()
ggdid(out)

es %>% summary()
ggdid(es)

group_effects %>% summary()
ggdid(group_effects)





"Одиночное протяжение уличной водопроводной сети, которая заменена и отремонтирована за отчетный год" %>% get_code() 

y_id <- "t8008025"
x_fm <- ~ 1 + t8042018_kfs199_okved0


out <- att_gt(yname = y_id,
              gname = "first.treat",
              idname = "oktmo",
              tname = "year",
              xformla = x_fm,
              data = big_cities,
              est_method = "dr",
              clustervars = "region_oktmo"
)

es <- aggte(out, type = "dynamic")
group_effects <- aggte(out, type = "group")

out %>% summary()
ggdid(out)

es %>% summary()
ggdid(es)

group_effects %>% summary()
ggdid(group_effects)




"Среднесписочная численность работников организаций муниципальной формы собственности|||t8123015|||Раздел L Государственное управление и обеспечение военной безопасности; социальное страхование" %>% get_code() 

y_id <- "t8123015_12"
x_fm <- ~ 1 + t8042018_kfs199_okved0


out <- att_gt(yname = y_id,
              gname = "first.treat",
              idname = "oktmo",
              tname = "year",
              xformla = x_fm,
              data = big_cities,
              est_method = "dr",
              clustervars = "region_oktmo"
)

es <- aggte(out, type = "dynamic")
group_effects <- aggte(out, type = "group")

out %>% summary()
ggdid(out)

es %>% summary()
ggdid(es)

group_effects %>% summary()
ggdid(group_effects)




"Число семей, получивших жилые помещения и улучшивших жилищные условия в отчетном году.|||t8011010|||Всего" %>% get_code()

y_id <- "t8011010_0"
x_fm <- ~ 1 + t8042018_kfs199_okved0


out <- att_gt(yname = y_id,
              gname = "first.treat",
              idname = "oktmo",
              tname = "year",
              xformla = x_fm,
              data = big_cities,
              est_method = "dr",
              clustervars = "region_oktmo"
)

es <- aggte(out, type = "dynamic")
group_effects <- aggte(out, type = "group")

out %>% summary()
ggdid(out)

es %>% summary()
ggdid(es)

group_effects %>% summary()
ggdid(group_effects)