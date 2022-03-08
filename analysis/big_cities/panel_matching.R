# Purpose: estimate staggered adoption models on big_cities data
# Inputs:  intermediate_data/BDMO_id_name.csv
#          final_data/big_cities.csv
# Outputs: -




library(dplyr)
library(plm)
library(PanelMatch)
library(readr)
library(tidyr)




big_cities <- read_csv("final_data/big_cities.csv") %>% select(-c(1))
BDMO_id_name <- read_csv("intermediate_data/BDMO_id_name.csv") %>% select(-c(1))

big_cities <- big_cities %>% 
  mutate(treatment_status = case_when(model == "Избираемый мэр" ~ 0,
                                      model == "Сити-менеджер" ~ 1,
                                      model == "Назначаемый мэр" ~ 2))
big_cities <- big_cities %>% filter(!is.na(treatment_status))

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


# TODO: move to EDA
big_cities %>% select(municipality, year, treatment_status, model, group) %>% 
  filter(group == "unexpected") %>% View() # находка для синтетического контроля

library(ggplot2)
ggplot(big_cities, aes(x = group)) +
  geom_bar()

structure <- read_csv("raw_data/Krupnie_goroda-RF_1985-2019_187_09.12.21/structure.csv")
big_cities %>% is.na() %>% colSums()
########## переменные, в которых не слишком много пропусков (< 400) ############
# latitude_dd, longitude_dd, assets (311), assets_depreciation (311), assets_exhausted (313), birth,
# build_flat, catering, catering_growth, construction, crimes (285), criminals (249),
# death, doctors, doctors_per10, hospital_beds, hospital_beds_per10, hospitals, 
# invest_budg, invest_fed, investment, job_seeker (207), job_seeker_unempl (199),
# living_space, n_companies, new_housing, new_polycl_visits (324), new_preschool_places (120),
# nurses, nurses_per10, pens, pension, polycl_visits, polycl_visits_per10, polyclinic,
# pop_1_6, pop_old, pop_work, pop_young, population, preschool (165), preschool_child,
# preschool_places, retail, retail_growth, rni, volume_electr, volume_manufact,
# wage, workers, t8016002 (232), t8008007, t8008008, t8008025 (345), t8006003,
# t8006007 (224), t8013002_1, t8013002_212, t8013002_220 (111), t8013002_221 (102),
# t8013002_229, t8013002_234, t8013001_1 (338), t8013001_89 (358), t8013001_34 (339),
# t8013001_36 (338), t8013001_27 (342), t8011011_0 (398), t8011010_0 (394),
# t8045002_21 (410), t8045002_27 (418), share_profitable_firms (598)

########## переменные, которые точно нужно брать как контрольные т.к. мало пропусков ############
# birth Число родившихся на 1000 человек населения
# build_flat Ввод в действие объектов социально-культурного назначения за счет всех источников финансирования: квартиры (включая квартиры в общежитиях)
# catering Оборот общественного питания (в фактически действовавших ценах), млн. руб.
# construction Объем работ, выполненных по виду деятельности “Строительство” (в фактически действовавших ценах), млн. руб.
# death Число умерших на 1000 человек населения
# doctors_per10 Численность врачей, человек: на 10 000 человек населения
# hospital_beds_per10 Число больничных коек круглосуточных стационаров: на 10 000 человек населения
# hospitals Число больничных учреждений
# living_space Общая площадь жилых помещений, приходящаяся в среднем на одного городского жителя (на конец года), м2
# n_companies Число предприятий и организаций (на конец года; по данным государственной регистрации)
# new_housing Ввод в действие объектов социально-культурного назначения за счет всех источников финансирования: жилые дома, тыс. м2 общей площади
# nurses_per10 Численность среднего медицинского персонала, человек: на 10 000 человек населения
# pens Численность пенсионеров, тыс. человек
# pension Средний размер назначенных пенсий, руб.
# polycl_visits_per10 Мощность амбулаторно-поликлинических организаций, посещений в смену: на 10 000 человек населения
# polyclinic Число амбулаторно-поликлинических организаций
# pop_1_6 Из общей численности – население в возрасте: из них детей в возрасте 1-6 лет
# pop_old Из общей численности – население в возрасте: старше трудоспособного
# pop_work Из общей численности – население в возрасте: трудоспособном
# pop_young Из общей численности – население в возрасте: моложе трудоспособного
# population Численность населения (оценка на конец года), тыс. человек
# preschool_child Число дошкольных учреждений в них: детей, тыс. человек
# preschool_places Число дошкольных учреждений в них: мест, тыс.
# retail Оборот розничной торговли (в фактически действовавших ценах), млн. руб.
# rni Естественный прирост, убыль (-) на 1000 человек населения
# wage Среднемесячная номинальная начисленная заработная плата, руб.
# workers Среднегодовая численность работников организаций, тыс. человек
## t8008007 Одиночное протяжение уличной водопроводной сети (lдо 2008г.- км)
## t8008008 Одиночное протяжение уличной водопроводной сети, нуждающейся в замене (lдо 2008г.- км)
# t8006003 Общая протяженность освещенных частей улиц, проездов, набережных на конец года
#

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



BDMO_id_name %>% filter(id == "t8008007") %>% View()



wage_gap %>% 
  select(SMSA_central, AFQT2, self_conf, education, years, woman, black, 
         hispanic, fam_size, married, union, promotion, risk, group, 
         size_of_firm, HGT_father, HGT_mother) %>% 
  filter(group %in% c(2, 3)) %>% 
  mutate(group = replace(group, group == 2, "Stayed")) %>% 
  mutate(group = replace(group, group == 3, "Moved")) %>% 
  st(group = "group", summ = c('mean(x)', 'notNA(x)'),
     summ.names = list(c('Mean','Observations')),
     digits = 2, labels = T, group.test = list(digits = 2),
     title = "Баланс ковариатов",
     col.align = c("left", rep("center", 5)))


# разница в характеристиках до тритмента (всё равно значимые различия)
wage_gap %>% 
  select(SMSA_central, AFQT2, self_conf, education, years, woman, black, 
         hispanic, fam_size, married, union, promotion, risk, group, 
         size_of_firm, HGT_father, HGT_mother) %>% 
  filter(group %in% c(2, 3)) %>% 
  filter(SMSA_central == 0) %>% 
  mutate(group = replace(group, group == 2, "Stayed")) %>% 
  mutate(group = replace(group, group == 3, "Moved")) %>% 
  st(group = "group", summ = c('mean(x)', 'notNA(x)'),
     summ.names = list(c('Mean','Observations')),
     digits = 2, labels = T, group.test = list(digits = 2),
     title = "Баланс ковариатов",
     col.align = c("left", rep("center", 5)))


#########################################################################
########################        Задание 3        ########################
#########################################################################


task3 <- wage_gap %>% 
  select(n, t, cpi_w, SMSA_central, AFQT2, self_conf, education, years, woman, black, 
         hispanic, fam_size, married, union, promotion, risk, group) %>% 
  filter(group %in% c(2, 3)) %>% as.data.frame()

task3$t <- task3$t %>% as.integer()
task3$n <- task3$n %>% as.integer()

DisplayTreatment(unit.id = "n",
                 time.id = "t", legend.position = "none",
                 xlab = "year", ylab = "Individual number",
                 treatment = "SMSA_central", data = task3 %>% filter(group == 3), 
                 hide.y.axis.label = F)


# PSW
PM.results <- PanelMatch(lag = 4, time.id = "t", unit.id = "n", 
                         treatment = "SMSA_central", refinement.method = "ps.weight", 
                         data = task3, match.missing = TRUE, size.match = 5,
                         covs.formula = ~ AFQT2 + self_conf + education + years + woman + black + hispanic + 
                           fam_size + married + promotion + risk + union,
                         qoi = "att" , outcome.var = "cpi_w", lead = 1:4,
                         forbid.treatment.reversal = TRUE)

PE.results <- PanelEstimate(sets = PM.results, task3, se.method = "conditional", confidence.level = 0.99)
PE.results %>% summary()
plot(PE.results)


# PSM
PM.results <- PanelMatch(lag = 4, time.id = "t", unit.id = "n", 
                         treatment = "SMSA_central", refinement.method = "ps.match", 
                         data = task3, match.missing = TRUE, size.match = 5,
                         covs.formula = ~ AFQT2 + self_conf + education + years + woman + black + hispanic + 
                           fam_size + married + promotion + risk + union,
                         qoi = "att" , outcome.var = "cpi_w", lead = 1:4,
                         forbid.treatment.reversal = TRUE)

PE.results <- PanelEstimate(sets = PM.results, task3, se.method = "conditional", confidence.level = 0.99)
PE.results %>% summary()
plot(PE.results)


#########################################################################
########################        Задание 4        ########################
#########################################################################


# баланс ковариатов
get_covariate_balance(PM.results$att, task3, 
                      covariates = c("AFQT2", "self_conf", "education", "years", "woman", "black", 
                                     "hispanic", "fam_size", "married", "union", "promotion", "risk"), 
                      plot = T, ylim = c(-2,2),
                      use.equal.weights = T) # было

get_covariate_balance(PM.results$att, task3, 
                      covariates = c("AFQT2", "self_conf", "education", "years", "woman", "black", 
                                     "hispanic", "fam_size", "married", "union", "promotion", "risk"), 
                      plot = T, ylim = c(-2,2)) # стало

get_covariate_balance(PM.results$att, task3, 
                      covariates = c("AFQT2", "self_conf", "education", "years", "woman", "black", 
                                     "hispanic", "fam_size", "married", "union", "promotion", "risk"))


#########################################################################
########################        Задание 5        ########################
#########################################################################


task5 <- wage_gap %>% 
  select(n, t, cpi_w, SMSA_central, AFQT2, self_conf, education, years, woman, black, 
         hispanic, fam_size, married, union, promotion, risk, group) %>% 
  filter(group %in% c(1, 3)) %>% as.data.frame()

# техническая манипуляция (нужно, чтобы пакет рассматривал тех, у кого всегда SMSA_central=1 как контроль)
task5[task5$group == 1,]$SMSA_central <- 0

task5$t <- task5$t %>% as.integer()
task5$n <- task5$n %>% as.integer()


DisplayTreatment(unit.id = "n",
                 time.id = "t", legend.position = "none",
                 xlab = "year", ylab = "Individual number",
                 treatment = "SMSA_central", data = task5 %>% filter(group == 3), 
                 hide.y.axis.label = F)


PM.results <- PanelMatch(lag = 4, time.id = "t", unit.id = "n", 
                         treatment = "SMSA_central", refinement.method = "ps.match", 
                         data = task5, match.missing = TRUE, size.match = 5,
                         covs.formula = ~ AFQT2 + self_conf + education + years + woman + black + hispanic + 
                           fam_size + married + promotion + risk + union,
                         qoi = "att" , outcome.var = "cpi_w", lead = 0:4,
                         forbid.treatment.reversal = TRUE)


# баланс ковариатов
get_covariate_balance(PM.results$att, task5, 
                      covariates = c("AFQT2", "self_conf", "education", "years", "woman", "black", 
                                     "hispanic", "fam_size", "married", "union", "promotion", "risk"), 
                      plot = T, ylim = c(-2,2),
                      use.equal.weights = T) # было

get_covariate_balance(PM.results$att, task5, 
                      covariates = c("AFQT2", "self_conf", "education", "years", "woman", "black", 
                                     "hispanic", "fam_size", "married", "union", "promotion", "risk"), 
                      plot = T, ylim = c(-2,2)) # стало


PE.results <- PanelEstimate(sets = PM.results, task5, se.method = "conditional", confidence.level = 0.99)
PE.results %>% summary()
plot(PE.results) 


#########################################################################
########################        Задание 6        ########################
#########################################################################


task6 <- wage_gap %>% 
  select(n, t, cpi_w, SMSA_central, AFQT2, self_conf, education, years, woman, black, 
         hispanic, fam_size, married, union, promotion, risk, group) %>% 
  filter(group %in% c(2, 3)) %>% as.data.frame()

task6$t <- task6$t %>% as.integer()
task6$n <- task6$n %>% as.integer()


PM.results <- PanelMatch(lag = 4, time.id = "t", unit.id = "n", 
                         treatment = "SMSA_central", refinement.method = "mahalanobis", 
                         data = task6, match.missing = TRUE, size.match = 5,
                         covs.formula = ~ AFQT2 + self_conf + education + years + woman + black + hispanic + 
                           fam_size + married + promotion + risk + union,
                         qoi = "att" , outcome.var = "cpi_w", lead = 0:4,
                         forbid.treatment.reversal = TRUE)


# баланс ковариатов
get_covariate_balance(PM.results$att, task6, 
                      covariates = c("AFQT2", "self_conf", "education", "years", "woman", "black", 
                                     "hispanic", "fam_size", "married", "union", "promotion", "risk"), 
                      plot = T, ylim = c(-2,2),
                      use.equal.weights = T) # было

get_covariate_balance(PM.results$att, task6, 
                      covariates = c("AFQT2", "self_conf", "education", "years", "woman", "black", 
                                     "hispanic", "fam_size", "married", "union", "promotion", "risk"), 
                      plot = T, ylim = c(-2,2)) # стало


PE.results <- PanelEstimate(sets = PM.results, task6, se.method = "conditional", confidence.level = 0.99)
PE.results %>% summary()
plot(PE.results)


# вот так выглядят индивиды, которые метчатся по времени (из тех, кто не never treated)
DisplayTreatment(unit.id = "n",
                 time.id = "t", legend.position = "none",
                 xlab = "year", ylab = "Individual number", matched.set = matched_sets[4],
                 treatment = "SMSA_central", data = task6 %>% filter(group == 3), 
                 hide.y.axis.label = F, show.set.only = F)


matched_sets <- PM.results$att
attr(matched_sets[[1]], "weights")
# веса одинаковы (округление числа 1/782 до 0.0);
# на самом деле, Y для diff-in-diff по matched группам
# будет считаться по 5 ближайшим соседям с весами 0.2 (т.к. size.match = 5);
# чтобы веса были разными, нужно делать weighting
# например, refinement.method = "ps.weight"


#########################################################################
########################       Ещё что-то        ########################
#########################################################################


# вот так выглядят индивиды, которые метчатся по времени (из тех, кто не never treated)
DisplayTreatment(unit.id = "n",
                 time.id = "t", legend.position = "none",
                 xlab = "year", ylab = "Individual number", matched.set = matched_sets[4],
                 treatment = "SMSA_central", data = task6 %>% filter(group == 3), 
                 hide.y.axis.label = F, show.set.only = F)





