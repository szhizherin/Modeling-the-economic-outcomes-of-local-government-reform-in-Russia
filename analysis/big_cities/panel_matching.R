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


get_group <- function(treatment_history) {
  # 0 ~ "Избираемый мэр"
  # 1 ~ "Сити-менеджер"
  # 2 ~ "Назаначаемый мэр"
  # the expected behavior would be either 0 -> 2 or 0 -> 1 -> 2 or 0 -> 1 or
  # 1 -> 2 or constant treatment status
  
  if (!(treatment_history == cummax(treatment_history))) {
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
  else if ((treatment_history == rep(0, length(treatment_history)))) {
    return("0")
  }
  else if ((treatment_history == rep(1, length(treatment_history)))) {
    return("1")
  }
  else if ((treatment_history == rep(2, length(treatment_history)))) {
    return("2")
  }
}




wage_gap <- wage_gap %>% 
  group_by(n) %>% mutate(group = get_group(SMSA_central)) %>% ungroup()


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





