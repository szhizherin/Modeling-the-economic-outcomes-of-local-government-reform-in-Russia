# Purpose: estimate synthetic control models on big_cities data
# Inputs:  intermediate_data/BDMO_id_name.csv
#          final_data/big_cities.csv
# Outputs: -


library(dplyr)
library(plm)
library(readr)
library(synthdid)
library(tidyr)





big_cities <- read_csv("final_data/big_cities.csv") %>% select(-c(1))
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
big_cities["t8008008_t8008007"] <- big_cities$t8008008 / big_cities$t8008007


################################################################################

y_var <- "t8013002_1_c" # не значимо, очень хорошая подгонка
city <- "Пятигорск"

data <- big_cities %>% 
  select(c("settlement", "year", all_of(y_var), "treatment", "treatment_status")) %>%
  filter(year %in% 2006:2014)

data <- data %>% 
  group_by(settlement) %>% mutate(group = get_group(treatment_status)) %>% ungroup()

always_treated <- (data %>% filter(group %in% c("1")))$settlement %>% unique() # , "2", "1 -> 2"
never_treated <- (data %>% filter(group %in% c("0")))$settlement %>% unique()
data$treatment_status <- NULL
data$group <- NULL

data <- data %>% 
  filter(!is.na(data[, paste(y_var)])) %>% 
  filter(settlement %in% c(city, all_of(always_treated))) %>% 
  make.pbalanced(balance.type = "shared.individuals")

data <- data %>% 
  mutate(treatment = case_when(treatment == 1 ~ 0,
                               treatment == 0 ~ 1))

data <- data %>% 
  arrange(year, settlement) %>% as.data.frame()


setup = panel.matrices(data)
tau.hat = synthdid_estimate(setup$Y, setup$N0, setup$T0)
se = sqrt(vcov(tau.hat, method='placebo'))
sprintf('point estimate: %1.2f', tau.hat)
sprintf('95%% CI (%1.2f, %1.2f)', tau.hat - 1.96 * se, tau.hat + 1.96 * se)

plot(tau.hat, se.method='placebo')
synthdid_units_plot(tau.hat, se.method='placebo')
plot(tau.hat, overlay=1,  se.method='placebo')
plot(tau.hat, overlay=.8, se.method='placebo')

tau.sc   = sc_estimate(setup$Y, setup$N0, setup$T0)
tau.did  = did_estimate(setup$Y, setup$N0, setup$T0)
estimates = list(tau.did, tau.sc, tau.hat)
names(estimates) = c('Diff-in-Diff', 'Synthetic Control', 'Synthetic Diff-in-Diff')
print(unlist(estimates))
synthdid_plot(estimates, se.method='placebo')
synthdid_units_plot(estimates, se.method='placebo')


################################################################################

y_var <- "t8013002_212_c" # не значимо, очень хорошая подгонка
city <- "Пятигорск"

data <- big_cities %>% 
  select(c("settlement", "year", all_of(y_var), "treatment", "treatment_status")) %>%
  filter(year %in% 2006:2014)

data <- data %>% 
  group_by(settlement) %>% mutate(group = get_group(treatment_status)) %>% ungroup()

always_treated <- (data %>% filter(group %in% c("1", "2", "1 -> 2")))$settlement %>% unique()
never_treated <- (data %>% filter(group %in% c("0")))$settlement %>% unique()
data$treatment_status <- NULL
data$group <- NULL

data <- data %>% 
  filter(!is.na(data[, paste(y_var)])) %>% 
  filter(settlement %in% c(city, all_of(always_treated))) %>% 
  make.pbalanced(balance.type = "shared.individuals")

data <- data %>% 
  mutate(treatment = case_when(treatment == 1 ~ 0,
                               treatment == 0 ~ 1))

data <- data %>% 
  arrange(year, settlement) %>% as.data.frame()


setup = panel.matrices(data)
tau.hat = synthdid_estimate(setup$Y, setup$N0, setup$T0)
se = sqrt(vcov(tau.hat, method='placebo'))
sprintf('point estimate: %1.2f', tau.hat)
sprintf('95%% CI (%1.2f, %1.2f)', tau.hat - 1.96 * se, tau.hat + 1.96 * se)

plot(tau.hat, se.method='placebo')
synthdid_units_plot(tau.hat, se.method='placebo')
plot(tau.hat, overlay=1,  se.method='placebo')
plot(tau.hat, overlay=.8, se.method='placebo')

tau.sc   = sc_estimate(setup$Y, setup$N0, setup$T0)
tau.did  = did_estimate(setup$Y, setup$N0, setup$T0)
estimates = list(tau.did, tau.sc, tau.hat)
names(estimates) = c('Diff-in-Diff', 'Synthetic Control', 'Synthetic Diff-in-Diff')
print(unlist(estimates))
synthdid_plot(estimates, se.method='placebo')
synthdid_units_plot(estimates, se.method='placebo')


################################################################################

y_var <- "t8013002_220_c" 
city <- "Пятигорск"

data <- big_cities %>% 
  select(c("settlement", "year", all_of(y_var), "treatment", "treatment_status")) %>%
  filter(year %in% 2006:2014)

data <- data %>% 
  group_by(settlement) %>% mutate(group = get_group(treatment_status)) %>% ungroup()

always_treated <- (data %>% filter(group %in% c("1", "2", "1 -> 2")))$settlement %>% unique()
never_treated <- (data %>% filter(group %in% c("0")))$settlement %>% unique()
data$treatment_status <- NULL
data$group <- NULL

data <- data %>% 
  filter(!is.na(data[, paste(y_var)])) %>% 
  filter(settlement %in% c(city, all_of(always_treated))) %>% 
  make.pbalanced(balance.type = "shared.individuals")

data <- data %>% 
  mutate(treatment = case_when(treatment == 1 ~ 0,
                               treatment == 0 ~ 1))

data <- data %>% 
  arrange(year, settlement) %>% as.data.frame()


setup = panel.matrices(data)
tau.hat = synthdid_estimate(setup$Y, setup$N0, setup$T0)
se = sqrt(vcov(tau.hat, method='placebo'))
sprintf('point estimate: %1.2f', tau.hat)
sprintf('95%% CI (%1.2f, %1.2f)', tau.hat - 1.96 * se, tau.hat + 1.96 * se)

plot(tau.hat, se.method='placebo')
synthdid_units_plot(tau.hat, se.method='placebo')
plot(tau.hat, overlay=1,  se.method='placebo')
plot(tau.hat, overlay=.8, se.method='placebo')

tau.sc   = sc_estimate(setup$Y, setup$N0, setup$T0)
tau.did  = did_estimate(setup$Y, setup$N0, setup$T0)
estimates = list(tau.did, tau.sc, tau.hat)
names(estimates) = c('Diff-in-Diff', 'Synthetic Control', 'Synthetic Diff-in-Diff')
print(unlist(estimates))
synthdid_plot(estimates, se.method='placebo')
synthdid_units_plot(estimates, se.method='placebo')


################################################################################

y_var <- "t8013002_221_c" 
city <- "Пятигорск"

data <- big_cities %>% 
  select(c("settlement", "year", all_of(y_var), "treatment", "treatment_status")) %>%
  filter(year %in% 2006:2014)

data <- data %>% 
  group_by(settlement) %>% mutate(group = get_group(treatment_status)) %>% ungroup()

always_treated <- (data %>% filter(group %in% c("1", "2", "1 -> 2")))$settlement %>% unique()
never_treated <- (data %>% filter(group %in% c("0")))$settlement %>% unique()
data$treatment_status <- NULL
data$group <- NULL

data <- data %>% 
  filter(!is.na(data[, paste(y_var)])) %>% 
  filter(settlement %in% c(city, all_of(always_treated))) %>% 
  make.pbalanced(balance.type = "shared.individuals")

data <- data %>% 
  mutate(treatment = case_when(treatment == 1 ~ 0,
                               treatment == 0 ~ 1))

data <- data %>% 
  arrange(year, settlement) %>% as.data.frame()


setup = panel.matrices(data)
tau.hat = synthdid_estimate(setup$Y, setup$N0, setup$T0)
se = sqrt(vcov(tau.hat, method='placebo'))
sprintf('point estimate: %1.2f', tau.hat)
sprintf('95%% CI (%1.2f, %1.2f)', tau.hat - 1.96 * se, tau.hat + 1.96 * se)

plot(tau.hat, se.method='placebo')
synthdid_units_plot(tau.hat, se.method='placebo')
plot(tau.hat, overlay=1,  se.method='placebo')
plot(tau.hat, overlay=.8, se.method='placebo')

tau.sc   = sc_estimate(setup$Y, setup$N0, setup$T0)
tau.did  = did_estimate(setup$Y, setup$N0, setup$T0)
estimates = list(tau.did, tau.sc, tau.hat)
names(estimates) = c('Diff-in-Diff', 'Synthetic Control', 'Synthetic Diff-in-Diff')
print(unlist(estimates))
synthdid_plot(estimates, se.method='placebo')
synthdid_units_plot(estimates, se.method='placebo')


################################################################################

y_var <- "t8013002_229_c" # не значимо, схожесть необычных траекторий после
city <- "Пятигорск"

data <- big_cities %>% 
  select(c("settlement", "year", all_of(y_var), "treatment", "treatment_status")) %>%
  filter(year %in% 2006:2014)

data <- data %>% 
  group_by(settlement) %>% mutate(group = get_group(treatment_status)) %>% ungroup()

always_treated <- (data %>% filter(group %in% c("1")))$settlement %>% unique()
never_treated <- (data %>% filter(group %in% c("0")))$settlement %>% unique()
data$treatment_status <- NULL
data$group <- NULL

data <- data %>% 
  filter(!is.na(data[, paste(y_var)])) %>% 
  filter(settlement %in% c(city, all_of(always_treated))) %>% 
  make.pbalanced(balance.type = "shared.individuals")

data <- data %>% 
  mutate(treatment = case_when(treatment == 1 ~ 0,
                               treatment == 0 ~ 1))

data <- data %>% 
  arrange(year, settlement) %>% as.data.frame()


setup = panel.matrices(data)
tau.hat = synthdid_estimate(setup$Y, setup$N0, setup$T0)
se = sqrt(vcov(tau.hat, method='placebo'))
sprintf('point estimate: %1.2f', tau.hat)
sprintf('95%% CI (%1.2f, %1.2f)', tau.hat - 1.96 * se, tau.hat + 1.96 * se)

plot(tau.hat, se.method='placebo')
synthdid_units_plot(tau.hat, se.method='placebo')
plot(tau.hat, overlay=1,  se.method='placebo')
plot(tau.hat, overlay=.8, se.method='placebo')

tau.sc   = sc_estimate(setup$Y, setup$N0, setup$T0)
tau.did  = did_estimate(setup$Y, setup$N0, setup$T0)
estimates = list(tau.did, tau.sc, tau.hat)
names(estimates) = c('Diff-in-Diff', 'Synthetic Control', 'Synthetic Diff-in-Diff')
print(unlist(estimates))
synthdid_plot(estimates, se.method='placebo')
synthdid_units_plot(estimates, se.method='placebo')


################################################################################

y_var <- "t8013002_234_c" 
city <- "Пятигорск"

data <- big_cities %>% 
  select(c("settlement", "year", all_of(y_var), "treatment", "treatment_status")) %>%
  filter(year %in% 2006:2014)

data <- data %>% 
  group_by(settlement) %>% mutate(group = get_group(treatment_status)) %>% ungroup()

always_treated <- (data %>% filter(group %in% c("1")))$settlement %>% unique()
never_treated <- (data %>% filter(group %in% c("0")))$settlement %>% unique()
data$treatment_status <- NULL
data$group <- NULL

data <- data %>% 
  filter(!is.na(data[, paste(y_var)])) %>% 
  filter(settlement %in% c(city, all_of(always_treated))) %>% 
  make.pbalanced(balance.type = "shared.individuals")

data <- data %>% 
  mutate(treatment = case_when(treatment == 1 ~ 0,
                               treatment == 0 ~ 1))

data <- data %>% 
  arrange(year, settlement) %>% as.data.frame()


setup = panel.matrices(data)
tau.hat = synthdid_estimate(setup$Y, setup$N0, setup$T0)
se = sqrt(vcov(tau.hat, method='placebo'))
sprintf('point estimate: %1.2f', tau.hat)
sprintf('95%% CI (%1.2f, %1.2f)', tau.hat - 1.96 * se, tau.hat + 1.96 * se)

plot(tau.hat, se.method='placebo')
synthdid_units_plot(tau.hat, se.method='placebo')
plot(tau.hat, overlay=1,  se.method='placebo')
plot(tau.hat, overlay=.8, se.method='placebo')

tau.sc   = sc_estimate(setup$Y, setup$N0, setup$T0)
tau.did  = did_estimate(setup$Y, setup$N0, setup$T0)
estimates = list(tau.did, tau.sc, tau.hat)
names(estimates) = c('Diff-in-Diff', 'Synthetic Control', 'Synthetic Diff-in-Diff')
print(unlist(estimates))
synthdid_plot(estimates, se.method='placebo')
synthdid_units_plot(estimates, se.method='placebo')


################################################################################

y_var <- "invest_budg" 
city <- "Пятигорск"

data <- big_cities %>% 
  select(c("settlement", "year", all_of(y_var), "treatment", "treatment_status")) %>%
  filter(year %in% 2006:2014)

data <- data %>% 
  group_by(settlement) %>% mutate(group = get_group(treatment_status)) %>% ungroup()

always_treated <- (data %>% filter(group %in% c("1")))$settlement %>% unique()
never_treated <- (data %>% filter(group %in% c("0")))$settlement %>% unique()
data$treatment_status <- NULL
data$group <- NULL

data <- data %>% 
  filter(!is.na(data[, paste(y_var)])) %>% 
  filter(settlement %in% c(city, all_of(always_treated))) %>% 
  make.pbalanced(balance.type = "shared.individuals")

data <- data %>% 
  mutate(treatment = case_when(treatment == 1 ~ 0,
                               treatment == 0 ~ 1))

data <- data %>% 
  arrange(year, settlement) %>% as.data.frame()


setup = panel.matrices(data)
tau.hat = synthdid_estimate(setup$Y, setup$N0, setup$T0)
se = sqrt(vcov(tau.hat, method='placebo'))
sprintf('point estimate: %1.2f', tau.hat)
sprintf('95%% CI (%1.2f, %1.2f)', tau.hat - 1.96 * se, tau.hat + 1.96 * se)

plot(tau.hat, se.method='placebo')
synthdid_units_plot(tau.hat, se.method='placebo')
plot(tau.hat, overlay=1,  se.method='placebo')
plot(tau.hat, overlay=.8, se.method='placebo')

tau.sc   = sc_estimate(setup$Y, setup$N0, setup$T0)
tau.did  = did_estimate(setup$Y, setup$N0, setup$T0)
estimates = list(tau.did, tau.sc, tau.hat)
names(estimates) = c('Diff-in-Diff', 'Synthetic Control', 'Synthetic Diff-in-Diff')
print(unlist(estimates))
synthdid_plot(estimates, se.method='placebo')
synthdid_units_plot(estimates, se.method='placebo')


################################################################################

y_var <- "invest_fed" # хорошая подгонка, интересно, что эффект незначим
city <- "Пятигорск"

data <- big_cities %>% 
  select(c("settlement", "year", all_of(y_var), "treatment", "treatment_status")) %>%
  filter(year %in% 2006:2014)

data <- data %>% 
  group_by(settlement) %>% mutate(group = get_group(treatment_status)) %>% ungroup()

always_treated <- (data %>% filter(group %in% c("1")))$settlement %>% unique()
never_treated <- (data %>% filter(group %in% c("0")))$settlement %>% unique()
data$treatment_status <- NULL
data$group <- NULL

data <- data %>% 
  filter(!is.na(data[, paste(y_var)])) %>% 
  filter(settlement %in% c(city, all_of(always_treated))) %>% 
  make.pbalanced(balance.type = "shared.individuals")

data <- data %>% 
  mutate(treatment = case_when(treatment == 1 ~ 0,
                               treatment == 0 ~ 1))

data <- data %>% 
  arrange(year, settlement) %>% as.data.frame()


setup = panel.matrices(data)
tau.hat = synthdid_estimate(setup$Y, setup$N0, setup$T0)
se = sqrt(vcov(tau.hat, method='placebo'))
sprintf('point estimate: %1.2f', tau.hat)
sprintf('95%% CI (%1.2f, %1.2f)', tau.hat - 1.96 * se, tau.hat + 1.96 * se)

plot(tau.hat, se.method='placebo')
synthdid_units_plot(tau.hat, se.method='placebo')
plot(tau.hat, overlay=1,  se.method='placebo')
plot(tau.hat, overlay=.8, se.method='placebo')

tau.sc   = sc_estimate(setup$Y, setup$N0, setup$T0)
tau.did  = did_estimate(setup$Y, setup$N0, setup$T0)
estimates = list(tau.did, tau.sc, tau.hat)
names(estimates) = c('Diff-in-Diff', 'Synthetic Control', 'Synthetic Diff-in-Diff')
print(unlist(estimates))
synthdid_plot(estimates, se.method='placebo')
synthdid_units_plot(estimates, se.method='placebo')


################################################################################

y_var <- "investment_c" # не значимо, неплохая подгонка
city <- "Пятигорск"

data <- big_cities %>% 
  select(c("settlement", "year", all_of(y_var), "treatment", "treatment_status")) %>%
  filter(year %in% 2006:2014)

data <- data %>% 
  group_by(settlement) %>% mutate(group = get_group(treatment_status)) %>% ungroup()

always_treated <- (data %>% filter(group %in% c("1")))$settlement %>% unique()
never_treated <- (data %>% filter(group %in% c("0")))$settlement %>% unique()
data$treatment_status <- NULL
data$group <- NULL

data <- data %>% 
  filter(!is.na(data[, paste(y_var)])) %>% 
  filter(settlement %in% c(city, all_of(always_treated))) %>% 
  make.pbalanced(balance.type = "shared.individuals")

data <- data %>% 
  mutate(treatment = case_when(treatment == 1 ~ 0,
                               treatment == 0 ~ 1))

data <- data %>% 
  arrange(year, settlement) %>% as.data.frame()


setup = panel.matrices(data)
tau.hat = synthdid_estimate(setup$Y, setup$N0, setup$T0)
se = sqrt(vcov(tau.hat, method='placebo'))
sprintf('point estimate: %1.2f', tau.hat)
sprintf('95%% CI (%1.2f, %1.2f)', tau.hat - 1.96 * se, tau.hat + 1.96 * se)

plot(tau.hat, se.method='placebo')
synthdid_units_plot(tau.hat, se.method='placebo')
plot(tau.hat, overlay=1,  se.method='placebo')
plot(tau.hat, overlay=.8, se.method='placebo')

tau.sc   = sc_estimate(setup$Y, setup$N0, setup$T0)
tau.did  = did_estimate(setup$Y, setup$N0, setup$T0)
estimates = list(tau.did, tau.sc, tau.hat)
names(estimates) = c('Diff-in-Diff', 'Synthetic Control', 'Synthetic Diff-in-Diff')
print(unlist(estimates))
synthdid_plot(estimates, se.method='placebo')
synthdid_units_plot(estimates, se.method='placebo')


################################################################################

y_var <- "volume_electr_c"
city <- "Пятигорск"

data <- big_cities %>% 
  select(c("settlement", "year", all_of(y_var), "treatment", "treatment_status")) %>%
  filter(year %in% 2006:2014)

data <- data %>% 
  group_by(settlement) %>% mutate(group = get_group(treatment_status)) %>% ungroup()

always_treated <- (data %>% filter(group %in% c("1")))$settlement %>% unique()
never_treated <- (data %>% filter(group %in% c("0")))$settlement %>% unique()
data$treatment_status <- NULL
data$group <- NULL

data <- data %>% 
  filter(!is.na(data[, paste(y_var)])) %>% 
  filter(settlement %in% c(city, all_of(always_treated))) %>% 
  make.pbalanced(balance.type = "shared.individuals")

data <- data %>% 
  mutate(treatment = case_when(treatment == 1 ~ 0,
                               treatment == 0 ~ 1))

data <- data %>% 
  arrange(year, settlement) %>% as.data.frame()


setup = panel.matrices(data)
tau.hat = synthdid_estimate(setup$Y, setup$N0, setup$T0)
se = sqrt(vcov(tau.hat, method='placebo'))
sprintf('point estimate: %1.2f', tau.hat)
sprintf('95%% CI (%1.2f, %1.2f)', tau.hat - 1.96 * se, tau.hat + 1.96 * se)

plot(tau.hat, se.method='placebo')
synthdid_units_plot(tau.hat, se.method='placebo')
plot(tau.hat, overlay=1,  se.method='placebo')
plot(tau.hat, overlay=.8, se.method='placebo')

tau.sc   = sc_estimate(setup$Y, setup$N0, setup$T0)
tau.did  = did_estimate(setup$Y, setup$N0, setup$T0)
estimates = list(tau.did, tau.sc, tau.hat)
names(estimates) = c('Diff-in-Diff', 'Synthetic Control', 'Synthetic Diff-in-Diff')
print(unlist(estimates))
synthdid_plot(estimates, se.method='placebo')
synthdid_units_plot(estimates, se.method='placebo')


################################################################################

y_var <- "volume_manufact_c" 
city <- "Пятигорск"

data <- big_cities %>% 
  select(c("settlement", "year", all_of(y_var), "treatment", "treatment_status")) %>%
  filter(year %in% 2006:2014)

data <- data %>% 
  group_by(settlement) %>% mutate(group = get_group(treatment_status)) %>% ungroup()

always_treated <- (data %>% filter(group %in% c("1")))$settlement %>% unique()
never_treated <- (data %>% filter(group %in% c("0")))$settlement %>% unique()
data$treatment_status <- NULL
data$group <- NULL

data <- data %>% 
  filter(!is.na(data[, paste(y_var)])) %>% 
  filter(settlement %in% c(city, all_of(always_treated))) %>% 
  make.pbalanced(balance.type = "shared.individuals")

data <- data %>% 
  mutate(treatment = case_when(treatment == 1 ~ 0,
                               treatment == 0 ~ 1))

data <- data %>% 
  arrange(year, settlement) %>% as.data.frame()


setup = panel.matrices(data)
tau.hat = synthdid_estimate(setup$Y, setup$N0, setup$T0)
se = sqrt(vcov(tau.hat, method='placebo'))
sprintf('point estimate: %1.2f', tau.hat)
sprintf('95%% CI (%1.2f, %1.2f)', tau.hat - 1.96 * se, tau.hat + 1.96 * se)

plot(tau.hat, se.method='placebo')
synthdid_units_plot(tau.hat, se.method='placebo')
plot(tau.hat, overlay=1,  se.method='placebo')
plot(tau.hat, overlay=.8, se.method='placebo')

tau.sc   = sc_estimate(setup$Y, setup$N0, setup$T0)
tau.did  = did_estimate(setup$Y, setup$N0, setup$T0)
estimates = list(tau.did, tau.sc, tau.hat)
names(estimates) = c('Diff-in-Diff', 'Synthetic Control', 'Synthetic Diff-in-Diff')
print(unlist(estimates))
synthdid_plot(estimates, se.method='placebo')
synthdid_units_plot(estimates, se.method='placebo')


################################################################################

y_var <- "t8008008_t8008007" # по Пятигорску нет данных
city <- "Пятигорск"

data <- big_cities %>% 
  select(c("settlement", "year", all_of(y_var), "treatment", "treatment_status")) %>%
  filter(year %in% 2006:2014)

data <- data %>% 
  group_by(settlement) %>% mutate(group = get_group(treatment_status)) %>% ungroup()

always_treated <- (data %>% filter(group %in% c("1")))$settlement %>% unique()
never_treated <- (data %>% filter(group %in% c("0")))$settlement %>% unique()
data$treatment_status <- NULL
data$group <- NULL

data <- data %>% 
  filter(!is.na(data[, paste(y_var)])) %>% 
  filter(settlement %in% c(city, all_of(always_treated))) %>% 
  make.pbalanced(balance.type = "shared.individuals")

data <- data %>% 
  mutate(treatment = case_when(treatment == 1 ~ 0,
                               treatment == 0 ~ 1))

data <- data %>% 
  arrange(year, settlement) %>% as.data.frame()


setup = panel.matrices(data)
tau.hat = synthdid_estimate(setup$Y, setup$N0, setup$T0)
se = sqrt(vcov(tau.hat, method='placebo'))
sprintf('point estimate: %1.2f', tau.hat)
sprintf('95%% CI (%1.2f, %1.2f)', tau.hat - 1.96 * se, tau.hat + 1.96 * se)

plot(tau.hat, se.method='placebo')
synthdid_units_plot(tau.hat, se.method='placebo')
plot(tau.hat, overlay=1,  se.method='placebo')
plot(tau.hat, overlay=.8, se.method='placebo')

tau.sc   = sc_estimate(setup$Y, setup$N0, setup$T0)
tau.did  = did_estimate(setup$Y, setup$N0, setup$T0)
estimates = list(tau.did, tau.sc, tau.hat)
names(estimates) = c('Diff-in-Diff', 'Synthetic Control', 'Synthetic Diff-in-Diff')
print(unlist(estimates))
synthdid_plot(estimates, se.method='placebo')
synthdid_units_plot(estimates, se.method='placebo')







