# Purpose: estimate synthetic control models on big_cities data
# Inputs:  intermediate_data/BDMO_id_name.csv
#          final_data/big_cities.csv
# Outputs: -




library(dplyr)
library(plm)
library(readr)
library(tidysynth)
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



# TODO: move to EDA
big_cities %>% select(municipality, year, treatment_status, model, group) %>% 
  filter(group == "0 -> 1") %>% View()
# Калининград и Пятигорск - примеры обратного перехода
# Березники и Миасский - тоже, в каком-то смысле
# эти 4 - пожалуй, самые интересные кейсы
# Астрахань, Вологда - длинный претритмент


library(ggplot2)
ggplot(big_cities, aes(x = group)) +
  geom_bar()

always_treated <- (big_cities %>% filter(group %in% c("1", "2", "1 -> 2")))$settlement %>% unique()
# !(settlement %in% c("Воркута", "Ангарск", "Киселевск", "Новотроицк"))
never_treated <- (big_cities %>% filter(group %in% c("0")))$settlement %>% unique()


################################################################################

y_var <- "t8013002_1_c" 
city <- "Калининград"
cov_vars <- c("build_flat", "catering_c", "construction_c", "doctors_per10", 
              "living_space", "n_companies", "pop_work", "log_population", 
              "retail_c", "log_wage", "workers", "t8006003")


data <- big_cities %>% 
  select(c("settlement", "year", "treatment", all_of(y_var), all_of(cov_vars))) %>%
  filter(settlement %in% c(city, all_of(always_treated)))

data <- data %>% 
  filter(!is.na(data[, paste(y_var)])) %>% 
  make.pbalanced(balance.type = "shared.individuals")

data %>% is.na() %>% sum()

# "shared.individuals" - выбросить страны, для которых не хватает лет
# "shared.times" - выбросить года, для которых не хватает стран

# в этой конструкции нужно руками проставлять зависимую переменную
result <- data %>% 
  
  synthetic_control(outcome = t8013002_1_c, 
                    unit = settlement, 
                    time = year, 
                    i_unit = city, 
                    i_time = 2012, 
                    generate_placebos = T 
  ) %>%
  
  generate_predictor(time_window = 2007:2012,
                     build_flat = mean(build_flat, na.rm = T),
                     catering_c = mean(catering_c, na.rm = T),
                     construction_c = mean(construction_c, na.rm = T),
                     doctors_per10 = mean(doctors_per10, na.rm = T),
                     living_space = mean(living_space, na.rm = T),
                     n_companies = mean(n_companies, na.rm = T),
                     pop_work = mean(pop_work, na.rm = T),
                     log_population = mean(log_population, na.rm = T),
                     retail_c = mean(retail_c, na.rm = T),
                     log_wage = mean(log_wage, na.rm = T),
                     workers = mean(workers, na.rm = T),
                     streets_with_light = mean(t8006003, na.rm = T),
                     
  ) %>%
  
  generate_predictor(time_window = 2007,
                     investment_2007 = t8013002_1_c) %>% 
  
  generate_predictor(time_window = 2010,
                     investment_2010 = t8013002_1_c) %>% 
  
  generate_predictor(time_window = 2012,
                     investment_2012 = t8013002_1_c) %>%
  
  generate_weights(optimization_window = 2007:2012) %>% 
  
  generate_control()

result %>% plot_trends()
result %>% plot_differences()
result %>% plot_weights()


################################################################################

y_var <- "t8013002_212_c" 
city <- "Калининград"
cov_vars <- c("build_flat", "catering_c", "construction_c", "doctors_per10", 
              "living_space", "n_companies", "pop_work", "log_population", 
              "retail_c", "log_wage", "workers", "t8006003")


data <- big_cities %>% 
  select(c("settlement", "year", "treatment", all_of(y_var), all_of(cov_vars))) %>%
  filter(settlement %in% c(city, all_of(always_treated)))

data <- data %>% 
  filter(!is.na(data[, paste(y_var)])) %>% 
  make.pbalanced(balance.type = "shared.individuals")

data %>% is.na() %>% sum()

# "shared.individuals" - выбросить страны, для которых не хватает лет
# "shared.times" - выбросить года, для которых не хватает стран

# в этой конструкции нужно руками проставлять зависимую переменную
result <- data %>% 
  
  synthetic_control(outcome = t8013002_212_c, 
                    unit = settlement, 
                    time = year, 
                    i_unit = city, 
                    i_time = 2012, 
                    generate_placebos = T 
  ) %>%
  
  generate_predictor(time_window = 2007:2012,
                     build_flat = mean(build_flat, na.rm = T),
                     catering_c = mean(catering_c, na.rm = T),
                     construction_c = mean(construction_c, na.rm = T),
                     doctors_per10 = mean(doctors_per10, na.rm = T),
                     living_space = mean(living_space, na.rm = T),
                     n_companies = mean(n_companies, na.rm = T),
                     pop_work = mean(pop_work, na.rm = T),
                     log_population = mean(log_population, na.rm = T),
                     retail_c = mean(retail_c, na.rm = T),
                     log_wage = mean(log_wage, na.rm = T),
                     workers = mean(workers, na.rm = T),
                     streets_with_light = mean(t8006003, na.rm = T),
                     
  ) %>%
  
  generate_predictor(time_window = 2007,
                     investment_2007 = t8013002_212_c) %>% 
  
  generate_predictor(time_window = 2010,
                     investment_2010 = t8013002_212_c) %>% 
  
  generate_predictor(time_window = 2012,
                     investment_2012 = t8013002_212_c) %>%
  
  generate_weights(optimization_window = 2007:2012) %>% 
  
  generate_control()

result %>% plot_trends()
result %>% plot_differences()
result %>% plot_weights()


################################################################################

y_var <- "t8013002_220_c" 
city <- "Калининград"
cov_vars <- c("build_flat", "catering_c", "construction_c", "doctors_per10", 
              "living_space", "n_companies", "pop_work", "log_population", 
              "retail_c", "log_wage", "workers", "t8006003")


data <- big_cities %>% 
  select(c("settlement", "year", "treatment", all_of(y_var), all_of(cov_vars))) %>%
  filter(settlement %in% c(city, all_of(always_treated)))

data <- data %>% 
  filter(!is.na(data[, paste(y_var)])) %>% 
  make.pbalanced(balance.type = "shared.individuals")

data %>% is.na() %>% sum()

# "shared.individuals" - выбросить страны, для которых не хватает лет
# "shared.times" - выбросить года, для которых не хватает стран

# в этой конструкции нужно руками проставлять зависимую переменную
result <- data %>% 
  
  synthetic_control(outcome = t8013002_220_c, 
                    unit = settlement, 
                    time = year, 
                    i_unit = city, 
                    i_time = 2012, 
                    generate_placebos = T 
  ) %>%
  
  generate_predictor(time_window = 2007:2012,
                     build_flat = mean(build_flat, na.rm = T),
                     catering_c = mean(catering_c, na.rm = T),
                     construction_c = mean(construction_c, na.rm = T),
                     doctors_per10 = mean(doctors_per10, na.rm = T),
                     living_space = mean(living_space, na.rm = T),
                     n_companies = mean(n_companies, na.rm = T),
                     pop_work = mean(pop_work, na.rm = T),
                     log_population = mean(log_population, na.rm = T),
                     retail_c = mean(retail_c, na.rm = T),
                     log_wage = mean(log_wage, na.rm = T),
                     workers = mean(workers, na.rm = T),
                     streets_with_light = mean(t8006003, na.rm = T),
                     
  ) %>%
  
  generate_predictor(time_window = 2007,
                     investment_2007 = t8013002_220_c) %>% 
  
  generate_predictor(time_window = 2010,
                     investment_2010 = t8013002_220_c) %>% 
  
  generate_predictor(time_window = 2012,
                     investment_2012 = t8013002_220_c) %>%
  
  generate_weights(optimization_window = 2007:2012) %>% 
  
  generate_control()

result %>% plot_trends()
result %>% plot_differences()
result %>% plot_weights()


################################################################################

y_var <- "t8013002_221_c" 
city <- "Калининград"
cov_vars <- c("build_flat", "catering_c", "construction_c", "doctors_per10", 
              "living_space", "n_companies", "pop_work", "log_population", 
              "retail_c", "log_wage", "workers", "t8006003")


data <- big_cities %>% 
  select(c("settlement", "year", "treatment", all_of(y_var), all_of(cov_vars))) %>%
  filter(settlement %in% c(city, all_of(always_treated)))

data <- data %>% 
  filter(!is.na(data[, paste(y_var)])) %>% 
  make.pbalanced(balance.type = "shared.individuals")

data %>% is.na() %>% sum()

# "shared.individuals" - выбросить страны, для которых не хватает лет
# "shared.times" - выбросить года, для которых не хватает стран

# в этой конструкции нужно руками проставлять зависимую переменную
result <- data %>% 
  
  synthetic_control(outcome = t8013002_221_c, 
                    unit = settlement, 
                    time = year, 
                    i_unit = city, 
                    i_time = 2012, 
                    generate_placebos = T 
  ) %>%
  
  generate_predictor(time_window = 2007:2012,
                     build_flat = mean(build_flat, na.rm = T),
                     catering_c = mean(catering_c, na.rm = T),
                     construction_c = mean(construction_c, na.rm = T),
                     doctors_per10 = mean(doctors_per10, na.rm = T),
                     living_space = mean(living_space, na.rm = T),
                     n_companies = mean(n_companies, na.rm = T),
                     pop_work = mean(pop_work, na.rm = T),
                     log_population = mean(log_population, na.rm = T),
                     retail_c = mean(retail_c, na.rm = T),
                     log_wage = mean(log_wage, na.rm = T),
                     workers = mean(workers, na.rm = T),
                     streets_with_light = mean(t8006003, na.rm = T),
                     
  ) %>%
  
  generate_predictor(time_window = 2007,
                     investment_2007 = t8013002_221_c) %>% 
  
  generate_predictor(time_window = 2010,
                     investment_2010 = t8013002_221_c) %>% 
  
  generate_predictor(time_window = 2012,
                     investment_2012 = t8013002_221_c) %>%
  
  generate_weights(optimization_window = 2007:2012) %>% 
  
  generate_control()

result %>% plot_trends()
result %>% plot_differences()
result %>% plot_weights()


################################################################################

y_var <- "t8013002_229_c" 
city <- "Калининград"
cov_vars <- c("build_flat", "catering_c", "construction_c", "doctors_per10", 
              "living_space", "n_companies", "pop_work", "log_population", 
              "retail_c", "log_wage", "workers", "t8006003")


data <- big_cities %>% 
  select(c("settlement", "year", "treatment", all_of(y_var), all_of(cov_vars))) %>%
  filter(settlement %in% c(city, all_of(always_treated)))

data <- data %>% 
  filter(!is.na(data[, paste(y_var)])) %>% 
  make.pbalanced(balance.type = "shared.individuals")

data %>% is.na() %>% sum()

# "shared.individuals" - выбросить страны, для которых не хватает лет
# "shared.times" - выбросить года, для которых не хватает стран

# в этой конструкции нужно руками проставлять зависимую переменную
result <- data %>% 
  
  synthetic_control(outcome = t8013002_229_c, 
                    unit = settlement, 
                    time = year, 
                    i_unit = city, 
                    i_time = 2012, 
                    generate_placebos = T 
  ) %>%
  
  generate_predictor(time_window = 2007:2012,
                     build_flat = mean(build_flat, na.rm = T),
                     catering_c = mean(catering_c, na.rm = T),
                     construction_c = mean(construction_c, na.rm = T),
                     doctors_per10 = mean(doctors_per10, na.rm = T),
                     living_space = mean(living_space, na.rm = T),
                     n_companies = mean(n_companies, na.rm = T),
                     pop_work = mean(pop_work, na.rm = T),
                     log_population = mean(log_population, na.rm = T),
                     retail_c = mean(retail_c, na.rm = T),
                     log_wage = mean(log_wage, na.rm = T),
                     workers = mean(workers, na.rm = T),
                     streets_with_light = mean(t8006003, na.rm = T),
                     
  ) %>%
  
  generate_predictor(time_window = 2007,
                     investment_2007 = t8013002_229_c) %>% 
  
  generate_predictor(time_window = 2010,
                     investment_2010 = t8013002_229_c) %>% 
  
  generate_predictor(time_window = 2012,
                     investment_2012 = t8013002_229_c) %>%
  
  generate_weights(optimization_window = 2007:2012) %>% 
  
  generate_control()

result %>% plot_trends()
result %>% plot_differences()
result %>% plot_weights()


################################################################################

y_var <- "t8013002_234_c" 
city <- "Калининград"
cov_vars <- c("build_flat", "catering_c", "construction_c", "doctors_per10", 
              "living_space", "n_companies", "pop_work", "log_population", 
              "retail_c", "log_wage", "workers", "t8006003")


data <- big_cities %>% 
  select(c("settlement", "year", "treatment", all_of(y_var), all_of(cov_vars))) %>%
  filter(settlement %in% c(city, all_of(always_treated)))

data <- data %>% 
  filter(!is.na(data[, paste(y_var)])) %>% 
  make.pbalanced(balance.type = "shared.individuals")

data %>% is.na() %>% sum()

# "shared.individuals" - выбросить страны, для которых не хватает лет
# "shared.times" - выбросить года, для которых не хватает стран

# в этой конструкции нужно руками проставлять зависимую переменную
result <- data %>% 
  
  synthetic_control(outcome = t8013002_234_c, 
                    unit = settlement, 
                    time = year, 
                    i_unit = city, 
                    i_time = 2012, 
                    generate_placebos = T 
  ) %>%
  
  generate_predictor(time_window = 2007:2012,
                     build_flat = mean(build_flat, na.rm = T),
                     catering_c = mean(catering_c, na.rm = T),
                     construction_c = mean(construction_c, na.rm = T),
                     doctors_per10 = mean(doctors_per10, na.rm = T),
                     living_space = mean(living_space, na.rm = T),
                     n_companies = mean(n_companies, na.rm = T),
                     pop_work = mean(pop_work, na.rm = T),
                     log_population = mean(log_population, na.rm = T),
                     retail_c = mean(retail_c, na.rm = T),
                     log_wage = mean(log_wage, na.rm = T),
                     workers = mean(workers, na.rm = T),
                     streets_with_light = mean(t8006003, na.rm = T),
                     
  ) %>%
  
  generate_predictor(time_window = 2007,
                     investment_2007 = t8013002_234_c) %>% 
  
  generate_predictor(time_window = 2010,
                     investment_2010 = t8013002_234_c) %>% 
  
  generate_predictor(time_window = 2012,
                     investment_2012 = t8013002_234_c) %>%
  
  generate_weights(optimization_window = 2007:2012) %>% 
  
  generate_control()

result %>% plot_trends()
result %>% plot_differences()
result %>% plot_weights()


################################################################################

y_var <- "invest_budg" 
city <- "Калининград"
cov_vars <- c("build_flat", "catering_c", "construction_c", "doctors_per10", 
              "living_space", "n_companies", "pop_work", "log_population", 
              "retail_c", "log_wage", "workers", "t8006003")


data <- big_cities %>% 
  select(c("settlement", "year", "treatment", all_of(y_var), all_of(cov_vars))) %>%
  filter(settlement %in% c(city, all_of(always_treated)))

data <- data %>% 
  filter(!is.na(data[, paste(y_var)])) %>% 
  make.pbalanced(balance.type = "shared.individuals")

data %>% is.na() %>% sum()

# "shared.individuals" - выбросить страны, для которых не хватает лет
# "shared.times" - выбросить года, для которых не хватает стран

# в этой конструкции нужно руками проставлять зависимую переменную
result <- data %>% 
  
  synthetic_control(outcome = invest_budg, 
                    unit = settlement, 
                    time = year, 
                    i_unit = city, 
                    i_time = 2012, 
                    generate_placebos = T 
  ) %>%
  
  generate_predictor(time_window = 2007:2012,
                     build_flat = mean(build_flat, na.rm = T),
                     catering_c = mean(catering_c, na.rm = T),
                     construction_c = mean(construction_c, na.rm = T),
                     doctors_per10 = mean(doctors_per10, na.rm = T),
                     living_space = mean(living_space, na.rm = T),
                     n_companies = mean(n_companies, na.rm = T),
                     pop_work = mean(pop_work, na.rm = T),
                     log_population = mean(log_population, na.rm = T),
                     retail_c = mean(retail_c, na.rm = T),
                     log_wage = mean(log_wage, na.rm = T),
                     workers = mean(workers, na.rm = T),
                     streets_with_light = mean(t8006003, na.rm = T),
                     
  ) %>%
  
  generate_predictor(time_window = 2007,
                     investment_2007 = invest_budg) %>% 
  
  generate_predictor(time_window = 2010,
                     investment_2010 = invest_budg) %>% 
  
  generate_predictor(time_window = 2012,
                     investment_2012 = invest_budg) %>%
  
  generate_weights(optimization_window = 2007:2012) %>% 
  
  generate_control()

result %>% plot_trends()
result %>% plot_differences()
result %>% plot_weights()


################################################################################

y_var <- "invest_fed" 
city <- "Калининград"
cov_vars <- c("build_flat", "catering_c", "construction_c", "doctors_per10", 
              "living_space", "n_companies", "pop_work", "log_population", 
              "retail_c", "log_wage", "workers", "t8006003")


data <- big_cities %>% 
  select(c("settlement", "year", "treatment", all_of(y_var), all_of(cov_vars))) %>%
  filter(settlement %in% c(city, all_of(always_treated)))

data <- data %>% 
  filter(!is.na(data[, paste(y_var)])) %>% 
  make.pbalanced(balance.type = "shared.individuals")

data %>% is.na() %>% sum()

# "shared.individuals" - выбросить страны, для которых не хватает лет
# "shared.times" - выбросить года, для которых не хватает стран

# в этой конструкции нужно руками проставлять зависимую переменную
result <- data %>% 
  
  synthetic_control(outcome = invest_fed, 
                    unit = settlement, 
                    time = year, 
                    i_unit = city, 
                    i_time = 2012, 
                    generate_placebos = T 
  ) %>%
  
  generate_predictor(time_window = 2007:2012,
                     build_flat = mean(build_flat, na.rm = T),
                     catering_c = mean(catering_c, na.rm = T),
                     construction_c = mean(construction_c, na.rm = T),
                     doctors_per10 = mean(doctors_per10, na.rm = T),
                     living_space = mean(living_space, na.rm = T),
                     n_companies = mean(n_companies, na.rm = T),
                     pop_work = mean(pop_work, na.rm = T),
                     log_population = mean(log_population, na.rm = T),
                     retail_c = mean(retail_c, na.rm = T),
                     log_wage = mean(log_wage, na.rm = T),
                     workers = mean(workers, na.rm = T),
                     streets_with_light = mean(t8006003, na.rm = T),
                     dep_var_mean = mean(invest_fed, na.rm = T)
                     
  ) %>%
  
  generate_predictor(time_window = 2007,
                     investment_2007 = invest_fed) %>% 
  
  generate_predictor(time_window = 2010,
                     investment_2010 = invest_fed) %>% 
  
  generate_predictor(time_window = 2012,
                     investment_2012 = invest_fed) %>%
  
  generate_weights(optimization_window = 2007:2012) %>% 
  
  generate_control()

result %>% plot_trends()
result %>% plot_differences()
result %>% plot_weights()
result %>% plot_placebos()


################################################################################

y_var <- "investment_c" 
city <- "Калининград"
cov_vars <- c("build_flat", "catering_c", "construction_c", "doctors_per10", 
              "living_space", "n_companies", "pop_work", "log_population", 
              "retail_c", "log_wage", "workers", "t8006003")


data <- big_cities %>% 
  select(c("settlement", "year", "treatment", all_of(y_var), all_of(cov_vars))) %>%
  filter(settlement %in% c(city, all_of(always_treated)))

data <- data %>% 
  filter(!is.na(data[, paste(y_var)])) %>% 
  make.pbalanced(balance.type = "shared.individuals")

data %>% is.na() %>% sum()

# "shared.individuals" - выбросить страны, для которых не хватает лет
# "shared.times" - выбросить года, для которых не хватает стран

# в этой конструкции нужно руками проставлять зависимую переменную
result <- data %>% 
  
  synthetic_control(outcome = investment_c, 
                    unit = settlement, 
                    time = year, 
                    i_unit = city, 
                    i_time = 2012, 
                    generate_placebos = T 
  ) %>%
  
  generate_predictor(time_window = 2007:2012,
                     build_flat = mean(build_flat, na.rm = T),
                     catering_c = mean(catering_c, na.rm = T),
                     construction_c = mean(construction_c, na.rm = T),
                     doctors_per10 = mean(doctors_per10, na.rm = T),
                     living_space = mean(living_space, na.rm = T),
                     n_companies = mean(n_companies, na.rm = T),
                     pop_work = mean(pop_work, na.rm = T),
                     log_population = mean(log_population, na.rm = T),
                     retail_c = mean(retail_c, na.rm = T),
                     log_wage = mean(log_wage, na.rm = T),
                     workers = mean(workers, na.rm = T),
                     streets_with_light = mean(t8006003, na.rm = T),
                     dep_var_mean = mean(investment_c, na.rm = T)
                     
  ) %>%
  
  generate_predictor(time_window = 2007,
                     investment_2007 = investment_c) %>% 
  
  generate_predictor(time_window = 2010,
                     investment_2010 = investment_c) %>% 
  
  generate_predictor(time_window = 2012,
                     investment_2012 = investment_c) %>%
  
  generate_weights(optimization_window = 2007:2012) %>% 
  
  generate_control()

result %>% plot_trends()
result %>% plot_differences()
result %>% plot_weights()
result %>% plot_placebos()


################################################################################

y_var <- "volume_electr_c" # хороший плацебо-тест (по эому показателю осоо ничего и не должно было меняться)
city <- "Калининград"
cov_vars <- c("build_flat", "catering_c", "construction_c", "doctors_per10", 
              "living_space", "n_companies", "pop_work", "log_population", 
              "retail_c", "log_wage", "workers", "t8006003")


data <- big_cities %>% 
  select(c("settlement", "year", "treatment", all_of(y_var), all_of(cov_vars))) %>%
  filter(settlement %in% c(city, all_of(always_treated)))

data <- data %>% 
  filter(!is.na(data[, paste(y_var)])) %>% 
  make.pbalanced(balance.type = "shared.individuals")

data %>% is.na() %>% sum()

# "shared.individuals" - выбросить страны, для которых не хватает лет
# "shared.times" - выбросить года, для которых не хватает стран

# в этой конструкции нужно руками проставлять зависимую переменную
result <- data %>% 
  
  synthetic_control(outcome = volume_electr_c, 
                    unit = settlement, 
                    time = year, 
                    i_unit = city, 
                    i_time = 2012, 
                    generate_placebos = T 
  ) %>%
  
  generate_predictor(time_window = 2007:2012,
                     build_flat = mean(build_flat, na.rm = T),
                     catering_c = mean(catering_c, na.rm = T),
                     construction_c = mean(construction_c, na.rm = T),
                     doctors_per10 = mean(doctors_per10, na.rm = T),
                     living_space = mean(living_space, na.rm = T),
                     n_companies = mean(n_companies, na.rm = T),
                     pop_work = mean(pop_work, na.rm = T),
                     log_population = mean(log_population, na.rm = T),
                     retail_c = mean(retail_c, na.rm = T),
                     log_wage = mean(log_wage, na.rm = T),
                     workers = mean(workers, na.rm = T),
                     streets_with_light = mean(t8006003, na.rm = T),
                     dep_var_mean = mean(volume_electr_c, na.rm = T)
                     
  ) %>%
  
  generate_predictor(time_window = 2007,
                     investment_2007 = volume_electr_c) %>% 
  
  generate_predictor(time_window = 2010,
                     investment_2010 = volume_electr_c) %>% 
  
  generate_predictor(time_window = 2012,
                     investment_2012 = volume_electr_c) %>%
  
  generate_weights(optimization_window = 2007:2012) %>% 
  
  generate_control()

result %>% plot_trends()
result %>% plot_differences()
result %>% plot_weights()
result %>% plot_placebos()


################################################################################

y_var <- "volume_electr_c" # хороший плацебо-тест (по эому показателю осоо ничего и не должно было меняться)
city <- "Калининград"
cov_vars <- c("build_flat", "catering_c", "construction_c", "doctors_per10", 
              "living_space", "n_companies", "pop_work", "log_population", 
              "retail_c", "log_wage", "workers", "t8006003")


data <- big_cities %>% 
  select(c("settlement", "year", "treatment", all_of(y_var), all_of(cov_vars))) %>%
  filter(settlement %in% c(city, all_of(always_treated)))

data <- data %>% 
  filter(!is.na(data[, paste(y_var)])) %>% 
  make.pbalanced(balance.type = "shared.individuals")

data %>% is.na() %>% sum()

# "shared.individuals" - выбросить страны, для которых не хватает лет
# "shared.times" - выбросить года, для которых не хватает стран

# в этой конструкции нужно руками проставлять зависимую переменную
result <- data %>% 
  
  synthetic_control(outcome = volume_electr_c, 
                    unit = settlement, 
                    time = year, 
                    i_unit = city, 
                    i_time = 2012, 
                    generate_placebos = T 
  ) %>%
  
  generate_predictor(time_window = 2007:2012,
                     build_flat = mean(build_flat, na.rm = T),
                     catering_c = mean(catering_c, na.rm = T),
                     construction_c = mean(construction_c, na.rm = T),
                     doctors_per10 = mean(doctors_per10, na.rm = T),
                     living_space = mean(living_space, na.rm = T),
                     n_companies = mean(n_companies, na.rm = T),
                     pop_work = mean(pop_work, na.rm = T),
                     log_population = mean(log_population, na.rm = T),
                     retail_c = mean(retail_c, na.rm = T),
                     log_wage = mean(log_wage, na.rm = T),
                     workers = mean(workers, na.rm = T),
                     streets_with_light = mean(t8006003, na.rm = T),
                     dep_var_mean = mean(volume_electr_c, na.rm = T)
                     
  ) %>%
  
  generate_predictor(time_window = 2007,
                     investment_2007 = volume_electr_c) %>% 
  
  generate_predictor(time_window = 2010,
                     investment_2010 = volume_electr_c) %>% 
  
  generate_predictor(time_window = 2012,
                     investment_2012 = volume_electr_c) %>%
  
  generate_weights(optimization_window = 2007:2012) %>% 
  
  generate_control()

result %>% plot_trends()
result %>% plot_differences()
result %>% plot_weights()
result %>% plot_placebos()


################################################################################

y_var <- "volume_manufact_c" 
city <- "Калининград"
cov_vars <- c("build_flat", "catering_c", "construction_c", "doctors_per10", 
              "living_space", "n_companies", "pop_work", "log_population", 
              "retail_c", "log_wage", "workers", "t8006003")


data <- big_cities %>% 
  select(c("settlement", "year", "treatment", all_of(y_var), all_of(cov_vars))) %>%
  filter(settlement %in% c(city, all_of(always_treated)))

data <- data %>% 
  filter(!is.na(data[, paste(y_var)])) %>% 
  make.pbalanced(balance.type = "shared.individuals")

data %>% is.na() %>% sum()

# "shared.individuals" - выбросить страны, для которых не хватает лет
# "shared.times" - выбросить года, для которых не хватает стран

# в этой конструкции нужно руками проставлять зависимую переменную
result <- data %>% 
  
  synthetic_control(outcome = volume_manufact_c, 
                    unit = settlement, 
                    time = year, 
                    i_unit = city, 
                    i_time = 2012, 
                    generate_placebos = T 
  ) %>%
  
  generate_predictor(time_window = 2007:2012,
                     build_flat = mean(build_flat, na.rm = T),
                     catering_c = mean(catering_c, na.rm = T),
                     construction_c = mean(construction_c, na.rm = T),
                     doctors_per10 = mean(doctors_per10, na.rm = T),
                     living_space = mean(living_space, na.rm = T),
                     n_companies = mean(n_companies, na.rm = T),
                     pop_work = mean(pop_work, na.rm = T),
                     log_population = mean(log_population, na.rm = T),
                     retail_c = mean(retail_c, na.rm = T),
                     log_wage = mean(log_wage, na.rm = T),
                     workers = mean(workers, na.rm = T),
                     streets_with_light = mean(t8006003, na.rm = T)
  ) %>%
  
  generate_predictor(time_window = 2007,
                     investment_2007 = volume_manufact_c) %>% 
  
  generate_predictor(time_window = 2010,
                     investment_2010 = volume_manufact_c) %>% 
  
  generate_predictor(time_window = 2012,
                     investment_2012 = volume_manufact_c) %>%
  
  generate_weights(optimization_window = 2007:2012) %>% 
  
  generate_control()

result %>% plot_trends()
result %>% plot_differences()
result %>% plot_weights()
result %>% plot_placebos()


################################################################################

y_var <- "t8008008_t8008007" 
city <- "Калининград"
cov_vars <- c("build_flat", "catering_c", "construction_c", "doctors_per10", 
              "living_space", "n_companies", "pop_work", "log_population", 
              "retail_c", "log_wage", "workers", "t8006003")


data <- big_cities %>% 
  select(c("settlement", "year", "treatment", all_of(y_var), all_of(cov_vars))) %>%
  filter(settlement %in% c(city, all_of(always_treated)))

data <- data %>% 
  filter(!is.na(data[, paste(y_var)])) %>% 
  make.pbalanced(balance.type = "shared.individuals")

data %>% is.na() %>% sum()

# "shared.individuals" - выбросить страны, для которых не хватает лет
# "shared.times" - выбросить года, для которых не хватает стран

# в этой конструкции нужно руками проставлять зависимую переменную
result <- data %>% 
  
  synthetic_control(outcome = t8008008_t8008007, 
                    unit = settlement, 
                    time = year, 
                    i_unit = city, 
                    i_time = 2012, 
                    generate_placebos = T 
  ) %>%
  
  generate_predictor(time_window = 2007:2012,
                     build_flat = mean(build_flat, na.rm = T),
                     catering_c = mean(catering_c, na.rm = T),
                     construction_c = mean(construction_c, na.rm = T),
                     doctors_per10 = mean(doctors_per10, na.rm = T),
                     living_space = mean(living_space, na.rm = T),
                     n_companies = mean(n_companies, na.rm = T),
                     pop_work = mean(pop_work, na.rm = T),
                     log_population = mean(log_population, na.rm = T),
                     retail_c = mean(retail_c, na.rm = T),
                     log_wage = mean(log_wage, na.rm = T),
                     workers = mean(workers, na.rm = T),
                     streets_with_light = mean(t8006003, na.rm = T)
  ) %>%
  
  generate_predictor(time_window = 2007,
                     investment_2007 = t8008008_t8008007) %>% 
  
  generate_predictor(time_window = 2010,
                     investment_2010 = t8008008_t8008007) %>% 
  
  generate_predictor(time_window = 2012,
                     investment_2012 = t8008008_t8008007) %>%
  
  generate_weights(optimization_window = 2007:2012) %>% 
  
  generate_control()

result %>% plot_trends()
result %>% plot_differences()
result %>% plot_weights()
result %>% plot_placebos()


















Nigeria_data <- data %>% 
  filter(!(Country %in% c("Mozambique", "Cabo Verde", "Rwanda", "Uganda", "Senegal", "Tanzania")))

Nigeria <- Nigeria_data %>% 
  
  synthetic_control(outcome = Real_GDP, 
                    unit = Country, 
                    time = Year, 
                    i_unit = "Nigeria", 
                    i_time = 2006, 
                    generate_placebos = T 
  ) %>%
  
  generate_predictor(time_window = 1992:2006,
                     investment_rate = mean(`investment rate`),
                     economic_openness = mean(openness),
                     population_density = mean(`population density`),
                     share_of_agriculture = mean(`share of agriculture`),
                     share_of_industry = mean(`share of industry`),
                     secondary_school = mean(`sec school enr rate`),
                     tertiary_school = mean(`tert school enr rate`),
                     abs_lat = mean(abs(latitude))
  ) %>%
  
  generate_predictor(time_window = 1995,
                     GDP_1995 = Real_GDP) %>% 
  
  generate_predictor(time_window = 2000,
                     GDP_2000 = Real_GDP) %>% 
  
  generate_predictor(time_window = 2005,
                     GDP_2005 = Real_GDP) %>% 
  
  generate_weights(optimization_window = 1992:2006) %>% 
  
  generate_control()

Nigeria %>% plot_trends()


#########################################################################
########################       Задание 2.2       ########################
#########################################################################


country_weights <- Cabo_Verde %>% 
  grab_unit_weights() %>% 
  rename(" " = unit, "Cabo Verde" = weight)

country_weights$Nigeria <- (Nigeria %>% grab_unit_weights())$weight

country_weights$Nigeria <- country_weights$Nigeria %>% round(3)
country_weights$`Cabo Verde` <- country_weights$`Cabo Verde` %>% round(3)

country_weights %>% as.data.frame() %>% 
  filter(!(Nigeria == 0 & `Cabo Verde` == 0)) %>% 
  stargazer(summary = F, type = "text")


#########################################################################
########################       Задание 2.3       ########################
#########################################################################


rmspe <- function(real_y, synth_y) {
  return(sqrt(mean((real_y - synth_y)^2)))
}

y_Cabo_Verde <- Cabo_Verde %>% 
  grab_synthetic_control() %>% filter(time_unit <= 2007)

# Fit Index для Кабо-Верде
rmspe(y_Cabo_Verde$real_y, y_Cabo_Verde$synth_y) / rmspe(y_Cabo_Verde$real_y, 0)


y_Nigeria <- Nigeria %>% 
  grab_synthetic_control() %>% filter(time_unit <= 2006)

# Fit Index для Нигерии
rmspe(y_Nigeria$real_y, y_Nigeria$synth_y) / rmspe(y_Nigeria$real_y, 0)


#########################################################################
########################       Задание 2.4       ########################
#########################################################################


Cabo_Verde %>% plot_mspe_ratio() # плацебо-тест не пройден
Nigeria %>% plot_mspe_ratio()

Nigeria %>% grab_signficance()


#########################################################################
########################       Задание 2.5       ########################
#########################################################################


CV_stats <- Cabo_Verde %>% grab_balance_table()
CV_stats$`Cabo Verde` <- CV_stats$`Cabo Verde` %>% round(2)
CV_stats$`synthetic_Cabo Verde` <- CV_stats$`synthetic_Cabo Verde` %>% round(2)
CV_stats$donor_sample <- CV_stats$donor_sample %>% round(2)

CV_stats %>% stargazer(summary = F, type = "text")


N_stats <- Nigeria %>% grab_balance_table()
N_stats$Nigeria <- N_stats$Nigeria %>% round(2)
N_stats$synthetic_Nigeria <- N_stats$synthetic_Nigeria %>% round(2)
N_stats$donor_sample <- N_stats$donor_sample %>% round(2)

N_stats %>% stargazer(summary = F, type = "latex")



#########################################################################
########################       Задание 2.6       ########################
#########################################################################


Cabo_Verde %>% plot_placebos()
Nigeria %>% plot_placebos()


#########################################################################
########################       Задание 2.7       ########################
#########################################################################


Cabo_Verde_placebo <- Cabo_Verde_data %>% 
  
  synthetic_control(outcome = Real_GDP, 
                    unit = Country, 
                    time = Year, 
                    i_unit = "Cabo Verde", 
                    i_time = 1999, 
                    generate_placebos = F 
  ) %>%
  
  generate_predictor(time_window = 1992:1999,
                     investment_rate = mean(`investment rate`),
                     economic_openness = mean(openness),
                     population_density = mean(`population density`),
                     share_of_agriculture = mean(`share of agriculture`),
                     share_of_industry = mean(`share of industry`),
                     secondary_school = mean(`sec school enr rate`),
                     tertiary_school = mean(`tert school enr rate`),
                     abs_lat = mean(abs(latitude))
  ) %>%
  
  generate_predictor(time_window = 1995,
                     GDP_1995 = Real_GDP) %>% 
  
  generate_weights(optimization_window = 1992:1999) %>% 
  
  generate_control()

Cabo_Verde_placebo %>% plot_trends()


Nigeria_placebo <- Nigeria_data %>% 
  
  synthetic_control(outcome = Real_GDP, 
                    unit = Country, 
                    time = Year, 
                    i_unit = "Nigeria", 
                    i_time = 1999, 
                    generate_placebos = F 
  ) %>%
  
  generate_predictor(time_window = 1992:1999,
                     investment_rate = mean(`investment rate`),
                     economic_openness = mean(openness),
                     population_density = mean(`population density`),
                     share_of_agriculture = mean(`share of agriculture`),
                     share_of_industry = mean(`share of industry`),
                     secondary_school = mean(`sec school enr rate`),
                     tertiary_school = mean(`tert school enr rate`),
                     abs_lat = mean(abs(latitude))
  ) %>%
  
  generate_predictor(time_window = 1995,
                     GDP_1995 = Real_GDP) %>% 
  
  generate_weights(optimization_window = 1992:1999) %>% 
  
  generate_control()

Nigeria_placebo %>% plot_trends()

