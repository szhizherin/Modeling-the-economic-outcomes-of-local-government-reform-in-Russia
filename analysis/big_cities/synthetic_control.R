# Семен Жижерин 402




library(dplyr)
library(tidysynth)
library(readr)
library(stargazer)




#########################################################################
########################         Данные          ########################
#########################################################################


data <- read_csv("HA3_2021_data.csv") %>% 
  arrange(Country, Year) %>% 
  as.data.frame() %>% 
  rename(Real_GDP = `Real GDP`)


#########################################################################
########################       Задание 2.1       ########################
#########################################################################


Cabo_Verde_data <- data %>% 
  filter(!(Country %in% c("Mozambique", "Nigeria", "Rwanda", "Uganda", "Senegal", "Tanzania")))

Cabo_Verde <- Cabo_Verde_data %>% 
  
  synthetic_control(outcome = Real_GDP, 
                    unit = Country, 
                    time = Year, 
                    i_unit = "Cabo Verde", 
                    i_time = 2007, 
                    generate_placebos = T 
  ) %>%
  
  generate_predictor(time_window = 1992:2007,
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
  
  generate_weights(optimization_window = 1992:2007) %>% 
  
  generate_control()

Cabo_Verde %>% plot_trends()


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

