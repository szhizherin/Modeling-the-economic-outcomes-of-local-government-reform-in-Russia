# Purpose: create municipalities table for further analysis
# Inputs:  intermediate_data/BDMO_panel.csv
#          intermediate_data/goods_and_services_prices.csv
#          intermediate_data/CPI.csv
# Outputs: final_data/municipalities.csv




library(did)
library(dplyr)
library(readr)




municipalities <- read_csv("final_data/municipalities.csv") %>% select(-c(1))
BDMO_id_name <- read_csv("intermediate_data/BDMO_id_name.csv") %>% select(-c(1))


municipalities <- municipalities %>% arrange(oktmo, year)

get_code <- function(var_name) {
  var_id <- (BDMO_id_name %>% filter(name == var_name))$id[1]
  return(var_id)
}


################################################################################
#  Общий объем расходов бюджета муниципального образования|||t8313001|||Всего  #
#                                                                              #
#                                                                              #
################################################################################

"Общий объем расходов бюджета муниципального образования|||t8313001|||Всего" %>% get_code()

# "c" stands for inflation- and regional prices-corrected 
municipalities$t8313001_1_c <- municipalities$t8313001_1 * municipalities$index

y_id <- "t8313001_1_c"

X_names <- c("Оценка численности населения на 1 января текущего года|||t8112027|||Все население",
             "Удельный вес прибыльных организаций в общем числе организаций (по 2016 год)|||t8042018|||Всего_Всего",
             "Среднемесячная заработная плата работников организаций (по 2016 год)|||t8123007|||Всего",
             "Наличие основных фондов на конец года по полной учетной стоимости по некоммерческим организациям муниципальной формы собственности|||t8045002|||Всего основных фондов",
             "Наличие основных фондов на конец года по полной учетной стоимости по некоммерческим организациям муниципальной формы собственности|||t8045002|||Здания",
             "Кредиторская задолженность (по 2016 год)|||t8042005|||Муниципальная собственность_Всего",
             "Протяженность автодорог общего пользования местного значения, находящихся в собственности муниципальных образований на конец года|||t8006005|||всего",
             "Общая протяженность улиц, проездов, набережных на конец года")

for (name in X_names) {
  print(name %>% get_code())
}


municipalities$t8123007_0 %>% is.na() %>% sum()

x_fm <- ~ 1 + t8042018_kfs199_okved0 + t8045002_0


out <- att_gt(yname = y_id,
              gname = "first.treat",
              idname = "oktmo",
              tname = "year",
              xformla = x_fm,
              data = municipalities,
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


################################################################################
#  Общая протяженность освещенных частей улиц, проездов, набережных на конец   #
#  года / Общая протяженность улиц, проездов, набережных на конец года         #
#                                                                              #
################################################################################


"Общая протяженность освещенных частей улиц, проездов, набережных на конец года" %>% get_code()
"Общая протяженность улиц, проездов, набережных на конец года" %>% get_code()

municipalities["t8006003/t8006007"] <- municipalities$t8006003 / municipalities$t8006007

y_id <- "t8006003"
x_fm <- ~ 1 + t8042018_kfs199_okved0


out <- att_gt(yname = y_id,
              gname = "first.treat",
              idname = "oktmo",
              tname = "year",
              xformla = x_fm,
              data = municipalities,
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
              data = municipalities,
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
              data = municipalities,
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
              data = municipalities,
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