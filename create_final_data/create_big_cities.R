# Purpose: create municipalities table for further analysis
# Inputs:  intermediate_data/BDMO_panel.csv
#          intermediate_data/goods_and_services_prices.csv
#          intermediate_data/CPI.csv
# Outputs: final_data/municipalities.csv




library(dplyr)
library(readr)




structure <- read_csv("raw_data/Krupnie_goroda-RF_1985-2019_187_09.12.21/structure.csv")
data <- read_delim("raw_data/Krupnie_goroda-RF_1985-2019_187_09.12.21/data.csv", 
                   ";", escape_double = FALSE, trim_ws = TRUE)

BDMO_id_name <- read_csv("intermediate_data/BDMO_id_name.csv") %>% select(-c(1))


join_oktmo <- c()
for (i in 1:dim(data)[1]) {
  if (data$oktmo[i] %>% substr(1, 1) == "0") {
    join_oktmo <- c(join_oktmo, data$oktmo[i] %>% substr(2, 8))
  } else {
    join_oktmo <- c(join_oktmo, data$oktmo[i] %>% substr(1, 8))
  }
}

data$oktmo <- join_oktmo

treatment_data <- read_csv("raw_data/bumo_models_30122018/data.csv") %>% 
  select(oktmo, year, model)

municipalities <- read_csv("final_data/municipalities.csv") %>% 
  select(-c(1)) %>% 
  select(-c(mun_type, municipality, oktmo_munr, rayon, region, model))

# inner join
big_cities <- data %>% 
  merge(treatment_data, by = c("oktmo", "year")) %>% 
  merge(municipalities, by = c("oktmo", "year")) %>% 
  arrange(oktmo, year)


tt <- big_cities[big_cities %>% is.na() %>% colSums() < 300]
tt %>% is.na() %>% colSums()


# с долей водопроводной сети, нуждающейся в замене должно норм получиться






