# Purpose: create big_cities table for further analysis
# Inputs:  raw_data/Krupnie_goroda-RF_1985-2019_187_09.12.21/data.csv
#          raw_data/bumo_models_30122018/data.csv
#          intermediate_data/BDMO_id_name.csv
#          final_data/municipalities.csv
# Outputs: final_data/big_cities.csv




library(dplyr)
library(readr)




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


municipalities <- read_csv("final_data/municipalities.csv") %>% 
  select(-c(1)) %>% 
  select(-c(mun_type, municipality, oktmo_munr, rayon, region, model))


big_cities <- data %>% 
  merge(municipalities, by = c("oktmo", "year"), all.x = T) %>% # left join
  arrange(oktmo, year)


big_cities %>% write.csv("final_data/big_cities.csv", 
                         fileEncoding = "UTF-8")

