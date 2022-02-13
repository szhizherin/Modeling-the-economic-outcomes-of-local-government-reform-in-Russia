# Purpose: create goods and services prices table for nominal variables weighting
# Inputs:  raw_data/Стоимость фиксированного набора товаров и услуг.xls
#          raw_data/regions_oktmo.xlsx
# Outputs: intermediate_data/goods_and_services_prices.csv




library(dplyr)
library(readxl)




raw_prices <- read_excel("raw_data/Стоимость фиксированного набора товаров и услуг.xls", 
                         col_names = c("region", 1:192), skip = 2)

goods_and_services_prices <- data.frame(region = raw_prices$region)

# average monthly values for each year
count <- 2
for (year in 2:17) {
  goods_and_services_prices[, year] <- raw_prices %>% 
    select(count:count+11) %>% 
    rowMeans()
  count <- count + 12
}

colnames(goods_and_services_prices) <- c("region", 2006:2021)


# add oktmo codes
regions_oktmo <- read_excel("raw_data/regions_oktmo.xlsx")
goods_and_services_prices <- goods_and_services_prices %>% 
  merge(regions_oktmo, by = "region")

goods_and_services_prices %>% write.csv("intermediate_data/goods_and_services_prices.csv", 
                                        fileEncoding = "UTF-8")

