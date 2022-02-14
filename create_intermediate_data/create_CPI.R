# Purpose: create CPI table for nominal variables weighting
# Inputs:  raw_data/Индексы потребительских цен на товары и услуги (процент).xls
#          raw_data/regions_oktmo.xlsx
# Outputs: intermediate_data/CPI.csv




library(data.table)
library(dplyr)
library(readxl)




raw_CPI <- read_excel("raw_data/Индексы потребительских цен на товары и услуги (процент).xls")

CPI <- raw_CPI %>% rename(region = ...1)
CPI[, 2:17] <- CPI[, 2:dim(CPI)[2]] / 100

# create an index
CPI$`2006` <- 1
for (i in 3:17) {
  CPI[, i] <- CPI[, i-1] * CPI[, i]
}


# add oktmo codes
regions_oktmo <- read_excel("raw_data/regions_oktmo.xlsx")
CPI <- CPI %>% merge(regions_oktmo, by = "region")


# convert to long format
CPI <- CPI %>% as.data.table() %>% 
  melt(id.vars = c("region", "oktmo"), value.name = "index", variable.name = "year")

CPI %>% write.csv("intermediate_data/CPI.csv", fileEncoding = "UTF-8")

