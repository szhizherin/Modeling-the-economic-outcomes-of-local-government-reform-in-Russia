library(tidyverse)
library(hrbrthemes)


data <- read_csv("raw_data/bumo_models_30122018/data.csv")



data %>% filter(municipality == "Лискинский муниципальный район") %>% View()
data %>% filter(municipality == "город Воронеж") %>% View()
data %>% filter(municipality == "город Тамбов") %>% View()
data %>% filter(municipality == "Город Горно-Алтайск") %>% View()
data %>% filter(municipality == "город Кирсанов") %>% View()




type1 <- data %>% filter(model == "Избираемый мэр")
type2 <- data %>% filter(model == "Назначаемый мэр")
type3 <- data %>% filter(model == "Сити-менджер")

type1$year %>% hist(breaks = 30)
type2$year %>% hist(breaks = 30)
type3$year %>% hist(breaks = 30)


p <- type1 %>% 
  ggplot(aes(x=year)) +
  geom_histogram(binwidth=1, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  ggtitle("Избираемый мэр, число муниципалитетов") +
  theme_ipsum() +
  theme(
    plot.title = element_text(size=15),
    axis.text.x = element_text(angle = 90, vjust = 0.5)
  ) +
  scale_x_continuous("Год", labels = as.character(type1$year), breaks = type1$year)
p


p <- type2 %>% 
  ggplot(aes(x=year)) +
  geom_histogram(binwidth=0.5, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  ggtitle("Назначаемый мэр, число муниципалитетов") +
  theme_ipsum() +
  theme(
    plot.title = element_text(size=15),
    axis.text.x = element_text(angle = 90, vjust = 0.5)
  ) +
  scale_x_continuous("Год", labels = as.character(type2$year), breaks = type2$year)
p


data_nona <- data %>% drop_na()
p <- data_nona %>%
  ggplot(aes(x=year, fill=model)) +
  geom_histogram(binwidth=0.51, color="#e9ecef", alpha=0.6, position = 'identity') +
  ggtitle("Динамика формы управления") +
  scale_fill_manual(values=c("#69b3a2", "#404080", "grey")) +
  theme_ipsum() +
  theme(
    plot.title = element_text(size=15),
    axis.text.x = element_text(angle = 90, vjust = 0.5)
  ) +
  scale_x_continuous("Год", labels = as.character(data_nona$year), breaks = data_nona$year) +
  labs(fill="")
p


data_nona <- data %>% drop_na()
p <- data_nona %>%
  ggplot(aes(x=year, fill=model)) +
  geom_bar(position = 'stack') +
  ggtitle("Динамика формы управления") +
  scale_fill_manual(values=c("#69b3a2", "#404080", "grey")) +
  theme_ipsum() +
  theme(
    plot.title = element_text(size=15),
    axis.text.x = element_text(angle = 90, vjust = 0.5)
  ) +
  scale_x_continuous("Год", labels = as.character(data_nona$year), breaks = data_nona$year) +
  labs(fill="")
p


p <- data_nona %>% filter(mun_type == "Городской округ, городской округ с внутригородским делением") %>% 
  ggplot(aes(x=year, fill=model)) +
  geom_histogram(binwidth=0.51, color="#e9ecef", alpha=0.6, position = 'identity') +
  ggtitle("Динамика формы управления по городским округам") +
  scale_fill_manual(values=c("#69b3a2", "#404080", "grey")) +
  theme_ipsum() +
  theme(
    plot.title = element_text(size=15),
    axis.text.x = element_text(angle = 90, vjust = 0.5)
  ) +
  scale_x_continuous("Год", labels = as.character(data_nona$year), breaks = data_nona$year) +
  labs(fill="")
p


p <- data_nona %>% filter(mun_type == "Городской округ, городской округ с внутригородским делением") %>% 
  ggplot(aes(x=year, fill=model)) +
  geom_bar(position = 'stack') +
  ggtitle("Динамика формы управления по городским округам") +
  scale_fill_manual(values=c("#69b3a2", "#404080", "grey")) +
  theme_ipsum() +
  theme(
    plot.title = element_text(size=15),
    axis.text.x = element_text(angle = 90, vjust = 0.5)
  ) +
  scale_x_continuous("Год", labels = as.character(data_nona$year), breaks = data_nona$year) +
  labs(fill="")
p


p <- data_nona %>% filter(mun_type == "Муниципальный район") %>% 
  ggplot(aes(x=year, fill=model)) +
  geom_bar(position = 'stack') +
  ggtitle("Динамика формы управления по муниципальным районам") +
  scale_fill_manual(values=c("#69b3a2", "#404080", "grey")) +
  theme_ipsum() +
  theme(
    plot.title = element_text(size=15),
    axis.text.x = element_text(angle = 90, vjust = 0.5)
  ) +
  scale_x_continuous("Год", labels = as.character(data_nona$year), breaks = data_nona$year) +
  labs(fill="")
p



# совокупная динамика, 2008-2012
data_nona <- data %>% filter(year %in% 2008:2012) %>% drop_na()
p <- data_nona %>%
  ggplot(aes(x=year, fill=model)) +
  geom_histogram(binwidth=0.51, color="#e9ecef", alpha=0.6, position = 'identity') +
  ggtitle("Динамика формы управления") +
  scale_fill_manual(values=c("#69b3a2", "#404080", "grey")) +
  theme_ipsum() +
  theme(
    plot.title = element_text(size=15),
    axis.text.x = element_text(angle = 90, vjust = 0.5)
  ) +
  scale_x_continuous("Год", labels = as.character(data_nona$year), breaks = data_nona$year) +
  labs(fill="")
p

data_nona <- data %>% filter(year %in% 2008:2012) %>% drop_na()
p <- data_nona %>%
  ggplot(aes(x=year, fill=model)) +
  geom_bar(position = 'stack') +
  ggtitle("Динамика формы управления") +
  scale_fill_manual(values=c("#69b3a2", "#404080", "grey")) +
  theme_ipsum() +
  theme(
    plot.title = element_text(size=15),
    axis.text.x = element_text(angle = 90, vjust = 0.5)
  ) +
  scale_x_continuous("Год", labels = as.character(data_nona$year), breaks = data_nona$year) +
  labs(fill="")
p


#########################################################################
########################          БДМО           ########################
#########################################################################


# memory.limit(size=25000)

BDMO_variables <- read_csv("raw_data/BDMO_01012018/BDMO variables.csv")
BDMO <- read_csv("raw_data/BDMO_01012018/1 indicator clean v2.csv")
BDMO2 <- read_csv("raw_data/BDMO_01012018/2 indicator clean v2.csv")

BDMO_variables %>% filter(indicator_text == "Общий объем расходов муниципального бюджета")


# страница 43
y_names <- c("Общий объем расходов бюджета муниципального образования|||t8313001|||Всего")


y_ids <- (BDMO_variables %>% 
  filter(indicator_text == "Общий объем расходов бюджета муниципального образования|||t8313001|||Всего"))$indicator_id


treatment_data <- read_csv("raw_data/bumo_models_30122018/data.csv")

reg_data <- BDMO2 %>% select(c(y_ids, "oktmo", "year"))
reg_data$t8313001_1 %>% is.na() %>% sum()

dd <- reg_data %>% inner_join(treatment_data, by = c("oktmo", "year")) %>% arrange(municipality, year)


for (i in 2006:2018) {
  print(i)
  print(dd %>% filter(year == i) %>% is.na() %>% colSums())
}

dd %>% select(oktmo) %>% unique() %>% count()

# ну то есть в реальности для совокупных расходов 2008-2012