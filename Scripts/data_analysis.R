library(tidyverse)
library(hrbrthemes)


data <- read_csv("data.csv")



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


p <- data_nona %>% filter(mun_type == "Муниципальный район") %>% 
  ggplot(aes(x=year, fill=model)) +
  geom_histogram(binwidth=0.51, color="#e9ecef", alpha=0.6, position = 'identity') +
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

