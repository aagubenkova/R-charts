install.packages("readr")
install.packages("ggplot2")
install.packages("dplyr")

library(readr)
library(ggplot2)

data <- read_csv("economy_data.csv.txt")

library(tidyverse)
library(ggplot2)

setwd("C:/Users/guben/OneDrive/Рабочий стол")
data <- read_csv("economy_data.csv.txt")

data_long <- data %>%
  select(year, inflation, key_rate) %>%
  pivot_longer(cols = c(inflation, key_rate),
               names_to = "indicator",
               values_to = "percent")


p1 <- ggplot(data_long, aes(x = year, y = percent, color = indicator, group = indicator)) +
  geom_line(size = 1.5) +
  geom_point(size = 3) +
  geom_hline(yintercept = 4, linetype = "dashed", color = "gray50", size = 0.8) +
  annotate("text", x = 2012, y = 4.5, label = "Цель ЦБ (4%)", size = 4, color = "gray50") +
  scale_color_manual(values = c("inflation" = "lightgreen", "key_rate" = "pink"),
                     labels = c("Инфляция (%)", "Ключевая ставка (%)")) +
  labs(
    title = "📈 Инфляция и ключевая ставка ЦБ РФ",
    subtitle = "2010-2024 годы | Данные Росстата и ЦБ РФ",
    x = "Год",
    y = "Проценты (%)",
    color = "Показатель",
    caption = "Красная линия - инфляция | Синяя линия - ключевая ставка\nПунктир - целевой уровень инфляции 4%"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray40"),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray90")
  ) +
  scale_x_continuous(breaks = seq(2010, 2024, by = 2)) +
  scale_y_continuous(breaks = seq(0, 25, by = 5),
                     labels = function(x) paste0(x, "%"))


print(p1)

ggsave("inflation_and_rate.png", width = 12, height = 7, dpi = 300)

data_scaled <- data %>%
  mutate(
    gdp_scaled = (gdp_growth + 10) / 20 * 8,  # приводим ВВП к шкале ~0-8%
    unemployment_scaled = unemployment
  )

p2 <- ggplot(data_scaled, aes(x = year)) +
  geom_col(aes(y = gdp_growth, fill = "ВВП"), alpha = 0.7, width = 0.6) +
  geom_line(aes(y = unemployment, color = "Безработица"), size = 1.5) +
  geom_point(aes(y = unemployment, color = "Безработица"), size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.5) +
  scale_fill_manual(values = c("ВВП" = "steelblue"), name = "") +
  scale_color_manual(values = c("Безработица" = "orange"), name = "") +
  labs(
    title = "Экономический рост и рынок труда",
    subtitle = "Темпы роста ВВП (%) и уровень безработицы (%)",
    x = "Год",
    y = "Проценты (%)",
    caption = "Синие столбцы - рост ВВП | Оранжевая линия - безработица"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray40"),
    axis.title = element_text(size = 12, face = "bold"),
    legend.position = "bottom",
    legend.text = element_text(size = 11),
    panel.grid.minor = element_blank()
  ) +
  scale_x_continuous(breaks = seq(2010, 2024, by = 2)) +
  scale_y_continuous(breaks = seq(-4, 12, by = 2),
                     labels = function(x) paste0(x, "%"))

print(p2)
ggsave("gdp_and_unemployment.png", width = 12, height = 7, dpi = 300)


