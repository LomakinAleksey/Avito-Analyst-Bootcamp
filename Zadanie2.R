install.packages("tidyverse")
library(ggplot2)
library(dplyr)

# Загружаем набор данных diamonds
data(diamonds)
?diamonds

str(diamonds)
summary(diamonds)

#Проверим наличие пропусков в данных
colSums(is.na(diamonds))

# Строим гистограммы распределения бриллиантов по цене и глубине
ggplot(diamonds, mapping = aes(x = price)) + geom_histogram(color = "green", fill="lightgreen")+ xlab("Цена") + ylab("Количество")
ggplot(diamonds, mapping = aes(x = depth)) + geom_histogram(color = "Yellow", fill="green", binwidth = 0.5)+ xlab("Глубина бриллиантов") + ylab("Количество")

# Строим столбчатую диаграмму средней цены бриллиантов в зависимости от качества огранки
ggplot(diamonds %>% group_by(cut) %>% summarise(avg_price = mean(price)))+ geom_col(mapping = aes(x = cut,y = avg_price,fill = cut))+ xlab("Качество огранки") + ylab("Средняя цена")

# Строим столбчатую диаграмму с подкатегориями 2 способами
ggplot(diamonds) + geom_bar(mapping = aes(x = cut, fill = clarity),position="fill")+ xlab("Качество огранки") + ylab("Удельный вес")
ggplot(diamonds) + geom_bar(mapping = aes(x = cut, fill = clarity),position="dodge")+ xlab("Качество огранки") + ylab("Количество")

# Строим диаграмму рессеяния
ggplot(head(diamonds, 1000), mapping = aes(x = cut, y = depth)) + geom_jitter(color = "cadetblue3",alpha = 0.8)+ xlab("Огранка бриллиантов") + ylab("Глубина бриллиантов")

# Строим boxplot (ящик с усами)
ggplot(diamonds,mapping = aes(x = cut,y = carat)) + geom_boxplot(color = "steelblue",fill="lightblue") + xlab("Огранка бриллиантов") + ylab("Вес (карат)")

# Средний вес по категориям огранки
diamonds %>%
  group_by(cut) %>%
  summarise(
    avg_price = mean(price),
    avg_carat = mean(carat),
    n = n()
  ) %>%
  arrange(desc(avg_price))

# Визуализируем компромисс
ggplot(diamonds, aes(x = carat, y = price, color = cut)) +
  geom_point(alpha = 0.1, size = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Компромисс: Вес против Качества огранки",
       subtitle = "Ideal-огранка преобладает у мелких камней")

# Сравним цены для камней ОДИНАКОВОГО веса (например, 1 карат)
diamonds %>%
  filter(carat >= 0.99, carat <= 1.01) %>%  # Бриллианты ~1 карат
  group_by(cut) %>%
  summarise(
    avg_price = mean(price),
    n = n()
  ) %>%
  arrange(desc(avg_price))

# Визуализируем взаимосвязь всех трех переменных
ggplot(diamonds, aes(x = carat, y = price, color = clarity)) +
  geom_point(alpha = 0.05) +
  facet_wrap(~ cut) +
  labs(title = "Взаимосвязь: Вес-Цена-Чистота по типам огранки")

# Числовой анализ
triple_analysis <- diamonds %>%
  group_by(cut) %>%
  summarise(
    avg_carat = mean(carat),
    avg_price = mean(price),
    # Доля высоких clarity (IF, VVS)
    prop_high_clarity = mean(clarity %in% c("IF", "VVS1", "VVS2")),
    # Доля низких clarity (I1, SI)
    prop_low_clarity = mean(clarity %in% c("I1", "SI1", "SI2")),
    n = n()
  ) %>%
  arrange(desc(avg_price))

triple_analysis

# Самый простой и наглядный способ
top_10_expensive <- diamonds %>%
  arrange(desc(price)) %>%    # Сортируем по убыванию цены
  head(10) %>%                # Берем первые 10 строк
  select(price, carat, cut, color, clarity, depth, table)  # Выбираем ключевые столбцы

# Выводим результат
print("10 САМЫХ ДОРОГИХ БРИЛЛИАНТОВ:")
print(top_10_expensive)

# Создадим график, показывающий топ-10 самых дорогих
ggplot(diamonds, aes(x = carat, y = price)) +
  geom_point(alpha = 0.1, color = "gray70") +  # Все бриллианты
  geom_point(data = top_10_expensive, 
             aes(color = cut, size = price),
             alpha = 0.8) +  # Топ-10
  scale_size_continuous(range = c(3, 8)) +
  labs(
    title = "10 самых дорогих бриллиантов в общем распределении",
    subtitle = "Размер точки = цена, цвет = тип огранки",
    x = "Вес (карат)",
    y = "Цена ($)"
  ) +
  theme_minimal()

# Давайте найдем эти "гиганты" 5 и 4 карата
giants <- diamonds %>%
  filter(carat >= 4) %>%
  arrange(desc(price)) %>%
  select(price, carat, cut, color, clarity, depth)

print("БРИЛЛИАНТЫ-ГИГАНТЫ (4+ карата):")
print(giants)