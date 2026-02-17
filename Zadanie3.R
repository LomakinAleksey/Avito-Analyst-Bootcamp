library(readxl)
library(dplyr)
library(tidyr)
# Определение количества пропущенных значений в каждом столбце
missing_values <- sapply(titanic, function(x) sum(is.na(x) | x == ""))
print("Количество пропущенных значений в каждом столбце:")
print(missing_values)

# Обработка пропусков: замена пропущенных значений в Age медианой
# Сначала найдем медиану возраста (игнорируя пропуски)
age_median <- median(titanic$age, na.rm = TRUE)

# Заменим пропуски в Age медианой
titanic$age <- ifelse(is.na(titanic$age), age_median, titanic$age)

# Изменение типа данных для переменных survived и pclass на факторы
titanic$survived <- as.factor(titanic$survived)
titanic$pclass <- as.factor(titanic$pclass)

# Создание новой переменной AgeGroup
titanic$AgeGroup <- cut(titanic$age,
                             breaks = c(0, 18, 60, Inf),
                             labels = c("0-18", "19-60", "60+"),
                             include.lowest = TRUE)

# Подсчет числа пассажиров в каждой возрастной группе
age_group_counts <- table(titanic$AgeGroup)
print("Число пассажиров в каждой возрастной группе:")
print(age_group_counts)

# Дополнительно: посмотрим на структуру данных после обработки
print("Структура данных после обработки:")
str(titanic)